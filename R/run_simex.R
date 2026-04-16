#' Run the model using the odin2/dust2 backend
#'
#' Runs one or more parameter regimes over time. For multiple parameter columns
#' (change points), the dust system is updated and simulated in segments.
#'
#' Requires \code{odin2::odin_package(pkgroot)} to have been run so that the
#' dust system is generated (e.g. \code{inst/dust}, \code{R/dust.R}).
#'
#' @param pars A parameter set from \code{\link{get_parameters}}, or a
#'   matrix/list layout as described in the package README.
#' @param state Optional initial state passed to the dust system when not using
#'   the model default.
#' @param time Integer vector of times to simulate (inclusive range).
#' @param n_particles Number of particles (stochastic replicates) per group.
#' @return An object of class \code{simex} with \code{prevalence}, \code{deltas},
#'   \code{incidence} (time x age x compartment), and \code{pars} (list with
#'   one element).
#'
#' @importFrom magrittr divide_by
#' @importFrom abind abind
#' @importFrom dust2 dust_system_create dust_system_set_state_initial
#'   dust_system_simulate
#' @author Finlay Campbell, Prabasaj Paul
#' @export
run_simex <- function(pars,
                      state = NULL,
                      time = 0:200,
                      n_particles = 1) {

  # convert into parameter matrix where rows represent parallel
  # parameter sets (e.g. posterior draws) and columns represent
  # parameter sets applied to the same system over time. when a list
  # is provided, assume the latter.
  if (is.null(dim(pars)) && length(pars) == length(get_parameters())) {
    pars <- matrix(list(pars), dimnames = list(NULL, min(time)))
  } else if (is.null(dim(pars))) {
    if (is.null(names(pars))) stop("breaks must be provided as list names to pars")
    pars <- matrix(pars, dimnames = list(NULL, names(pars)), nrow = 1)
  } else if (is.matrix(pars)) {
    if (is.null(dimnames(pars)[[2]])) {
      # if 1 column provided, assume starting time is 1
      if (dim(pars)[2] == 1) dimnames(pars)[[2]] <- "1"
      else stop("breaks must be provided as column names to pars")
    }
  }

  breaks <- c(as.numeric(dimnames(pars)[[2]]), max(time))
  time <- seq(min(breaks), max(breaks))
  sys <- dust2::dust_system_create(
    generator = simex:::simex,
    pars = if (nrow(pars) == 1) pars[[1]] else pars[, 1],
    n_groups = nrow(pars),
    n_particles = n_particles,
    time = min(time)
  )
  if (!is.null(state)) {
    dust2::dust_system_set_state(sys, state)
  } else {
    dust2::dust_system_set_state_initial(sys)
  }
  sim <- dust2::dust_system_simulate(sys, seq(breaks[1], breaks[2]))
  if (ncol(pars) > 1) {
    for (i in 2:ncol(pars)) {
      dust2::dust_system_update_pars(
        sys = sys,
        pars = if (nrow(pars) == 1) pars[[i]] else pars[, i]
      )
      after <- dust2::dust_system_simulate(
        sys, seq(breaks[i] + 1, breaks[i + 1])
      )
      # identify dimension to bind along
      along <- match(setdiff(dim(sim), dim(after)), dim(sim))
      sim <- abind(sim, after, along = along)
    }
  }

  dims <- c("state", "particle", "sample", "time")
  if (n_particles == 1) dims <- setdiff(dims, "particle")
  if (nrow(pars) == 1) dims <- setdiff(dims, "sample")
  sample <- if (is.null(dimnames(pars)[[1]])) seq_len(dim(pars)[1]) else dimnames(pars)[[1]]

  # generate index
  index <- dust2::dust_unpack_index(sys)

  # attach unpack index and age labels for as.simex()
  attr(sim, "index") <- index
  attr(sim, "age_groups") <- pars[[1L]]$age_groups

  # convert to simex and attach parameter matrix for downstream plotters
  sx <- as.simex(sim, dims = dims, time = time, sample = sample)
  sx$pars <- pars
  return(sx)

}

#' Coerce dust2 output or arrays into a \code{simex} object
#'
#' Reshapes simulation output using dimension names in \code{dims}. Optional
#' named arguments in \code{...} supply index levels for dimensions (e.g.
#' \code{time = 1:200}).
#'
#' @param x Numeric array with \code{attr(x, "index")} from the dust system.
#' @param dims Character vector naming dimensions of \code{x} (length must match
#'   \code{length(dim(x))}).
#' @param ... Optional dimension level vectors, named to match entries in
#'   \code{dims}.
#'
#' @export
as.simex <- function(x, dims = c("state", "time"), ...) {

  # get index
  index <- attr(x, "index")

  # collect dimension values if provided
  args <- list(...)

  # index breakdown for model states
  args$state <- list(
    age = attr(x, "age_groups"),
    compartment = unique(substr(names(attr(x, "index")), 1, 1)),
    vax = c(FALSE, TRUE)
  )

  # check dimensions
  stopifnot(length(dims) == length(dim(x)))

  # is incidence vs prevalence
  is_i <- grepl("_", names(attr(x, "index")))

  # get values for dimensions
  get_dimval <- function(name, ln, args) {
    if (name %in% names(args)) args[[name]]
    else seq_len(ln)
  }

  # run across dimensions
  dn <- map2(set_names(dims), dim(x), ~ get_dimval(.x, .y, args)) |>
    list_flatten(name_spec = "{inner}")

  # construct DT
  out <- list(
    incidence = as.data.table(do.call(expand.grid, dn)),
    prevalence = as.data.table(do.call(expand.grid, dn))
  )

  # assign incidence
  data.table::set(
    out$incidence,
    j = "value",
    value = as.vector(
      slice_dim(x, unlist(index[is_i]), match("state", dims))
    )
  )

  # assign incidence
  data.table::set(
    out$prevalence,
    j = "value",
    value = as.vector(
      slice_dim(x, unlist(index[!is_i]), match("state", dims))
    )
  )

  # define as simex object class
  class(out) <- "simex"

  # return
  return(out)

}

#' Simulate forward from posterior samples
#'
#' Unpacks parameters from a \code{monty} samples object (with \code{packer}
#' attribute) and calls [run_simex()].
#'
#' @param samples Object returned by [monty::monty_sample()] (must carry
#'   \code{attr(samples, "packer")}).
#' @param time Time indices to simulate, passed to [run_simex()].
#' @param modification Optional list of parameter overrides applied after
#'   unpacking (structure depends on \code{packer$groups}).
#' @param start_from_snapshot If not \code{NULL}, index of a saved snapshot
#'   used as the initial state instead of the default.
#'
#' @export
run_simex_from_samples <- function(samples,
                                   time = 0:200,
                                   modification = NULL,
                                   start_from_snapshot = NULL
                                   ) {

  packer <- attr(samples, "packer")

  if (is.null(packer$groups)) {

    # generate full parameter sets (i.e. fitted + fixed pars) from
    # posterior samples of inferred parameters using the unpacker
    pars <- collapse_dim(samples$pars, keep = 1) |>
      apply(2, packer$unpack) |>
      map(~ list_modify(.x, !!!modification)) |>
      matrix(ncol = 1)

    if (!is.null(start_from_snapshot))
      state <- collapse_dim(
        samples$observations$snapshots[, start_from_snapshot, , ],
        keep = 1
      )
    else
      state <- NULL

    run_simex(pars, state, time)

  } else {

    # collapse chain dimesion
    pars <- collapse_dim(samples$pars, keep = 1) |>
      # unpack to generate full parameter set
      apply(2, packer$unpack) |>
      # apply modifications
      map(
        ~ map2(.x, modification, \(par, mod) list_modify(par, !!!mod))
      )

    # collapse group dimension to map that of states
    pars <- map(packer$groups(), ~ map(pars, pluck, .x))
    pars <- do.call(c, pars)
    pars <- matrix(pars, ncol = 1)

    if (!is.null(start_from_snapshot))
      state <- collapse_dim(
        samples$observations$snapshots[, , start_from_snapshot, , ],
        keep = 1
      )
    else
      state <- NULL

    run_simex(pars, state, time)

  }

}


# slice into a given dimension without knowing number of dimensions
slice_dim <- function(x, idx, dim) {
  k <- length(dim(x))
  indices <- rep(list(quote(expr = )), k)  # select all dimensions
  indices[[dim]] <- idx                    # replace the target dimension
  do.call("[", c(list(x), indices, list(drop = FALSE)))
}


# collapse dimensions by specifying the ones to keep
collapse_dim <- function(x, keep = 1) {
  array(
    x,
    c(dim(x)[keep], prod(dim(x)[-keep])),
    dimnames = c(dimnames(x)[keep], list(NULL))
  )
}
