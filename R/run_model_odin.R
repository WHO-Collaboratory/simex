#' Run the model using the odin2/dust2 backend (single parameter set only).
#'
#' Same interface as \code{run_model}: one call per run, no chaining of
#' parameter sets. Time-varying parameters are to be added later inside the
#' odin model.
#'
#' Requires \code{odin2::odin_package(pkgroot)} to have been run so that the
#' dust system is generated (e.g. \code{inst/dust}, \code{R/dust.R}).
#'
#' @param pars A single parameter set as returned by \code{get_parameters}.
#' @param init_state Optional initial state matrix (age x compartment); same
#'   format as for \code{run_model}.
#' @param max_day Last day to simulate.
#' @return An object of class \code{simex} with \code{prevalence}, \code{deltas},
#'   \code{incidence} (time x age x compartment), and \code{pars} (list with
#'   one element).
#'
#' @importFrom magrittr divide_by
#' @importFrom dust2 dust_system_create dust_system_set_state_initial
#'   dust_system_simulate
#' @author Finlay Campbell, Prabasaj Paul
#' @export
run_model_odin <- function(pars,
                           max_day = 200,
                           n_particles = 1) {


  if(length(pars) == length(get_parameters())) pars <- list("1" = pars)

  ## if only a single parameter set is provided
  if (length(pars) == 1) {
    # define system and simulate
    times <- 1:max_day
    sys <- dust2::dust_system_create(simex:::simex, pars[[1]], n_particles = n_particles)
    dust2::dust_system_set_state_initial(sys)
    traj <- dust2::dust_system_simulate(sys, times)
    traj <- dust2::dust_unpack_state(sys, traj)

  } else {

    times <- c(as.numeric(names(pars)), max_day)
    sys <- dust2::dust_system_create(simex:::simex, pars[[1]], n_particles = n_particles)
    dust2::dust_system_set_state_initial(sys)
    traj <- dust2::dust_system_simulate(sys, seq(1, times[2]))
    for (i in 2:length(pars)) {
      dust2::dust_system_update_pars(sys, pars[[i]])
      after <- dust2::dust_system_simulate(sys, seq(times[i] + 1, times[i+1]))
      traj <- abind(traj, after, along = ifelse(n_particles == 1, 2, 3))
    }
    traj <- dust2::dust_unpack_state(sys, traj)
    times <- seq(1:max_day)
  }

  # is incidence vs prevalence
  is_i <- grepl("_", names(traj))

  # is vaccinated or not
  is_v <- substr(names(traj), 2, 2) == "v"

  # define dimensions
  dn <- list(
    what = c("prevalence", "incidence"),
    compartment = unique(substr(names(traj), 1, 1)),
    vax = c("vax", "unvax"),
    age = pars[[1]]$age_groups,
    day = times,
    particle = seq_len(n_particles)
  )

  # return as array
  if (n_particles == 1) {
    out <- simplify2array(traj)
    out <- abind(
      abind(out[, , !is_i & is_v], out[, , is_i & is_v], along = 0),
      abind(out[, , !is_i & !is_v], out[, , is_i & !is_v], along = 0),
      along = 0
    ) |>
      aperm(c(2, 5, 1, 3, 4)) |>
      setDimnames(dn[-6])
  } else {
    out <- simplify2array(traj)
    # split prevalence vs incidence and vax vs unvax
    out <- abind(
      abind(out[, , , !is_i & is_v], out[, , , is_i & is_v], along = 0),
      abind(out[, , , !is_i & !is_v], out[, , , is_i & !is_v], along = 0),
      along = 0
    ) |>
      aperm(c(2, 6, 1, 3, 5, 4)) |>
      setDimnames(dn)
  }

  ## add parameter states
  attr(out, "pars") <- pars

  ## define as simex object class
  class(out) <- "simex"

  return(out)

}
