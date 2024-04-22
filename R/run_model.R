#' Run the model on on a single set of parameters, or a list of parameters.
#'
#' @param pars A parameter set as returned by \code{get_parameters}, or a named
#'   list of parameters where the name indicates the start date of the given
#'   parameter set.
#'
#' @param max_day
#'
#' @importFrom stats uniroot
#' @importFrom abind abind
#'
#' @author Finlay Campbell, Prabasaj Paul
#'
#' @export
#'
run_model <- function(pars,
                      init_state = NULL,
                      max_day = 365
                      ) {

  ## check if multiple parameter sets used or not
  if(length(pars) == length(get_parameters())) pars <- list("1" = pars)
  age_frac <- pars[[1]]$age_frac
  population <- pars[[1]]$population

  ## compartment names
  nms <- c("S_u", "E_u", "C_u", "H_u", "R_u", "D_u",
           "S_v", "E_v", "C_v", "H_v", "R_v", "D_v")

  ## Naive state: all susceptible
  state_naive <- matrix(
    c(age_frac, rep(age_frac*0, length(nms) - 1)),
    ncol = length(nms),
    dimnames = list(NULL, nms)
  ) %>%
    divide_by(sum(.)) %>%
    "row.names<-"(paste0("age_", seq_len(nrow(.))))

  ## Initial state may be supplied through pars
  if (is.null(init_state)) {
    ## a single infection evenly distribution across age groups
    init_state <- state_naive
    init_state[, "C_u"] <- age_frac/population
    init_state[, "S_u"] <- init_state[, "S_u"] - init_state[, "C_u"]
  } else {
    if(any(dim(init_state) != dim(state_naive)))
      stop(paste("init_state must have dimensions:",
                 paste0(dim(state_naive), collapse = ", ")))
    init_state %<>% divide_by(sum(.))
    colnames(init_state) <- nms
    rownames(init_state) <- paste0("age_", seq_len(nrow(init_state)))
  }

  ## if only a single parameter set is provided
  if(length(pars) == 1) {
    out <- solve_ode(pars[[1]], days = seq(1, max_day), state = init_state)
  } else {
    times <- c(as.numeric(names(pars)), max_day)
    ## solve first run from init state
    out <- solve_ode(pars[[1]], days = seq(1, times[2]), state = init_state)
    ## store dimension names
    dm_names <- names(dimnames(out$prevalence))
    final_state <- out$prevalence[dim(out$prevalence)[1],,]
    ## iterate across parameter set
    for(i in 2:length(pars)) {
      ## solve using endpoint of previous run
      after <- solve_ode(pars[[i]], days = seq(times[i], times[i+1]), state = final_state)
      ## update endpoint
      final_state <- after$prevalence[dim(after$prevalence)[1],,]
      ## update output by binding arrays along time axis, removed duplicated starting point
      for(j in seq_along(out)) {
        out[[j]] <- abind(out[[j]], after[[j]][-1,,], along = 1)
        names(dimnames(out[[j]])) <- dm_names
      }
    }
  }

  ## add parameter states
  out$pars <- pars

  ## define as simex object class
  class(out) <- "simex"

  return(out)

}
