#' Calculate transition rates from periods.
#'
#' @param pars A parameter object as returned by \code{get_parameters}.
#'
#' @param state An optional state of compartments, used to adjust rates for
#'   hospital occupancy.
#'
add_rates <- function(pars, state = NULL) {

  if(is.null(state)) hosp_required <- rep(0, length(pars$age_frac))
  else hosp_required <- state[, "H_u"] + state[, "H_v"]

  if(sum(hosp_required) > pars$hosp_capacity) {

    # Define number that are actually hospitalised
    hospitalised <- hosp_required
    hospitalised[] <- 0
    hosp_left <- pars$hosp_capacity
    # Fill in hospitalisations from oldest age group
    if(pars$hosp_prioritised) {
      for(i in rev(seq_along(hospitalised))) {
        if(hosp_left > hosp_required[i]) {
          hospitalised[i] <- hosp_required[i]
          hosp_left <- hosp_left - hospitalised[i]
        } else {
          hospitalised[i] <- hosp_left
          hosp_left <- 0
        }
      }
    } else {
      # vaccinate a given proportion of all age groups equally
      hospitalised <- pars$hosp_capacity*hosp_required/sum(hosp_required)
    }

    # total mortality of those that belong in hospital is weighted mean of
    # proportion hospitalised and unhospitalised
    pars$total_hosp_mortality <- pars$hosp_mortality*hospitalised/hosp_required +
      pars$unhosp_mortality*(hosp_required - hospitalised)/hosp_required
    # avoid dividing by zero when no hospitals required
    pars$total_hosp_mortality[hosp_required == 0] <- pars$hosp_mortality[hosp_required == 0]

  } else pars$total_hosp_mortality <- pars$hosp_mortality

  pars <- within(pars, {

    #  E -> C_u (infectious, community, unvax)
    lambda_cu <- (1-prop_hosp)/incubation_period
    #  E -> H_u (infectious, hospital, unvax)
    lambda_hu <- prop_hosp/incubation_period
    #  E -> H_u (infectious, community, vax)
    lambda_cv <- (1-(1-vax_hosp)*prop_hosp)/incubation_period
    #  E -> H_u (infectious, hospital, vax)
    lambda_hv <- (1-vax_hosp)*prop_hosp/incubation_period

    # define rates from C_u -> R and D
    sigma_cu <- (1-comm_mortality)/symptomatic_period
    mu_cu <- comm_mortality/symptomatic_period

    # define rates from C_v -> R and D
    sigma_cv <- (1-(1-vax_death)*comm_mortality)/symptomatic_period
    mu_cv <- (1-vax_death)*comm_mortality/symptomatic_period

    # define rates from H_u -> R and D
    sigma_hu <- (1-total_hosp_mortality)/hosp_duration
    mu_hu <- total_hosp_mortality/hosp_duration

    # define rates from H_v -> R and D
    sigma_hv <- (1-(1-vax_death)*total_hosp_mortality)/hosp_duration
    mu_hv <- (1-vax_death)*total_hosp_mortality/hosp_duration

  })

  return(pars)

}

