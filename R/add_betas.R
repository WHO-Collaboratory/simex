#' Calculate force of infection under different contact rates, isolation
#' measures and social distancing scenarios.
#'
#' @param pars A parameter object as returned by \code{get_parameters}.
#'
#' @param mult A logical indicating whether social distancing and isolation
#'   measures have a combined effect.
#'
add_betas <- function(pars, mult = FALSE) {

  ## names of places that contacts are split into
  places <- c("home", "school", "work", "other")

  ## proportion of baseline contact rate of non-isolated cases under a given
  ## social-distancing scenario
  non_isol_coeffs <- 1 - unlist(pars$social_distancing[places])

  ## proportion of baseline contact rate when community cases isolate
  isol_coeffs <- rep(1 - pars$isolation_effectiveness, 4)

  ## proportion of contact rate maintained by cases in the hospital:
  ## home/school/work all assumed to disappear, and other to be reduced by the
  ## isolation effectiveness
  hosp_coeffs <- c(0, 0, 0, 1 - pars$isolation_effectiveness)

  ## do social distancing and isolation have a combined effect?
  if (mult) isol_coeffs <- non_isol_coeffs * isol_coeffs
  else isol_coeffs <- pmin(non_isol_coeffs, isol_coeffs)

  ## the proportion of symptomatic infectious-days in the community that are
  ## isolated is given by the product of the proportion that eventually develop
  ## symptoms, the proportion that adhere to isolation and the delay from
  ## symptom onset to isolation
  frac_isol <- pars$frac_symp *
    pars$isolation_adherence *
    max(c(0, (pars$symptomatic_period - pars$isolation_delay)/pars$symptomatic_period))

  ## Multiply non-isolation coefficients (i.e. proportion of contacts still
  ## occuring after a given intervention while *not* isolated) by baseline
  ## contact rates and sum to get total contacts in non-isolated state during a
  ## given intervention
  polyscale_non_isol <- map2(non_isol_coeffs, pars$polyscale[places], \(x, y) x*y) %>%
    Reduce("+", .)

  ## Multiply isolation coefficients (i.e. proportion of contacts still occuring
  ## once isolated) by baseline contact rates and sum to get total contacts in
  ## isolated state
  polyscale_isol <- map2(isol_coeffs, pars$polyscale[places], \(x, y) x*y) %>%
    Reduce("+", .)

  ## get total contact rates across different types
  polyscale_hosp <- map2(hosp_coeffs, pars$polyscale[places], \(x, y) x*y) %>%
    Reduce("+", .)

  ## beta for Exposed cases takes:
  ## - contact rates unaffected by isolated but affected by social distancing
  ## - estimated transmission probability per contact
  ## - relative infectiousness of Exposed relative to Infected
  pars$beta_E = pars$infectiousness_presymp * pars$p_trans * polyscale_non_isol

  ## beta for Infected cases in the community takes:
  ## - weighted mean of contact rates from isolated and non-isolated Infecteds
  ## - estimated transmission probability per contact
  pars$beta_I_c = pars$p_trans * (frac_isol * polyscale_isol + (1 - frac_isol) * polyscale_non_isol)

  ## beta for Infected cases in the hospital takes:
  ## - estimated transmission probability per contact
  ## - polyscale that removes home/work/school
  pars$beta_I_h = pars$p_trans * polyscale_hosp

  return(pars)

}
