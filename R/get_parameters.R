#' Define model parameters
#'
#' @param iso3 The ISO3 code of the country used to draw age-distributions and
#'   contact rates from.
#'
#' @param R0 The basic reproduction number.
#'
#' @param generation_time The mean generation time in days.
#'
#' @param incubation_period The mean incubation period in days.
#'
#' @param infectiousness_presymp Relative infectiousness of presymptomatic cases
#'   to symptomatic cases.
#'
#' @param ifr Infection fatality rate provided either as a single value or as a
#'   vector of the same length as the number of age categories. Use the function
#'   \code{age_to_ifr} to calculate a COVID-like IFR from a vector of ages.
#'
#' @param hosp_mortality The probability of death given a case is admitted to
#'   hospital, either as a single value or as a vector of the same length as the
#'   number of age categories. The inverse of the number of cases admitted to
#'   hospital per death.
#'
#' @param hosp_protection_death Given a case requires hospitalisation, the
#'   proportion of deaths admission to hospital averts.
#'
#' @param hosp_duration The mean duration of stay in the hospital in days,
#'   either as a single value or as a vector of the same length as the number of
#'   age categories.
#'
#' @param hosp_capacity Total hospital bed capacity given as a proportion of the
#'   population.
#'
#' @param comm_mortality The probability of death of cases that remain in the
#'   community, either as a single value or as a vector of the same length as the
#'   number of age categories.
#'
#' @param vax_rate The daily rate of vaccination as a proportion of the population.
#'
#' @param vax_infectiousness The reduction (as a proportion) in infectioussness
#'   of an individual due to vaccination.
#'
#' @param vax_infection The protection (as a proportion) against infection
#'   provided by vaccination.
#'
#' @param vax_hosp The protection (as a proportion) against hospitalisation
#'   provided by vaccination, given infection.
#'
#' @param vax_death The protection (as a proportion) against death provided by
#'   vaccination, given hospitalisation.
#'
#' @param isolation_adherence The proportion of symptomatic individuals that
#'   adhere to isolation measures.
#'
#' @param isolation_effectiveness The reduction in daily transmission potential
#'   of a given individual due to adherence to isolation measures.
#'
#' @param isolation_delay The mean delay from symptom onset to isolation in
#'   days.
#'
#' @param social_distancing A named vector of length 4 containing the proportion
#'   reduction in contacts due to social distancing of 'home', 'school', 'work'
#'   and 'other'.
#'
#' @param vax_prioritised A logical indicating whether older age groups are
#'   vaccinated first.
#'
#' @param hosp_prioritised A logical indicating whether older age groups are
#'   hospitalised first when hospital capacity is exceeded.
#'
#' @param frac_symp The proportion of cases that eventually develop symptoms.
#'
#' @importFrom stats uniroot
#'
#' @author Finlay Campbell, Prabasaj Paul
#'
#' @export
#'
get_parameters <- function(iso3 = "USA",
                           R0 = 2.5,
                           generation_time = 7,
                           incubation_period = 3,
                           infectiousness_presymp = 0.25,
                           frac_symp = 0.8,
                           ifr = age_to_ifr(get_age_median()),
                           hosp_mortality = 1/seq(20, 5, length = 16),
                           hosp_protection_death = 0.75,
                           hosp_duration = seq(7, 21, length = 16),
                           hosp_capacity = 0.0025,
                           comm_mortality = rep(0, 16),
                           vax_rate = 0,
                           vax_infectiousness = 0.3,
                           vax_infection = 0.5,
                           vax_hosp = 0.5,
                           vax_death = 0.8,
                           isolation_adherence = 0,
                           isolation_effectiveness = 0.8,
                           isolation_delay = 3,
                           social_distancing = c(home = 0, school = 0, work = 0, other = 0),
                           vax_prioritised = TRUE,
                           hosp_prioritised = TRUE
                           ) {

  ## form to list
  pars <- as.list(environment())

  ## get age-group populations and age fractions
  pars$population = cdat[[iso3]]$pop$count
  pars$age_frac = cdat[[iso3]]$pop$prop

  ## get polymod contact matrix
  pars$polymod <- cdat[[iso3]]$mod
  pars$polyscale <- cdat[[iso3]]$scale

  ## check specified generation time is possible
  if(generation_time < incubation_period/2)
    stop("Generation time not possible with incubation period provided.")

  ## check social distancing is provided properly
  if(!all(map_lgl(c("home", "school", "work", "other"),
                  ~ .x %in% names(social_distancing))))
    stop(paste(
      "social_distancing must contain contact reductions",
      "for 'home', 'school', 'work' and 'other'"))

  ## calculate proportion hospitalised from IFR and proportion in hospital that die
  pars$prop_hosp <- pars$ifr/pars$hosp_mortality

  ## calculate mortality if you would go to hospital but can't
  pars$unhosp_mortality <- hosp_mortality/(1-hosp_protection_death)

  ## solve for symptomatic period in community to satisfy generation time
  ## (incubation period and hospital period are fixed)
  pars$symptomatic_period <- uniroot(
    function(symptomatic_period) {
      pars$symptomatic_period <- symptomatic_period
      ## infection prob does not affect gen time so pick arbitrary one
      get_generation_time(0.01, add_rates(pars)) - generation_time
    },
    interval = c(0.01, generation_time*5)
  )$root

  ## add rates with correct symptomatic_period
  pars %<>% add_rates()

  ## calibrate p_trans from R0 or doubling time
  pars$p_trans <- uniroot(\(x) get_R0(x, pars) - R0, c(0, 1))$root
  pars$doubling_time <- get_doubling_time(pars$p_trans, pars)

  ## calculate betas from pathogen, population structure and interventions
  pars %<>% add_betas(mult = FALSE)

  return(pars)

}
