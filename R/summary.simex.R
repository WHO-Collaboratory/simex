#' Generate summary statistics for a simex object.
#'
#' @param simex The solved system of equations as returned by \code{run_model}.
#'
#' @param day What day of the outbreak to summarise to
#'
#' @author Finlay Campbell
#'
#' @export
#'
summary.simex <- function(simex, day = dim(simex$prevalence)[1]) {

  ## get population and hosp capacity
  pop <- sum(simex$pars[[1]]$population)
  hosp_capacity <- simex$pars[[1]]$hosp_capacity

  ## define week indices
  week_ind <- max(c(1, (day-6))):day
  prev_week_ind <- max(c(1, (day-13))):max(c(1, (day-7)))

  ## define compartment indices
  e_ind <- match(c("E_u", "E_v"), dimnames(simex$prevalence)$state)
  d_ind <- match(c("D_u", "D_v"), dimnames(simex$prevalence)$state)
  h_ind <- match(c("H_u", "H_v"), dimnames(simex$prevalence)$state)

  daymean <- function(x) mean(apply(x, 1, sum))
  daysum <- function(x) sum(apply(x, 1, sum))

  lst <- list(
    incidence_last_7 = daymean(simex$incidence[week_ind,,e_ind])*pop,
    incidence_cumulative = daysum(simex$incidence[1:day,,e_ind])*pop,
    incidence_weekly_change = percent(daymean(simex$incidence[week_ind,,e_ind])/
                                      daymean(simex$incidence[prev_week_ind,,e_ind]) - 1),
    death_last_7 = daymean(simex$incidence[week_ind,,d_ind])*pop,
    death_cumulative = daysum(simex$incidence[1:day,,d_ind])*pop,
    death_weekly_change = percent(daymean(simex$incidence[week_ind,,d_ind])/
                                  daymean(simex$incidence[prev_week_ind,,d_ind]) - 1),
    hosp_admission_last_7 = daymean(simex$incidence[week_ind,,h_ind])*pop,
    hosp_admission_cumulative = daysum(simex$incidence[1:day,,h_ind])*pop,
    hosp_admission_weekly_change = percent(daymean(simex$incidence[week_ind,,h_ind])/
                                           daymean(simex$incidence[prev_week_ind,,h_ind]) - 1),
    hospital_occupancy = percent(daysum(simex$prevalence[day,,h_ind])/
                                 hosp_capacity, 0.1),
    proportion_cases_hospitalised_last_7 =
      percent(daysum(simex$incidence[week_ind,,h_ind])/
              daysum(simex$incidence[week_ind,,c(e_ind, h_ind)]), 0.1),
    ifr_last_7 = percent(daysum(simex$incidence[week_ind,,d_ind])/
                         daysum(simex$incidence[week_ind,,e_ind]), 0.001),
    ifr_cumulative = percent(daysum(simex$incidence[1:day,,d_ind])/
                             daysum(simex$incidence[1:day,,e_ind]), 0.001)
  ) %>%
    modify_if(is.numeric, round)

  tibble(statistic = names(lst), value = unlist(lst))

}
