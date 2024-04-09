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

  week_ind <- (day-6):day
  pop <- sum(simex$pars$population)
  e_ind <- match(c("E_u", "E_v"), dimnames(simex$prevalence)$state)
  d_ind <- match(c("D_u", "D_v"), dimnames(simex$prevalence)$state)
  h_ind <- match(c("H_u", "H_v"), dimnames(simex$prevalence)$state)

  lst <- list(
    incidence_last_7 = sum(simex$incidence[week_ind,,e_ind])*pop,
    incidence_cumulative = sum(simex$incidence[1:day,,e_ind])*pop,
    incidence_weekly_change = percent(sum(simex$incidence[week_ind,,e_ind])/
                                      sum(simex$incidence[week_ind-7,,e_ind]) - 1),
    death_last_7 = sum(simex$incidence[week_ind,,d_ind])*pop,
    death_cumulative = sum(simex$incidence[1:day,,d_ind])*pop,
    death_weekly_change = percent(sum(simex$incidence[week_ind,,d_ind])/
                                  sum(simex$incidence[week_ind-7,,d_ind]) - 1),
    hosp_admission_last_7 = sum(simex$incidence[week_ind,,h_ind])*pop,
    hosp_admission_cumulative = sum(simex$incidence[1:day,,h_ind])*pop,
    hosp_admission_weekly_change = percent(sum(simex$incidence[week_ind,,h_ind])/
                                      sum(simex$incidence[week_ind-7,,h_ind]) - 1),
    hospital_occupancy = percent(sum(simex$prevalence[day,,h_ind])/
                                 simex$pars$hosp_capacity),
    proportion_cases_hospitalised_last_7 =
      percent(sum(simex$incidence[week_ind,,h_ind])/
              sum(simex$incidence[week_ind,,c(e_ind, h_ind)]), 0.1),
    ifr_last_7 = percent(sum(simex$incidence[week_ind,,d_ind])/
                         sum(simex$incidence[week_ind,,e_ind]), 0.001),
    ifr_cumulative = percent(sum(simex$incidence[1:day,,d_ind])/
                             sum(simex$incidence[1:day,,e_ind]), 0.001)
  ) %>%
    modify_if(is.numeric, round)

  tibble(statistic = names(lst), value = unlist(lst))

}
