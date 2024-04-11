#' Plot timeline of prevalence or incidence.
#'
#' @param simex The solved system of equations as returned by \code{run_model}.
#'
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#'
#' @param log Logical indicating whether a log scale should be used.
#'
#' @param freescales Logical indicating whether the different comparments should
#'   use different y-axes.
#'
#' @param show_hosp_capacity Logical indicating whether hospital capacity should be
#'   displayed in the "H" compartment (only possible when what = "prevalence").
#'
#' @param use_absolute_numbers Logical indicating whether absolute absolute
#'   numbers (in terms of individuals) should be shown or relative to population
#'   size.
#'
#' @param base_size Base size passed to theme_*
#'
#' @author Finlay Campbell
#'
#' @export
#'
plot.simex <- function(simex,
                       what = c("prevalence", "deltas", "incidence"),
                       log = FALSE,
                       freescales = TRUE,
                       show_hosp_capacity = TRUE,
                       use_absolute_numbers = FALSE,
                       base_size = 11) {

  ## check what argument
  what <- match.arg(what)

  ## get population
  pop <- sum(simex$pars[[1]]$population)

  ## get hospital capacity
  hosp_capacity <- simex$pars[[1]]$hosp_capacity
  if(use_absolute_numbers) hosp_capacity <- hosp_capacity * pop

  ## extract relevant data
  df <- extract(simex, what)

  ## category labels
  cat <- c(S = "Susceptible", E = "Exposed", C = "Community Infection",
           H = "Hospital Infection", R = "Recovered", D = "Dead")

  ## define horizontal line for hospital capacity if needed
  hline <-
    if(what == "prevalence" & show_hosp_capacity)
      geom_hline(
        data = tibble(
          compartment = factor("H", levels(df$compartment)),
          y = hosp_capacity
        ),
        aes(yintercept = y),
        color = "grey",
        linewidth = 1.5
      )
    else NULL

  ## plot
  df %>%
    ggplot(aes(day, if(use_absolute_numbers) value*pop else value, linetype = vax)) +
    hline +
    geom_line(linewidth = 1.5) +
    facet_wrap(
      ~ compartment,
      scales = ifelse(freescales, "free_y", "fixed"),
      labeller = labeller(compartment = cat)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.01, 0.05)),
      trans = ifelse(log, "log10", "identity"),
      labels = if(use_absolute_numbers) waiver() else percent
    ) +
    scale_linetype(name = "Vaccinated") +
    labs(
      x = "Day",
      y = if(use_absolute_numbers) "Number of individuals"
          else "Proportion of population",
      color = "Category"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = 'bottom',
      plot.background = element_rect(fill = "white")
    )

}
