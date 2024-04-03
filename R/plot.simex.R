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
#' @author Finlay Campbell
#'
#' @export
#'
plot.simex <- function(simex,
                       what = c("prevalence", "deltas", "incidence"),
                       log = FALSE,
                       freescales = TRUE) {

  extract(simex, what) %>%
    ggplot(aes(day, value, linetype = vax)) +
    geom_line(linewidth = 1.5) +
    facet_wrap(~ compartment, scales = ifelse(freescales, "free_y", "fixed")) +
    scale_y_continuous(
      expand = expansion(mult = c(0.01, 0.05)),
      trans = ifelse(log, "log10", "identity"),
      labels = percent
    ) +
    ## scale_color_brewer(palette = "Dark2") +
    scale_linetype(name = "Vaccinated") +
    labs(x = "Day", y = "Proportion of population", color = "Category") +
    theme_minimal() +
    theme(legend.position = 'bottom')

}
