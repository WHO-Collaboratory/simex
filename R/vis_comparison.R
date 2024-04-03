#' Compare different runs.
#'
#' @param simexl The solved system of equations as returned by \code{run_model}.
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
#' @importFrom scales percent
#'
#' @export
#'
vis_comparison <- function(simexl,
                           what = c("prevalence", "deltas", "incidence"),
                           log = FALSE,
                           freescales = TRUE) {

  what <- match.arg(what)

  imap_dfr(simexl, ~ mutate(extract(.x, what), scenario = .y)) %>%
    mutate(scenario = fct_inorder(scenario)) %>%
    group_by(day, compartment, scenario) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    ggplot(aes(day, value, color = scenario)) +
    geom_line(linewidth = 1.5) +
    facet_wrap(~ compartment, scales = ifelse(freescales, "free_y", "fixed")) +
    scale_y_continuous(
      limits = if(!log) c(0, NA) else waiver(),
      expand = expansion(mult = c(0.01, 0.05)),
      trans = ifelse(log, "log10", "identity"),
      labels = percent
    ) +
    scale_color_brewer(name = "Scenario", palette = "Dark2") +
    labs(x = "Day", y = "Proportion of population", color = "Category") +
    theme_minimal() +
    theme(
      legend.position = 'bottom',
      plot.background = element_rect(fill = "white")
    )

}
