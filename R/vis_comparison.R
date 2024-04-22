#' Compare different runs.
#'
#' @param simexl The solved system of equations as returned by \code{run_model}.
#'
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#'
#' @param format One of "timeline" (shows a timeline stratified by compartment
#'   and vaccination status) or "endpoint" (shows the final value stratified by
#'   age category).
#'
#' @param log Logical indicating whether a log scale should be used.
#'
#' @param use_absolute_numbers Logical indicating whether absolute absolute
#'   numbers (in terms of individuals) should be shown or relative to population
#'   size.
#'
#' @param show_compartment The compartment to show when using \code{format = "endpoint"}.
#'
#' @param freescales Logical indicating whether the different comparments should
#'   use different y-axes.
#'
#' @param base_size Base size passed to theme_*
#'
#' @author Finlay Campbell
#'
#' @importFrom scales percent
#'
#' @export
#'
vis_comparison <- function(simexl,
                           what = c("prevalence", "deltas", "incidence"),
                           format = c("timeline", "endpoint"),
                           log = FALSE,
                           use_absolute_numbers = FALSE,
                           show_compartment = c("D", "S", "E", "C", "H", "R"),
                           freescales = TRUE,
                           base_size = 11) {

  ## catch NULLs for shiny app
  if(is.null(simexl)) return(NULL)

  ## match what argument
  what <- match.arg(what)
  format <- match.arg(format)
  show_compartment <- match.arg(show_compartment)

  ## category labels
  cat <- c(S = "Susceptible", E = "Exposed", C = "Community Infection",
           H = "Hospital Infection", R = "Recovered", D = "Dead")

  ## get population
  pop <- sum(simexl[[1]]$pars[[1]]$population)

  if(format == "timeline") {

    imap_dfr(simexl, ~ mutate(extract(.x, what), scenario = .y)) %>%
      mutate(scenario = fct_inorder(scenario)) %>%
      group_by(day, compartment, scenario) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      ggplot(aes(day, if(use_absolute_numbers) value*pop else value, color = scenario)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(
        ~ compartment,
        scales = ifelse(freescales, "free_y", "fixed"),
        labeller = labeller(compartment = cat)
      ) +
      scale_y_continuous(
        limits = if(!log) c(0, NA) else waiver(),
        expand = expansion(mult = c(0.01, 0.05)),
        trans = ifelse(log, "log10", "identity"),
        labels = if(use_absolute_numbers) waiver() else percent
      ) +
      scale_color_brewer(name = "Scenario", palette = "Dark2") +
      labs(x = "Day", y = str_to_title(what), color = "Category") +
      theme_minimal(base_size = base_size) +
      theme(
        legend.position = 'bottom',
        plot.background = element_rect(fill = "white")
      )

  } else {

    ## ## define age groupings
    ## groupings <- list(
    ##   "0-14" = paste0("age_", 1:3),
    ##   "15-34" = paste0("age_", 4:7),
    ##   "35-64" = paste0("age_", 8:13),
    ##   "65+" = paste0("age_", 14:16)
    ## )
    ## groupings <- unlist(unname(imap(groupings, ~ setNames(rep(.y, length(.x)), .x))))

    ## extract relevant data
    get_agestrat <- function(simex) {

      extract(simex, what, stratify_by = c("day", "compartment", "age")) %>%
        filter(day == max(day) & compartment == show_compartment) %>%
        arrange(age) %>%
        mutate(age_frac = simex$pars[[1]]$age_frac[age]) %>%
        ## age = fct_inorder(groupings[as.character(age)])
        group_by(age) %>%
        summarise(
          value = if(use_absolute_numbers) sum(value)*pop else sum(value)/sum(age_frac)
        )

    }

    imap_dfr(simexl, ~ mutate(get_agestrat(.x), scenario = .y)) %>%
      mutate(scenario = fct_inorder(scenario)) %>%
      ggplot(aes(age, value, fill = scenario)) +
      geom_col(position = "dodge") +
      scale_x_discrete(drop = FALSE, labels = get_age_cat()) +
      scale_y_continuous(
        expand = expansion(mult = c(0.01, 0.05)),
        labels = if(use_absolute_numbers) waiver() else function(x) percent(x, 0.001)
      ) +
      scale_fill_brewer(
        name = "Scenario",
        palette = "Dark2",
        drop = FALSE
      ) +
      labs(
        x = "Age category",
        y = paste0("Final ", what, " of individuals in compartment: ",
                   cat[show_compartment]),
        fill = "Scenario"
      ) +
      theme_minimal(base_size = base_size) +
      theme(
        plot.background = element_rect(fill = "white"),
        legend.position = 'bottom'
      )

  }

}
