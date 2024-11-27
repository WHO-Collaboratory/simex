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
#' @param show_compartment The compartment to show when using \code{format =
#'   "endpoint"}.
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
                           type = c("ggplot", "highchart"),
                           log = FALSE,
                           use_absolute_numbers = FALSE,
                           show_compartment = c("D", "S", "E", "C", "H", "R"),
                           freescales = TRUE,
                           base_size = 11) {

  ## catch NULLs for shiny app
  if (is.null(simexl)) return(NULL)

  ## match arguments
  type <- match.arg(type)
  what <- match.arg(what)
  format <- match.arg(format)
  show_compartment <- match.arg(show_compartment)

  ## category labels
  lbs <- c(S = "Susceptible", E = "Exposed", C = "Community Infection",
           H = "Hospital Infection", R = "Recovered", D = "Dead")

  ## get population
  pop <- sum(simexl[[1]]$pars[[1]]$population)

  if (format == "timeline") {

    imap_dfr(simexl, ~ mutate(extract(.x, what), scenario = .y)) %>%
      mutate(scenario = fct_inorder(scenario)) %>%
      group_by(day, compartment, scenario) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      ggplot(
        aes(
          x = day,
          y = if (use_absolute_numbers) value * pop else value,
          color = scenario
        )
      ) +
      geom_line(linewidth = 1.5) +
      facet_wrap(
        ~ compartment,
        scales = ifelse(freescales, "free_y", "fixed"),
        labeller = labeller(compartment = lbs)
      ) +
      scale_y_continuous(
        limits = if (!log) c(0, NA) else waiver(),
        expand = expansion(mult = c(0.01, 0.05)),
        trans = ifelse(log, "log10", "identity"),
        labels = if (use_absolute_numbers) waiver() else percent
      ) +
      scale_color_brewer(name = "Scenario", palette = "Dark2") +
      labs(x = "Day", y = str_to_title(what), color = "Category") +
      theme_minimal(base_size = base_size) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "white")
      )

  } else {

    ## extract relevant data
    get_agestrat <- function(simex) {

      extract(
        simex, "incidence", stratify_by = c("day", "compartment", "age")
      ) %>%
        filter(compartment == show_compartment) %>%
        group_by(age) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        mutate(
          age_frac = simex$pars[[1]]$age_frac[age],
          value = if (use_absolute_numbers) value * pop else value / age_frac
        )

    }

    df <- imap_dfr(simexl, ~ mutate(get_agestrat(.x), scenario = .y)) %>%
      mutate(scenario = fct_inorder(scenario))

    if (type == "highchart") {

      df <- df %>%
        mutate(
          value = value * 100,
          age = get_age_cat()[as.numeric(age)]
        )

      ## Create a bar chart
      highchart() %>%
        hc_chart(
          type = "column",
          backgroundColor = "#FFFFFF"
        ) %>%
        hc_add_series(
          data = df, "column", hcaes(x = age, y = value, group = scenario)
        ) %>%
        hc_yAxis(
          title = list(text = "Proportion of population"),
          labels = list(format = "{value}%"),
          min = 0
        ) %>%
        hc_xAxis(
          title = list(text = "Age"),
          categories = unname(get_age_cat())
        ) %>%
        hc_tooltip(valueDecimals = 2, valueSuffix = "%")

    } else {

      df %>%
        ggplot(aes(age, value, fill = scenario)) +
        geom_col(position = "dodge") +
        scale_x_discrete(drop = FALSE, labels = get_age_cat()) +
        scale_y_continuous(
          expand = expansion(mult = c(0.01, 0.05)),
          labels =
            if (use_absolute_numbers) waiver()
            else function(x) percent(x, 0.001)
        ) +
        scale_fill_brewer(
          name = "Scenario",
          palette = "Dark2",
          drop = FALSE
        ) +
        labs(
          x = "Age category",
          y = paste0("Final ", what, " of individuals in compartment: ",
                     lbs[show_compartment]),
          fill = "Scenario"
        ) +
        theme_minimal(base_size = base_size) +
        theme(
          plot.background = element_rect(fill = "white"),
          legend.position = "bottom"
        )

    }

  }

}
