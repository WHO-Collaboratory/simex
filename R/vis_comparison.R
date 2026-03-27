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
#' @param show_ribbon If `TRUE` (endpoint plots only), show credible intervals
#'   when multiple particles exist (same `cri_alpha` as timeline / `plot.simex`).
#' @param cri_alpha Width of the central interval when `show_ribbon` is `TRUE`.
#' @param cri_display_alpha Line alpha for `geom_errorbar` only (ggplot
#'   endpoint); Highcharts error bars are solid black above columns.
#'
#' @importFrom forcats fct_inorder
#'
#' @author Finlay Campbell
#'
#' @export
#'
vis_comparison <- function(simexl,
                           what = c("prevalence", "incidence"),
                           format = c("timeline", "endpoint"),
                           type = c("ggplot", "highchart"),
                           log = FALSE,
                           use_absolute_numbers = FALSE,
                           show_compartment = c("D", "S", "E", "C", "H", "R"),
                           freescales = TRUE,
                           base_size = 11,
                           show_ribbon = TRUE,
                           cri_alpha = 0.75,
                           cri_display_alpha = 1) {

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

    map_dfr(
      simexl,
      ~ extract(.x, what, stratify_by = c("time", "vax", "compartment")),
      .id = "scenario"
    ) |>
      # imap_dfr(simexl, ~ mutate(extract(.x, what), scenario = .y)) %>%
      mutate(scenario = fct_inorder(scenario)) %>%
      group_by(time, compartment, scenario) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      ggplot(
        aes(
          x = time,
          y = if (use_absolute_numbers) value else value / pop,
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
        labels = if (use_absolute_numbers) waiver() else scales::percent
      ) +
      scale_color_brewer(name = "Scenario", palette = "Dark2") +
      labs(x = "Day", y = str_to_title(what), color = "Category") +
      theme_minimal(base_size = base_size) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "white")
      )

  } else {

    ## Particles per run (for drawing CRI)
    n_particles_simex <- function(sx) {
      inc <- sx[["incidence"]]
      if (is.null(inc)) {
        return(1L)
      }
      if (inherits(inc, "data.frame") && "particle" %in% names(inc)) {
        return(length(unique(inc[["particle"]])))
      }
      if (is.array(inc)) {
        dns <- dimnames(inc)
        if (!is.null(dns) && "particle" %in% names(dns)) {
          return(length(dns[["particle"]]))
        }
      }
      return(1L)
    }

    ## Same CRI logic as plot.simex summary (extract sums over time, then CRI
    ## across particles), then scale like the previous endpoint comparison.
    df <- imap_dfr(simexl, function(sx, scenario_nm) {
      pop_i <- sum(sx$pars[[1]]$population)
      ex <- extract(
        sx,
        "incidence",
        cri = show_ribbon,
        cri_alpha = cri_alpha,
        filter = list(compartment = show_compartment),
        stratify_by = "age"
      )
      out <- as.data.frame(ex)
      af <- sx$pars[[1]]$age_frac[out$age]
      if (!use_absolute_numbers) {
        out$value <- out$value / (pop_i * af)
        if (all(c("lower", "upper") %in% names(out))) {
          out$lower <- out$lower / (pop_i * af)
          out$upper <- out$upper / (pop_i * af)
        }
      }
      out$scenario <- scenario_nm
      out
    }) %>%
      mutate(
        scenario = fct_inorder(factor(scenario, levels = unique(scenario)))
      ) %>%
      arrange(scenario, as.numeric(.data$age))

    if (type == "highchart") {

      df <- df %>%
        mutate(
          value = if (use_absolute_numbers) value else value * 100,
          age = get_age_cat()[as.numeric(age)]
        )
      if (!use_absolute_numbers && all(c("lower", "upper") %in% names(df))) {
        df$lower <- df$lower * 100
        df$upper <- df$upper * 100
      }

      scen_lv <- levels(df$scenario)
      scen_col_map <- scenario_colors_dark2(scen_lv)
      age_cats <- unname(get_age_cat())

      hc <- highchart() %>%
        hc_chart(type = "column", backgroundColor = "#FFFFFF")

      ## Columns first, then error bars, so CRI draws on top (not hidden under
      ## fills). High zIndex on errorbar keeps stems visible over bars.
      err_specs <- vector("list", length(scen_lv))
      for (i in seq_along(scen_lv)) {
        sn <- scen_lv[[i]]
        dm <- df[df$scenario == sn, , drop = FALSE]
        col <- scen_col_map[[sn]]
        sid <- paste0("endpoint_col_", i)
        sx <- simexl[[sn]]
        hc <- hc %>%
          hc_add_series(
            data = dm,
            type = "column",
            hcaes(x = age, y = value),
            id = sid,
            name = as.character(sn),
            color = col,
            zIndex = 2L
          )
        if (
          isTRUE(show_ribbon) &&
            n_particles_simex(sx) > 1L &&
            all(c("lower", "upper") %in% names(dm))
        ) {
          err_specs[[i]] <- list(
            sid = sid,
            data = lapply(seq_len(nrow(dm)), function(j) {
              list(low = dm$lower[j], high = dm$upper[j])
            })
          )
        }
      }
      for (spec in err_specs) {
        if (is.null(spec)) {
          next
        }
        hc <- hc %>%
          hc_add_series(
            data = spec$data,
            type = "errorbar",
            linkedTo = spec$sid,
            color = "#000000",
            stemWidth = 1.25,
            whiskerLength = 5,
            zIndex = 10L
          )
      }

      hc %>%
        hc_plotOptions(
          column = list(grouping = TRUE, borderWidth = 0, zIndex = 2),
          errorbar = list(zIndex = 10, color = "#000000")
        ) %>%
        hc_yAxis(
          title = list(text = ifelse(use_absolute_numbers, "Count", "Proportion of population")),
          labels = list(format = ifelse(use_absolute_numbers, "{value}", "{value}%")),
          min = 0
        ) %>%
        hc_xAxis(
          title = list(text = "Age"),
          categories = age_cats
        ) %>%
        hc_tooltip(
          valueDecimals = if (use_absolute_numbers) 0L else 2L,
          valueSuffix = if (use_absolute_numbers) "" else "%"
        )

    } else {

      dodge <- position_dodge(width = 0.82)
      p <- df %>%
        mutate(age = get_age_cat()[as.numeric(age)]) %>%
        ggplot(aes(age, value, fill = scenario)) +
        geom_col(position = dodge) +
        scale_x_discrete(drop = FALSE, labels = get_age_cat()) +
        scale_y_continuous(
          expand = expansion(mult = c(0.01, 0.05)),
          labels =
            if (use_absolute_numbers) waiver()
            else function(x) scales::percent(x, 0.001)
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
      show_err <- isTRUE(show_ribbon) &&
        any(vapply(simexl, function(x) n_particles_simex(x) > 1L, logical(1L))) &&
        all(c("lower", "upper") %in% names(df))
      if (show_err) {
        p <- p +
          geom_errorbar(
            aes(ymin = lower, ymax = upper),
            position = dodge,
            width = 0.18,
            linewidth = 0.45,
            alpha = cri_display_alpha,
            colour = "black"
          )
      }
      p

    }

  }

}
