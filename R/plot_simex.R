## Unified plotting for simex objects (single run or scenario list).
## All model series are built with extract.simex(); renderers only draw.

#' @importFrom dplyr arrange group_by mutate summarise transmute .data
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 aes element_rect expansion facet_grid facet_wrap
#'   geom_col geom_errorbar geom_line geom_point geom_ribbon ggplot labeller
#'   labs position_dodge scale_color_brewer scale_fill_brewer
#'   scale_x_discrete scale_y_continuous theme theme_minimal vars waiver
#' @importFrom purrr imap_dfr
NULL

## Wrap one simex or a named list into a named list for scenario colouring.
normalize_simex_list <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (inherits(x, "simex")) {
    return(stats::setNames(list(x), "Model"))
  }
  if (!is.list(x)) {
    stop("`x` must be a simex object or a list of simex objects.", call. = FALSE)
  }
  if (length(x) == 0L) {
    return(list())
  }
  nm <- names(x)
  if (is.null(nm) || any(!nzchar(nm))) {
    names(x) <- paste0("scenario_", seq_along(x))
  }
  x
}

## Count particles for CRI gating (endpoint error bars / timeline ribbons).
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

## Run extract per scenario and bind with a scenario column.
bind_scenarios_timeline <- function(simexl, what, compartments, stratify_by_age,
                                    show_ribbon, cri_alpha, period_days) {
  if (length(simexl) == 0L) {
    return(data.frame())
  }
  strat <- if (isTRUE(stratify_by_age)) {
    c("time", "age", "compartment")
  } else {
    c("time", "compartment")
  }
  pd <- if (what == "incidence") period_days else 1L
  imap_dfr(simexl, function(sx, nm) {
    ex <- extract(
      sx,
      what,
      filter = list(compartment = compartments),
      cri = show_ribbon,
      cri_alpha = cri_alpha,
      stratify_by = strat,
      period_days = pd
    )
    d <- as.data.frame(ex)
    d$scenario <- nm
    d
  }) |>
    mutate(scenario = factor(.data$scenario, levels = names(simexl)))
}

## Prepare optional observed incidence for timeline overlays (vis_saved rules).
prepare_timeline_obs_data <- function(data, what, compartments, period_days,
                                      stratify_by_age) {
  if (is.null(data)) {
    return(NULL)
  }
  if (what != "incidence") {
    return(NULL)
  }
  if (!is.data.frame(data)) {
    return(NULL)
  }
  need <- c("time", "age", "compartment", "value")
  if (!all(need %in% names(data))) {
    return(NULL)
  }
  obs <- as.data.frame(data)
  obs$compartment <- trimws(as.character(obs$compartment))
  obs$age <- trimws(as.character(obs$age))
  obs <- obs[obs$compartment %in% compartments, , drop = FALSE]
  if (nrow(obs) == 0L) {
    return(NULL)
  }
  if (!isTRUE(stratify_by_age)) {
    obs <- obs |>
      group_by(.data$time, .data$compartment) |>
      summarise(value = sum(.data$value), .groups = "drop")
  }
  if (period_days > 1L) {
    obs <- aggregate_obs_incidence_by_period(obs, period_days)
  }
  obs
}

## Y-axis title for timeline (counts).
timeline_y_title <- function(what, period_days) {
  if (what == "incidence") {
    if (period_days <= 1L) {
      return("Daily count")
    }
    if (period_days == 7L) {
      return("Weekly count")
    }
    return(paste0(period_days, "-day count"))
  }
  return("Count")
}

## Endpoint: incidence summed over time, CRI by age, one compartment.
bind_scenarios_endpoint <- function(simexl, show_compartment, show_ribbon,
                                    cri_alpha, period_days) {
  imap_dfr(simexl, function(sx, scenario_nm) {
    ex <- extract(
      sx,
      "incidence",
      cri = show_ribbon,
      cri_alpha = cri_alpha,
      filter = list(compartment = show_compartment),
      stratify_by = "age",
      period_days = period_days
    )
    out <- as.data.frame(ex)
    out$scenario <- scenario_nm
    out
  }) |>
    mutate(
      scenario = fct_inorder(factor(.data$scenario, levels = unique(.data$scenario))),
      age_num = as.numeric(gsub("age_", "", .data$age))
    ) |>
    arrange(.data$scenario, .data$age_num)
}

## Highcharter timeline (single compartment); optional obs scatter.
render_timeline_highcharter <- function(df, obs_df, scen_lv, scen_col_map,
                                        show_ribbon, what, period_days,
                                        chart_title = NULL) {
  y_title <- timeline_y_title(what, period_days)
  tooltip_decimals <- 0L

  hc <- highchart() |>
    hc_chart(
      type = "line",
      backgroundColor = "#FFFFFF",
      events = list(
        load = highcharter::JS(
          "function() { var el = this.container; if (el) el.oncontextmenu = function(e) { e.preventDefault(); }; }"
        )
      )
    )

  if (nrow(df) > 0L) {
    for (i in seq_along(scen_lv)) {
      sn <- scen_lv[[i]]
      dm <- df[df$scenario == sn, , drop = FALSE]
      if (nrow(dm) == 0L) {
        next
      }
      col <- scen_col_map[[sn]]
      sid <- paste0("plot_simex_mod_", i)
      hc <- hc |>
        hc_add_series(
          data = dm,
          type = "line",
          hcaes(x = time, y = value),
          id = sid,
          name = as.character(sn),
          color = col,
          marker = list(enabled = FALSE)
        )
      if (isTRUE(show_ribbon) && all(c("lower", "upper") %in% names(dm))) {
        hc <- hc |>
          hc_add_series(
            data = dm |>
              transmute(time = time, low = lower, high = upper),
            type = "arearange",
            hcaes(x = time, low = low, high = high),
            linkedTo = sid,
            color = col,
            fillOpacity = 0.2,
            lineWidth = 0,
            marker = list(enabled = FALSE),
            enableMouseTracking = FALSE
          )
      }
    }
  }

  if (!is.null(obs_df) && nrow(obs_df) > 0L) {
    hc <- hc |>
      hc_add_series(
        data = obs_df,
        type = "scatter",
        hcaes(x = time, y = value),
        name = "Reported",
        color = "#c9424a",
        marker = list(symbol = "circle", radius = 4)
      )
  }

  out <- hc |>
    hc_plotOptions(
      line = list(lineWidth = 3),
      scatter = list(
        stickyTracking = TRUE,
        findNearestPointBy = "x"
      )
    ) |>
    hc_xAxis(
      title = list(text = "Time"),
      crosshair = list(
        width = 1,
        color = "#666666",
        dashStyle = "ShortDot"
      )
    ) |>
    hc_yAxis(title = list(text = y_title), min = 0) |>
    hc_tooltip(
      useHTML = TRUE,
      shared = TRUE,
      split = FALSE,
      valueDecimals = tooltip_decimals,
      headerFormat = paste0(
        "<span style=\"font-size:11px\"><b>Day {point.key}</b></span><br/>"
      ),
      pointFormat = paste0(
        "<span style=\"color:{point.color}\">&#9679;</span> ",
        "{series.name}: {point.y}<br/>"
      )
    ) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(enabled = FALSE)

  if (!is.null(chart_title) && nzchar(chart_title)) {
    out <- out |> hc_title(text = chart_title)
  }
  return(out)
}

## ggplot timeline: facet by compartment and/or age; CRI ribbons; optional obs.
render_timeline_ggplot <- function(df, obs_df, show_ribbon, what, period_days,
                                   log, freescales, base_size,
                                   stratify_by_age = FALSE) {
  lbs <- c(
    S = "Susceptible", E = "Exposed", C = "Community Infection",
    H = "Hospital Infection", R = "Recovered", D = "Dead"
  )
  age_levels <- unname(get_age_cat())
  y_title <- timeline_y_title(what, period_days)
  has_mod <- nrow(df) > 0L
  has_obs <- !is.null(obs_df) && nrow(obs_df) > 0L

  if (!has_mod && !has_obs) {
    return(NULL)
  }

  if (has_mod) {
    if (isTRUE(stratify_by_age)) {
      df <- df |>
        mutate(
          age_lbl = factor(
            get_age_cat()[as.character(.data$age)],
            levels = age_levels
          ),
          ribbon_grp = interaction(
            .data$scenario,
            .data$compartment,
            .data$age_lbl,
            drop = TRUE
          )
        )
    } else {
      df <- df |>
        mutate(
          age_lbl = NA_character_,
          ribbon_grp = interaction(
            .data$scenario,
            .data$compartment,
            drop = TRUE
          )
        )
    }

    p <- ggplot(df, aes(x = .data$time, y = .data$value, color = .data$scenario)) +
      geom_line(linewidth = 1.1)

    if (isTRUE(show_ribbon) && all(c("lower", "upper") %in% names(df))) {
      p <- p +
        geom_ribbon(
          aes(
            ymin = .data$lower,
            ymax = .data$upper,
            fill = .data$scenario,
            group = .data$ribbon_grp
          ),
          alpha = 0.2,
          colour = NA
        )
    }

    if (has_obs) {
      obs_plot <- obs_df
      if (isTRUE(stratify_by_age) && "age" %in% names(obs_plot)) {
        obs_plot <- obs_plot |>
          mutate(
            age_lbl = factor(
              get_age_cat()[as.character(.data$age)],
              levels = age_levels
            )
          )
      }
      p <- p +
        geom_point(
          data = obs_plot,
          aes(x = .data$time, y = .data$value),
          inherit.aes = FALSE,
          colour = "#c9424a",
          size = 2,
          shape = 16
        )
    }

    facet_scales <- ifelse(freescales, "free_y", "fixed")
    if (isTRUE(stratify_by_age)) {
      n_comp <- length(unique(df$compartment))
      if (n_comp > 1L) {
        p <- p +
          facet_grid(
            rows = vars(age_lbl),
            cols = vars(compartment),
            scales = facet_scales,
            labeller = labeller(compartment = lbs)
          )
      } else {
        p <- p +
          facet_wrap(~age_lbl, scales = facet_scales, nrow = 2L)
      }
    } else {
      p <- p +
        facet_wrap(
          ~compartment,
          scales = facet_scales,
          labeller = labeller(compartment = lbs)
        )
    }

    p <- p +
      scale_y_continuous(
        limits = if (!log) c(0, NA) else waiver(),
        expand = expansion(mult = c(0.01, 0.05)),
        trans = ifelse(log, "log10", "identity"),
        labels = waiver()
      ) +
      scale_color_brewer(name = "Scenario", palette = "Dark2") +
      scale_fill_brewer(name = "Scenario", palette = "Dark2") +
      labs(x = "Day", y = y_title, color = "Scenario", fill = "Scenario") +
      theme_minimal(base_size = base_size) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = "white")
      )
    return(p)
  }

  ## Data-only (no model trajectories)
  p <- ggplot(obs_df, aes(x = .data$time, y = .data$value)) +
    geom_point(colour = "#c9424a", size = 2, shape = 16) +
    scale_y_continuous(
      limits = if (!log) c(0, NA) else waiver(),
      expand = expansion(mult = c(0.01, 0.05)),
      trans = ifelse(log, "log10", "identity"),
      labels = waiver()
    ) +
    labs(x = "Day", y = y_title) +
    theme_minimal(base_size = base_size) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

  if ("compartment" %in% names(obs_df) && length(unique(obs_df$compartment)) > 1L) {
    p <- p +
      facet_wrap(~compartment,
        scales = ifelse(freescales, "free_y", "fixed"),
        labeller = labeller(compartment = lbs)
      )
  }
  return(p)
}

## Highcharter endpoint (columns + optional errorbar).
render_endpoint_highcharter <- function(df, simexl, show_ribbon, scen_lv,
                                        scen_col_map) {
  age_lbl_ord <- unname(get_age_cat())
  df <- df |>
    mutate(
      age_num = as.numeric(gsub("age_", "", .data$age)),
      age = get_age_cat()[as.character(.data$age)]
    ) |>
    arrange(.data$scenario, .data$age_num)

  hc <- highchart() |>
    hc_chart(type = "column", backgroundColor = "#FFFFFF")

  err_specs <- vector("list", length(scen_lv))
  for (i in seq_along(scen_lv)) {
    sn <- scen_lv[[i]]
    dm <- df[df$scenario == sn, , drop = FALSE]
    col <- scen_col_map[[sn]]
    sid <- paste0("plot_simex_ep_", i)
    sx <- simexl[[sn]]
    hc <- hc |>
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
    hc <- hc |>
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

  hc |>
    hc_plotOptions(
      column = list(grouping = TRUE, borderWidth = 0, zIndex = 2),
      errorbar = list(zIndex = 10, color = "#000000")
    ) |>
    hc_yAxis(
      title = list(text = "Count"),
      labels = list(format = "{value}"),
      min = 0
    ) |>
    hc_xAxis(
      title = list(text = "Age"),
      categories = age_lbl_ord
    ) |>
    hc_tooltip(valueDecimals = 0L, valueSuffix = "")
}

## ggplot endpoint: columns + errorbars when CRI present and particles > 1.
render_endpoint_ggplot <- function(df, simexl, show_ribbon, show_compartment,
                                   what, base_size, cri_display_alpha) {
  lbs <- c(
    S = "Susceptible", E = "Exposed", C = "Community Infection",
    H = "Hospital Infection", R = "Recovered", D = "Dead"
  )
  age_levels <- unname(get_age_cat())
  dodge <- position_dodge(width = 0.82)
  p <- df |>
    mutate(
      age_disp = factor(
        get_age_cat()[as.character(.data$age)],
        levels = age_levels
      )
    ) |>
    ggplot(aes(.data$age_disp, .data$value, fill = .data$scenario)) +
    geom_col(position = dodge) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(
      expand = expansion(mult = c(0.01, 0.05)),
      labels = waiver()
    ) +
    scale_fill_brewer(name = "Scenario", palette = "Dark2", drop = FALSE) +
    labs(
      x = "Age category",
      y = paste0(
        "Final ", tools::toTitleCase(what),
        " of individuals in compartment: ",
        lbs[show_compartment]
      ),
      fill = "Scenario"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white"),
      legend.position = "bottom"
    )

  show_err <- isTRUE(show_ribbon) &&
    any(vapply(simexl, function(z) n_particles_simex(z) > 1L, logical(1L))) &&
    all(c("lower", "upper") %in% names(df))
  if (show_err) {
    p <- p +
      geom_errorbar(
        aes(x = .data$age_disp, ymin = .data$lower, ymax = .data$upper),
        position = dodge,
        width = 0.18,
        linewidth = 0.45,
        alpha = cri_display_alpha,
        colour = "black"
      )
  }
  return(p)
}

#' Unified plots for one or more simex scenarios
#'
#' Builds all model series via [extract.simex()] (including `period_days`,
#' `cri`, and `stratify_by`). Renders with **ggplot2** or **highcharter**
#' depending on `renderer`.
#'
#' @param x A single `simex` object or a **named** list of simex objects
#'   (scenarios). A single object is wrapped as `Model = x`. Unnamed list
#'   elements get names `scenario_1`, ...
#' @param mode `"timeline"` (values vs time) or `"endpoint"` (incidence summed
#'   over the full time range, stratified by age).
#' @param renderer `"highcharter"` or `"ggplot"`.
#' @param what For `mode = "timeline"`: `"incidence"` or `"prevalence"`.
#'   Ignored for `mode = "endpoint"` (always incidence).
#' @param compartments Character vector of compartment **letters** (e.g.
#'   `c("E", "H", "D")`). For `mode = "endpoint"`, must have length 1. For
#'   `renderer = "highcharter"` and `mode = "timeline"`, if more than one
#'   compartment is given, only the first is used (with a warning).
#' @param stratify_by_age If `TRUE` (timeline only), ages are kept separate
#'   using facets in **ggplot** (`facet_grid` when multiple compartments,
#'   `facet_wrap` otherwise). For **highcharter**, one chart per age stratum
#'   is still returned as a list. If `FALSE`, ages are summed inside
#'   [extract.simex()].
#' @param show_ribbon If `TRUE`, request CRI columns from `extract` and draw
#'   ribbons (timeline) or error bars (endpoint) when multiple particles
#'   exist.
#' @param cri_alpha Central interval width for `extract(..., cri = TRUE)`.
#' @param period_days Passed to [extract.simex()] (incidence period aggregation).
#' @param data Optional observed **incidence** (`time`, `age`, `compartment`,
#'   `value`). **Only used when `mode = "timeline"`**; if `mode = "endpoint"`
#'   and `data` is not `NULL`, a warning is issued and `data` is ignored.
#' @param log,freescales,base_size Passed through to ggplot timeline/endpoint
#'   where relevant (`freescales` applies to timeline facets).
#' @param cri_display_alpha Line alpha for ggplot endpoint error bars only.
#'
#' @return A ggplot or highchart object, or a **list** of highcharts when
#'   `mode = "timeline"`, `renderer = "highcharter"`, and
#'   `stratify_by_age = TRUE` (ggplot age stratification uses facets instead).
#'   `NULL` when there is nothing to plot.
#'
#' @examples
#' \dontrun{
#' pars <- get_parameters()
#' sx <- run_simex(pars, time = 0:50, n_particles = 4)
#' plot_simex(sx,
#'   mode = "timeline", renderer = "ggplot",
#'   what = "incidence", compartments = c("E", "D")
#' )
#' plot_simex(list(baseline = sx),
#'   mode = "endpoint", renderer = "ggplot",
#'   compartments = "E"
#' )
#' }
#'
#' @export
plot_simex <- function(x,
                       mode = c("timeline", "endpoint"),
                       renderer = c("ggplot", "highcharter"),
                       what = c("prevalence", "incidence"),
                       compartments = c("S", "E", "C", "H", "R", "D"),
                       stratify_by_age = FALSE,
                       show_ribbon = TRUE,
                       cri_alpha = 0.75,
                       period_days = 1L,
                       data = NULL,
                       log = FALSE,
                       freescales = TRUE,
                       base_size = 11,
                       cri_display_alpha = 1) {
  mode <- match.arg(mode)
  renderer <- match.arg(renderer)
  what <- match.arg(what)
  period_days <- as.integer(period_days)
  if (length(period_days) != 1L || is.na(period_days) || period_days < 1L) {
    stop("period_days must be a single positive integer.", call. = FALSE)
  }

  if (!is.null(data) && mode != "timeline") {
    warning("`data` is only used when mode = \"timeline\"; ignoring `data`.",
      call. = FALSE
    )
    data <- NULL
  }

  if (mode == "endpoint") {
    if (length(compartments) != 1L) {
      stop("For mode = \"endpoint\", `compartments` must have length 1.",
        call. = FALSE
      )
    }
    show_compartment <- compartments[[1L]]
  }

  compartments_plot <- compartments
  if (mode == "timeline" && renderer == "highcharter" &&
    length(compartments_plot) > 1L) {
    warning(
      "highcharter timeline uses one compartment; using \"",
      compartments_plot[[1L]], "\".",
      call. = FALSE,
      immediate. = TRUE
    )
    compartments_plot <- compartments_plot[seq_len(1L)]
  }

  simexl <- normalize_simex_list(x)
  has_model <- length(simexl) > 0L

  if (mode == "timeline") {
    obs_prep <- prepare_timeline_obs_data(
      data, what, compartments_plot, period_days, stratify_by_age
    )
    if (!is.null(obs_prep) && what != "incidence") {
      obs_prep <- NULL
    }

    if (!has_model && (is.null(obs_prep) || nrow(obs_prep) == 0L)) {
      return(NULL)
    }
    if (!has_model && what != "incidence") {
      return(NULL)
    }

    df <- if (has_model) {
      bind_scenarios_timeline(
        simexl, what, compartments_plot, stratify_by_age,
        show_ribbon, cri_alpha, period_days
      )
    } else {
      data.frame()
    }

    scen_lv <- if (has_model) {
      names(simexl)
    } else {
      character(0)
    }
    scen_col_map <- if (length(scen_lv) > 0L) {
      scenario_colors_dark2(scen_lv)
    } else {
      stats::setNames(character(0), character(0))
    }

    if (!isTRUE(stratify_by_age)) {
      obs_hc <- if (is.null(obs_prep)) {
        NULL
      } else {
        obs_prep[, c("time", "value"), drop = FALSE]
      }
      obs_gg_plot <- obs_prep
      if (renderer == "highcharter") {
        if (nrow(df) == 0L && (is.null(obs_hc) || nrow(obs_hc) == 0L)) {
          return(NULL)
        }
        return(render_timeline_highcharter(
          df, obs_hc, scen_lv, scen_col_map,
          show_ribbon, what, period_days, NULL
        ))
      }
      if (nrow(df) == 0L &&
        (is.null(obs_gg_plot) || nrow(obs_gg_plot) == 0L)) {
        return(NULL)
      }
      return(render_timeline_ggplot(
        df, obs_gg_plot, show_ribbon, what, period_days,
        log, freescales, base_size,
        stratify_by_age = FALSE
      ))
    }

    ## Age-stratified: ggplot uses facets; highcharter one chart per age.
    if (renderer == "ggplot") {
      if (nrow(df) == 0L &&
        (is.null(obs_prep) || nrow(obs_prep) == 0L)) {
        return(NULL)
      }
      return(render_timeline_ggplot(
        df, obs_prep, show_ribbon, what, period_days,
        log, freescales, base_size,
        stratify_by_age = TRUE
      ))
    }

    ## highcharter + stratify_by_age: small multiples as a list of charts
    if (has_model) {
      ex_age <- extract(
        simexl[[1]],
        what,
        filter = list(compartment = compartments_plot),
        cri = FALSE,
        stratify_by = c("time", "age", "compartment"),
        period_days = if (what == "incidence") period_days else 1L
      )
      ages_vec <- unique(as.character(as.data.frame(ex_age)$age))
    } else {
      if (is.null(obs_prep) || nrow(obs_prep) == 0L) {
        return(NULL)
      }
      ages_vec <- unique(as.character(obs_prep$age))
    }
    if (length(ages_vec) == 0L) {
      return(NULL)
    }
    ages_sorted <- ages_vec[order(as.numeric(gsub("age_", "", ages_vec)))]

    charts <- lapply(ages_sorted, function(ag) {
      df_ag <- if (has_model && nrow(df) > 0L) {
        df[as.character(df$age) == ag, , drop = FALSE]
      } else {
        data.frame()
      }
      obs_ag <- if (is.null(obs_prep)) {
        NULL
      } else {
        od <- obs_prep[as.character(obs_prep$age) == ag, , drop = FALSE]
        if (nrow(od) == 0L) {
          NULL
        } else {
          od[, c("time", "value"), drop = FALSE]
        }
      }
      if ((!has_model || nrow(df_ag) == 0L) &&
        (is.null(obs_ag) || (is.data.frame(obs_ag) && nrow(obs_ag) == 0L))) {
        return(NULL)
      }
      render_timeline_highcharter(
        df_ag, obs_ag, scen_lv, scen_col_map,
        show_ribbon, what, period_days,
        age_stratum_title(ag)
      )
    })
    charts <- charts[!vapply(charts, is.null, logical(1))]
    if (length(charts) == 0L) {
      return(NULL)
    }
    if (length(charts) == 1L) {
      return(charts[[1]])
    }
    return(charts)
  }

  ## Endpoint ----------------------------------------------------------------
  if (!has_model) {
    return(NULL)
  }

  df_ep <- bind_scenarios_endpoint(
    simexl, show_compartment, show_ribbon, cri_alpha, period_days
  )
  scen_lv <- levels(df_ep$scenario)
  scen_col_map <- scenario_colors_dark2(scen_lv)

  if (renderer == "highcharter") {
    return(render_endpoint_highcharter(
      df_ep, simexl, show_ribbon, scen_lv, scen_col_map
    ))
  }
  render_endpoint_ggplot(
    df_ep, simexl, show_ribbon, show_compartment,
    "incidence", base_size, cri_display_alpha
  )
}
