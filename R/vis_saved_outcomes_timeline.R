#' Timeline for one outcome (E, H, or D) with optional incidence data
#'
#' Shows model trajectories for a single compartment when `simexl` is non-empty,
#' and/or observed incidence points when `what = "incidence"` and `data` is
#' supplied. Data-only plots are supported (empty `simexl` or `list()`).
#'
#' @param simexl Named list of simex objects, or `list()` for data-only.
#' @param outcome One of `"Cases"`, `"Hospitalisations"`, `"Deaths"`.
#' @param what `"incidence"` or `"prevalence"` for the model trajectories.
#' @param stratify_by_age If `FALSE`, ages are summed. If `TRUE`, one chart per
#'   age (ages from the model if present, otherwise from `data`).
#' @param data Optional data frame with columns `time`, `age`, `compartment`,
#'   and `value` (incidence).
#' @param use_absolute_numbers If `FALSE`, model values are divided by `N`
#'   from the first simex (ignored when there is no model).
#' @param show_ribbon Credible intervals when multiple particles exist.
#' @param cri_alpha Width of central interval (default `0.75`).
#' @param period_days Passed to [extract()] for model series when
#'   `what = "incidence"`; observed incidence is summed over the same periods
#'   (default `7L` for weekly totals). Use `1L` for daily values.
#'
#' @return A single `highchart` when `stratify_by_age` is `FALSE`, or a list
#'   of highcharts when `TRUE`. `NULL` if there is nothing to plot.
#'
#' @importFrom dplyr group_by summarise mutate transmute .data
#' @importFrom purrr imap_dfr
#'
#' @author Finlay Campbell
#'
#' @export
vis_saved_outcomes_timeline <- function(simexl,
                                        outcome = c(
                                          "Cases",
                                          "Hospitalisations",
                                          "Deaths"
                                        ),
                                        what = c("incidence", "prevalence"),
                                        stratify_by_age = FALSE,
                                        data = NULL,
                                        use_absolute_numbers = TRUE,
                                        show_ribbon = FALSE,
                                        cri_alpha = 0.75,
                                        period_days = 7L) {
  outcome <- match.arg(outcome)
  what <- match.arg(what)
  period_days <- as.integer(period_days)
  if (length(period_days) != 1L || is.na(period_days) || period_days < 1L) {
    stop("period_days must be a single positive integer.", call. = FALSE)
  }

  cmpt <- c(
    Cases = "E",
    Hospitalisations = "H",
    Deaths = "D"
  )[[outcome]]

  has_model <- !is.null(simexl) && length(simexl) > 0
  pop <- if (has_model) {
    simexl[[1]]$pars[[1]]$N
  } else {
    NA_real_
  }

  ## Data-only: require incidence (observations are incidence counts)
  if (!has_model && what != "incidence") {
    return(NULL)
  }

  obs_ready <- !is.null(data) &&
    what == "incidence" &&
    is.data.frame(data) &&
    all(c("time", "age", "compartment", "value") %in% names(data))

  if (obs_ready) {
    obs <- as.data.frame(data)
    obs$compartment <- trimws(as.character(obs$compartment))
    obs$age <- trimws(as.character(obs$age))
    obs <- obs[obs$compartment == cmpt, , drop = FALSE]
    if (!stratify_by_age) {
      obs <- obs |>
        group_by(.data$time) |>
        summarise(value = sum(.data$value), .groups = "drop")
    }
  } else {
    obs <- NULL
  }

  if (what == "incidence" && period_days > 1L && !is.null(obs) && nrow(obs) > 0) {
    obs <- aggregate_obs_incidence_by_period(obs, period_days)
  }

  if (!has_model) {
    if (is.null(obs) || nrow(obs) == 0) {
      return(NULL)
    }
  }

  x_axis_label <- "Time"

  y_title <- if (!has_model || use_absolute_numbers) {
    if (what == "incidence") {
      if (period_days <= 1L) {
        "Daily count"
      } else if (period_days == 7L) {
        "Weekly count"
      } else {
        paste0(period_days, "-day count")
      }
    } else {
      "Count"
    }
  } else {
    if (what == "incidence" && period_days > 1L) {
      if (period_days == 7L) "Weekly proportion" else paste0(period_days, "-day proportion")
    } else {
      "Proportion"
    }
  }

  scen_lv <- if (has_model) {
    nm <- names(simexl)
    if (is.null(nm)) as.character(seq_along(simexl)) else nm
  } else {
    character(0)
  }
  n_scen <- length(scen_lv)
  scen_col_map <- if (n_scen > 0L) {
    scenario_colors_dark2(scen_lv)
  } else {
    stats::setNames(character(0), character(0))
  }

  extract_mod <- function(sx, nm, strat_age) {
    strat <- if (strat_age) c("time", "age") else "time"
    ex <- extract(
      sx,
      what,
      filter = list(compartment = cmpt),
      cri = show_ribbon,
      cri_alpha = cri_alpha,
      stratify_by = strat,
      period_days = if (what == "incidence") period_days else 1L
    )
    d <- as.data.frame(ex)
    d$scenario <- nm
    d
  }

  build_hc <- function(df, obs_df, chart_title = NULL) {
    tooltip_decimals <- if (has_model && !use_absolute_numbers) 2L else 0L
    if (has_model && !use_absolute_numbers) {
      df$value <- df$value / pop
      if ("lower" %in% names(df)) {
        df$lower <- df$lower / pop
        df$upper <- df$upper / pop
      }
    }

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

    if (has_model && nrow(df) > 0) {
      for (i in seq_along(scen_lv)) {
        sn <- scen_lv[[i]]
        dm <- df[df$scenario == sn, , drop = FALSE]
        if (nrow(dm) == 0) {
          next
        }
        col <- scen_col_map[[sn]]
        sid <- paste0("mod_", i)
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
        if (show_ribbon && all(c("lower", "upper") %in% names(dm))) {
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

    if (!is.null(obs_df) && nrow(obs_df) > 0) {
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

    ## Shared tooltip + crosshair; HTML header/pointFormat (no custom formatter).
    out <- hc |>
      hc_plotOptions(
        line = list(lineWidth = 3),
        scatter = list(
          stickyTracking = TRUE,
          findNearestPointBy = "x"
        )
      ) |>
      hc_xAxis(
        title = list(text = x_axis_label),
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
    out
  }

  if (!stratify_by_age) {
    df <- if (has_model) {
      imap_dfr(simexl, function(sx, nm) extract_mod(sx, nm, FALSE)) |>
        mutate(scenario = factor(.data$scenario, levels = scen_lv))
    } else {
      data.frame(
        time = numeric(0),
        value = numeric(0),
        scenario = character(0),
        stringsAsFactors = FALSE
      )
    }
    obs_df <- if (is.null(obs)) {
      NULL
    } else {
      obs[, c("time", "value"), drop = FALSE]
    }
    if (!has_model && (is.null(obs_df) || nrow(obs_df) == 0)) {
      return(NULL)
    }
    return(build_hc(df, obs_df))
  }

  ## Age-stratified
  if (has_model) {
    ex_age <- extract(
      simexl[[1]],
      what,
      filter = list(compartment = cmpt),
      cri = FALSE,
      stratify_by = c("time", "age"),
      period_days = if (what == "incidence") period_days else 1L
    )
    ages_vec <- unique(as.character(as.data.frame(ex_age)$age))
  } else {
    ages_vec <- unique(as.character(obs$age))
  }
  if (length(ages_vec) == 0L) {
    return(NULL)
  }
  age_ord <- order(as.numeric(gsub("age_", "", ages_vec)))
  ages_sorted <- ages_vec[age_ord]

  charts <- lapply(ages_sorted, function(ag) {
    df <- if (has_model) {
      imap_dfr(simexl, function(sx, nm) extract_mod(sx, nm, TRUE)) |>
        mutate(scenario = factor(.data$scenario, levels = scen_lv))
    } else {
      data.frame(
        time = numeric(0),
        value = numeric(0),
        scenario = character(0),
        stringsAsFactors = FALSE
      )
    }
    if (has_model) {
      df <- df[as.character(df$age) == ag, , drop = FALSE]
    }
    obs_df <- if (is.null(obs)) {
      NULL
    } else {
      od <- obs[as.character(obs$age) == ag, , drop = FALSE]
      if (nrow(od) == 0) {
        NULL
      } else {
        od[, c("time", "value"), drop = FALSE]
      }
    }
    ## Skip panel with no model line and no points
    if ((!has_model || nrow(df) == 0) && (is.null(obs_df) || nrow(obs_df) == 0)) {
      return(NULL)
    }
    build_hc(df, obs_df, chart_title = age_stratum_title(ag))
  })
  charts <- charts[!vapply(charts, is.null, logical(1))]
  if (length(charts) == 0L) {
    return(NULL)
  }
  if (length(charts) == 1L) {
    return(charts[[1]])
  }
  charts
}
