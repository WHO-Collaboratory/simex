#' Fit plot across saved scenarios vs observed data
#'
#' Compares community plus hospital incidence from each simex object to the
#' same observed series, using [extract()] with the same logic as
#' [plot.simex()] `format = "fit"`. Model curves are coloured by scenario.
#'
#' @param simexl Named list of simex objects.
#' @param data Data frame with columns `time`, `age`, `value`.
#' @param stratify_fit_by_age If `TRUE`, return a list of highcharts (one per
#'   age); if `FALSE`, one chart with all ages summed.
#' @param use_absolute_numbers Passed through to axis labelling (values are
#'   not re-scaled here; ensure data match model scale).
#' @param show_ribbon If `TRUE`, show credible intervals per scenario when
#'   particles allow.
#' @param cri_alpha Width of central interval (default `0.75`).
#' @param period_days Passed to [extract()] for model series and used to sum
#'   observed counts over the same periods (default `7L`).
#'
#' @return One `highchart` or a list of them (when `stratify_fit_by_age` is
#'   `TRUE`). `NULL` if `simexl` is empty or `data` is `NULL`.
#'
#' @importFrom dplyr group_by summarise transmute
#'
#' @author Finlay Campbell
#'
#' @export
vis_saved_fit <- function(simexl,
                          data,
                          stratify_fit_by_age = TRUE,
                          use_absolute_numbers = TRUE,
                          show_ribbon = FALSE,
                          cri_alpha = 0.75,
                          period_days = 7L) {
  if (is.null(simexl) || length(simexl) == 0) {
    return(NULL)
  }
  if (is.null(data)) {
    return(NULL)
  }

  period_days <- as.integer(period_days)
  if (length(period_days) != 1L || is.na(period_days) || period_days < 1L) {
    stop("period_days must be a single positive integer.", call. = FALSE)
  }

  data <- as.data.frame(data)[, c("time", "age", "value"), drop = FALSE]

  n_scen <- length(simexl)
  scen_names <- names(simexl)
  if (is.null(scen_names)) {
    scen_names <- as.character(seq_len(n_scen))
  }
  scen_col_map <- scenario_colors_dark2(scen_names)

  prep_mod_one <- function(simex) {
    df_mod <- extract(
      simex,
      "incidence",
      filter = list(compartment = c("C", "H")),
      cri = show_ribbon,
      cri_alpha = cri_alpha,
      stratify_by = c("time", "age"),
      period_days = period_days
    )
    df_mod <- as.data.frame(df_mod)
    has_cri <- "lower" %in% names(df_mod)

    if (!stratify_fit_by_age) {
      if (has_cri) {
        df_mod <- df_mod |>
          group_by(.data$time) |>
          summarise(
            value = sum(.data$value),
            lower = sum(.data$lower),
            upper = sum(.data$upper),
            .groups = "drop"
          )
      } else {
        df_mod <- df_mod |>
          group_by(.data$time) |>
          summarise(value = sum(.data$value), .groups = "drop")
      }
    }
    list(df = df_mod, has_cri = has_cri)
  }

  mod_list <- lapply(simexl, prep_mod_one)

  if (period_days > 1L) {
    data <- aggregate_obs_incidence_by_period(data, period_days)
  }

  x_lab <- if (period_days > 1L) {
    paste0("Period start (day); ", period_days, "-day totals")
  } else {
    "Day"
  }
  y_title <- if (use_absolute_numbers) {
    if (period_days <= 1L) "Daily count" else if (period_days == 7L) {
      "Weekly count"
    } else {
      paste0(period_days, "-day count")
    }
  } else if (period_days > 1L) {
    if (period_days == 7L) "Weekly proportion" else paste0(period_days, "-day proportion")
  } else {
    "Proportion"
  }
  tooltip_decimals <- if (use_absolute_numbers) 0L else 2L

  if (!stratify_fit_by_age) {
    data_agg <- data |>
      group_by(.data$time) |>
      summarise(value = sum(.data$value), .groups = "drop")

    hc <- highchart() |>
      hc_chart(type = "line", backgroundColor = "#FFFFFF")

    for (i in seq_len(n_scen)) {
      dm <- mod_list[[i]]$df
      sid <- paste0("saved_fit_", i)
      hc <- hc |>
        hc_add_series(
          data = dm,
          type = "line",
          hcaes(x = time, y = value),
          id = sid,
          name = scen_names[[i]],
          color = scen_col_map[[scen_names[[i]]]],
          marker = list(enabled = FALSE)
        )
      if (show_ribbon && mod_list[[i]]$has_cri) {
        hc <- hc |>
          hc_add_series(
            data = dm |>
              transmute(time = time, low = lower, high = upper),
            type = "arearange",
            hcaes(x = time, low = low, high = high),
            linkedTo = sid,
            color = scen_col_map[[scen_names[[i]]]],
            fillOpacity = 0.2,
            lineWidth = 0,
            marker = list(enabled = FALSE),
            enableMouseTracking = FALSE
          )
      }
    }

    hc <- hc |>
      hc_add_series(
        data = data_agg,
        type = "scatter",
        hcaes(x = time, y = value),
        name = "Reported",
        color = "#c9424a",
        marker = list(symbol = "circle", radius = 4)
      )

    return(
      hc |>
        hc_plotOptions(
          line = list(lineWidth = 5),
          scatter = list(
            stickyTracking = TRUE,
            findNearestPointBy = "x"
          )
        ) |>
        hc_xAxis(
          title = list(text = x_lab),
          crosshair = list(
            width = 1,
            color = "#666666",
            dashStyle = "ShortDot"
          )
        ) |>
        hc_yAxis(
          title = list(text = y_title),
          min = 0,
          labels = list(format = "{value}")
        ) |>
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
        hc_exporting(enabled = FALSE)
    )
  }

  ## Stratify by age: same age order as plot.simex (numeric age_ index)
  ages_union <- unique(as.character(mod_list[[1]]$df$age))
  for (i in seq_len(n_scen)) {
    ages_union <- union(ages_union, unique(as.character(mod_list[[i]]$df$age)))
  }
  age_order <- order(as.numeric(gsub("age_", "", ages_union)))
  ages <- ages_union[age_order]

  hc_list <- lapply(ages, function(ag) {
    hc <- highchart() |>
      hc_chart(type = "line", backgroundColor = "#FFFFFF") |>
      hc_title(text = age_stratum_title(ag))

    for (i in seq_len(n_scen)) {
      dm <- mod_list[[i]]$df[as.character(mod_list[[i]]$df$age) == ag, ,
        drop = FALSE
      ]
      if (nrow(dm) == 0) {
        next
      }
      sid <- paste0("saved_fit_", ag, "_", i)
      hc <- hc |>
        hc_add_series(
          data = dm,
          type = "line",
          hcaes(x = time, y = value),
          id = sid,
          name = scen_names[[i]],
          color = scen_col_map[[scen_names[[i]]]],
          marker = list(enabled = FALSE)
        )
      if (show_ribbon && mod_list[[i]]$has_cri) {
        hc <- hc |>
          hc_add_series(
            data = dm |>
              transmute(time = time, low = lower, high = upper),
            type = "arearange",
            hcaes(x = time, low = low, high = high),
            linkedTo = sid,
            color = scen_col_map[[scen_names[[i]]]],
            fillOpacity = 0.2,
            lineWidth = 0,
            marker = list(enabled = FALSE),
            enableMouseTracking = FALSE
          )
      }
    }

    dd <- data[as.character(data$age) == ag, , drop = FALSE]
    if (nrow(dd) > 0) {
      hc <- hc |>
        hc_add_series(
          data = dd,
          type = "scatter",
          hcaes(x = time, y = value),
          name = "Reported",
          color = "#c9424a",
          marker = list(symbol = "circle", radius = 4)
        )
    }

    hc |>
      hc_plotOptions(
        line = list(lineWidth = 5),
        scatter = list(
          stickyTracking = TRUE,
          findNearestPointBy = "x"
        )
      ) |>
      hc_xAxis(
        title = list(text = x_lab),
        crosshair = list(
          width = 1,
          color = "#666666",
          dashStyle = "ShortDot"
        )
      ) |>
      hc_yAxis(
        title = list(text = y_title),
        min = 0,
        labels = list(format = "{value}")
      ) |>
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
      hc_exporting(enabled = FALSE)
  })

  hc_list
}
