#' Timeline for one outcome across saved scenarios
#'
#' Builds a highcharter line chart (or one chart per age group) comparing
#' named simex objects, using [extract()] for incidence or prevalence in a
#' single compartment (cases = E, hospitalisations = H, deaths = D).
#'
#' @param simexl Named list of simex objects (e.g. from the Shiny app).
#' @param outcome One of `"Cases"`, `"Hospitalisations"`, `"Deaths"`.
#' @param what Passed to [extract()]: `"incidence"` or `"prevalence"`.
#' @param stratify_by_age If `FALSE`, ages are summed; if `TRUE`, one small
#'   multiple per age (like the fit tab), each showing all scenarios.
#' @param use_absolute_numbers If `FALSE`, values are divided by total
#'   population `N` from the first simex (same idea as [plot.simex()]).
#' @param show_ribbon If `TRUE` and there are multiple particles, show
#'   credible intervals from [extract()] with `cri = TRUE`.
#' @param cri_alpha Width of central interval when `show_ribbon` is true
#'   (default `0.75`).
#' @param type Currently only `"highchart"`.
#' @param period_days Passed to [extract()] when `what = "incidence"` (default
#'   `7L` for weekly totals). Ignored for prevalence.
#'
#' @return A `highchart` object, or a list of them when
#'   `stratify_by_age` is `TRUE`. `NULL` if `simexl` is empty or `NULL`.
#'
#' @importFrom dplyr mutate transmute
#' @importFrom forcats fct_inorder
#' @importFrom purrr imap_dfr
#'
#' @author Finlay Campbell
#'
#' @export
vis_saved_timeline <- function(simexl,
                               outcome = c("Cases", "Hospitalisations", "Deaths"),
                               what = c("incidence", "prevalence"),
                               stratify_by_age = FALSE,
                               use_absolute_numbers = TRUE,
                               show_ribbon = FALSE,
                               cri_alpha = 0.75,
                               type = c("highchart"),
                               period_days = 7L) {
  if (is.null(simexl) || length(simexl) == 0) {
    return(NULL)
  }

  type <- match.arg(type)
  what <- match.arg(what)
  outcome <- match.arg(outcome)
  period_days <- as.integer(period_days)
  if (length(period_days) != 1L || is.na(period_days) || period_days < 1L) {
    stop("period_days must be a single positive integer.", call. = FALSE)
  }
  pd_ex <- if (what == "incidence") period_days else 1L

  cmpt <- c(Cases = "E", Hospitalisations = "H", Deaths = "D")[[outcome]]
  flt <- list(compartment = cmpt)

  pop <- simexl[[1]]$pars[[1]]$N

  if (!stratify_by_age) {
    df <- imap_dfr(
      simexl,
      function(sx, nm) {
        ex <- extract(
          sx,
          what,
          filter = flt,
          cri = show_ribbon,
          cri_alpha = cri_alpha,
          stratify_by = c("time"),
          period_days = pd_ex
        )
        d <- as.data.frame(ex)
        d$scenario <- nm
        d
      }
    ) |>
      mutate(scenario = fct_inorder(scenario))

    if (!use_absolute_numbers) {
      df$value <- df$value / pop
      if ("lower" %in% names(df)) {
        df$lower <- df$lower / pop
        df$upper <- df$upper / pop
      }
    }

    hc <- highchart() |>
      hc_chart(type = "line", backgroundColor = "#FFFFFF")

    scen_lv <- levels(df$scenario)
    scen_col_map <- scenario_colors_dark2(scen_lv)
    for (i in seq_along(scen_lv)) {
      sn <- scen_lv[[i]]
      dm <- df[df$scenario == sn, , drop = FALSE]
      sid <- paste0("saved_tl_", i)
      col <- scen_col_map[[sn]]
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

    x_lab <- if (what == "incidence" && period_days > 1L) {
      paste0("Period start (day); ", period_days, "-day totals")
    } else {
      "Day"
    }
    y_title <- if (use_absolute_numbers) {
      if (what == "incidence") {
        if (period_days <= 1L) "Daily count" else if (period_days == 7L) {
          "Weekly count"
        } else {
          paste0(period_days, "-day count")
        }
      } else {
        "Count"
      }
    } else if (what == "incidence" && period_days > 1L) {
      if (period_days == 7L) "Weekly proportion" else paste0(period_days, "-day proportion")
    } else {
      "Proportion"
    }
    return(
      hc |>
        hc_plotOptions(line = list(lineWidth = 3)) |>
        hc_xAxis(title = list(text = x_lab)) |>
        hc_yAxis(title = list(text = y_title), min = 0) |>
        hc_tooltip(valueDecimals = 0) |>
        hc_legend(enabled = TRUE) |>
        hc_exporting(enabled = TRUE)
    )
  }

  ## Age-stratified: one chart per age, scenarios as coloured lines
  df <- imap_dfr(
    simexl,
    function(sx, nm) {
      ex <- extract(
        sx,
        what,
        filter = flt,
        cri = show_ribbon,
        cri_alpha = cri_alpha,
        stratify_by = c("time", "age"),
        period_days = pd_ex
      )
      d <- as.data.frame(ex)
      d$scenario <- nm
      d
    }
  ) |>
    mutate(scenario = fct_inorder(scenario))

  if (!use_absolute_numbers) {
    df$value <- df$value / pop
    if ("lower" %in% names(df)) {
      df$lower <- df$lower / pop
      df$upper <- df$upper / pop
    }
  }

  ages_vec <- unique(as.character(df$age))
  age_ord <- order(as.numeric(gsub("age_", "", ages_vec)))
  ages_sorted <- ages_vec[age_ord]

  scen_lv <- levels(df$scenario)
  scen_col_map <- scenario_colors_dark2(scen_lv)

  x_lab <- if (what == "incidence" && period_days > 1L) {
    paste0("Period start (day); ", period_days, "-day totals")
  } else {
    "Day"
  }
  y_title <- if (use_absolute_numbers) {
    if (what == "incidence") {
      if (period_days <= 1L) "Daily count" else if (period_days == 7L) {
        "Weekly count"
      } else {
        paste0(period_days, "-day count")
      }
    } else {
      "Count"
    }
  } else if (what == "incidence" && period_days > 1L) {
    if (period_days == 7L) "Weekly proportion" else paste0(period_days, "-day proportion")
  } else {
    "Proportion"
  }

  lapply(ages_sorted, function(ag) {
    hc <- highchart() |>
      hc_chart(type = "line", backgroundColor = "#FFFFFF") |>
      hc_title(text = age_stratum_title(ag))

    for (i in seq_along(scen_lv)) {
      sn <- scen_lv[[i]]
      dm <- df[df$age == ag & df$scenario == sn, , drop = FALSE]
      if (nrow(dm) == 0) {
        next
      }
      col <- scen_col_map[[sn]]
      sid <- paste0("saved_tl_", ag, "_", i)
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

    hc |>
      hc_plotOptions(line = list(lineWidth = 3)) |>
      hc_xAxis(title = list(text = x_lab)) |>
      hc_yAxis(title = list(text = y_title), min = 0) |>
      hc_tooltip(valueDecimals = 0) |>
      hc_legend(enabled = TRUE) |>
      hc_exporting(enabled = FALSE)
  })
}
