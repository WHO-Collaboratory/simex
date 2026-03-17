#' Plot timeline of prevalence or incidence.
#'
#' @param simex The solved system of equations as returned by \code{run_model}.
#'
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#'
#' @param format One of "timeline" (shows a timeline stratified by compartment
#'   and vaccination status) or "summary" (sum across time of incidence, with
#'   same stratification options as timeline).
#'
#' @param log Logical indicating whether a log scale should be used.
#'
#' @param freescales Logical indicating whether the different comparments should
#'   use different y-axes.
#'
#' @param show_hosp_capacity Logical indicating whether hospital
#'   capacity should be displayed in the "H" compartment (only
#'   possible when what = "prevalence").
#'
#' @param use_absolute_numbers Logical indicating whether absolute absolute
#'   numbers (in terms of individuals) should be shown or relative to population
#'   size.
#'
#' @param show_compartment The compartment to show when using
#'   \code{format = "summary"} (age stratification) or when
#'   \code{format = "timeline"}, \code{type = "highchart"} and
#'   \code{stratify_by = "age"}.
#'
#' @param stratify_by When \code{type = "highchart"} and \code{format =
#'   "timeline"} or \code{format = "summary"}, either \code{"compartment"}
#'   (all compartments, ages summed) or \code{"age"} (single compartment
#'   given by \code{show_compartment}, stratified by age).
#'
#' @param split_vax When \code{stratify_by = "compartment"}, if \code{TRUE}
#'   (default) series are split by vaccination status; if \code{FALSE},
#'   vaccination is not stratified.
#'
#' @param base_size Base size passed to theme_*.
#'
#' @param type "ggplot" will return a static ggplot figure, "highchart" will
#'   return a dynamic figure.
#'
#' @author Finlay Campbell
#'
#' @export
#'
plot.simex <- function(simex,
                       data = NULL,
                       what = c("prevalence", "incidence"),
                       format = c("timeline", "summary"),
                       show_compartment = c("E", "D", "S", "C", "H", "R"),
                       stratify_by = c("compartment", "age"),
                       type = c("ggplot", "highchart"),
                       log = FALSE,
                       freescales = TRUE,
                       show_hosp_capacity = TRUE,
                       show_ribbon = FALSE,
                       use_absolute_numbers = TRUE,
                       split_vax = FALSE,
                       cri_alpha = 0.95,
                       base_size = 11) {
  ## check arguments
  type <- match.arg(type)
  what <- match.arg(what)
  format <- match.arg(format)
  show_compartment <- match.arg(show_compartment)
  stratify_by <- match.arg(stratify_by)

  ## get population
  pop <- simex$pars[[1]]$N

  ## get hospital capacity
  hosp_capacity <- simex$pars[[1]]$hosp_capacity
  if (!use_absolute_numbers) hosp_capacity <- hosp_capacity / pop

  ## category labels
  cat <- c(
    S = "Susceptible", E = "Exposed", C = "Community Infection",
    H = "Hospital Infection", R = "Recovered", D = "Dead"
  )

  if (format == "timeline") {
    if (type == "highchart") {
      if (stratify_by == "compartment") {
        ## All compartments by colour (ages summed); optional split by vax
        strat_comp <- if (split_vax) {
          c("time", "vax", "compartment")
        } else {
          c("time", "compartment")
        }
        df <- extract(
          simex, what,
          cri = show_ribbon, cri_alpha,
          stratify_by = strat_comp
        )
        df <- as.data.frame(df)
        cols <- RColorBrewer::brewer.pal(7, "Set1")[c(2, 1, 4, 5, 3, 7)]
        if (split_vax) {
          hcl <- distinct(df, compartment, vax) %>%
            arrange(vax) %>%
            mutate(color = rep(cols, 2))
        } else {
          hcl <- distinct(df, compartment) %>%
            mutate(color = cols[seq_len(n())])
        }
      } else {
        simex[[what]] <- simex[[what]][compartment == show_compartment]
        ## Single compartment (show_compartment) by age; continuous age palette
        df <- extract(
          simex, what,
          cri = show_ribbon, cri_alpha,
          stratify_by = c("time", "age", "compartment")
        )
        ages <- unique(df$age)
        n_age <- length(ages)
        age_cols <- grDevices::colorRampPalette(
          c("#89A236", "#277455", "#323776")
        )(n_age)
        age_order <- order(as.numeric(gsub("age_", "", ages)))
        hcl <- tibble(age = ages[age_order], color = age_cols)
      }

      ## define baseline chart
      hc <- highchart() %>%
        hc_chart(
          type = "line",
          backgroundColor = "#FFFFFF"
        )

      js_code1 <- "
        function() {
          if (typeof x === 'undefined') {
              var recalc_legend = true;
          }
          let chart = this;
            recalc_legend = false;
            console.log(chart);
            chart.update({
              legend: {
                itemWidth: chart.plotSizeX * 0.8 / 5.8,
                symbolWidth: chart.plotSizeX * 0.8 / 12
              }
            });
        }"

      js_code2 <- "
        function() {
          let chart = this;
          if (recalc_legend) {
            console.log(chart);
            chart.update({
              legend: {
                itemWidth: chart.plotSizeX * 0.8 / 5.8,
                symbolWidth: chart.plotSizeX * 0.8 / 12
              }
            });
          }
        }"

      if (stratify_by == "compartment") {
        ## Add one series per compartment (and per vax if split_vax)
        for (i in seq_len(nrow(hcl))) {
          if (split_vax) {
            hc <- hc %>%
              hc_add_series(
                name = HTML(paste0(
                  hcl$compartment[i], "<sub>",
                  ifelse(hcl$vax[i], "v", "u"), "</sub>"
                )),
                id = paste0(hcl$compartment[i], "_", ifelse(hcl$vax[i], "v", "u")),
                data = df %>%
                  filter(vax == hcl$vax[i], compartment == hcl$compartment[i]),
                "line", hcaes(x = time, y = value),
                dashStyle = ifelse(!hcl$vax[i], "Solid", "ShortDash"),
                color = hcl$color[i]
              )
            if (show_ribbon) {
              hc <- hc %>%
                hc_add_series(
                  name = HTML(paste0(
                    hcl$compartment[i], "<sub>",
                    ifelse(hcl$vax[i], "v", "u"), "</sub>"
                  )),
                  linkedTo = paste0(
                    hcl$compartment[i], "_", ifelse(hcl$vax[i], "v", "u")
                  ),
                  data = df %>%
                    filter(vax == hcl$vax[i], compartment == hcl$compartment[i]) %>%
                    transmute(time = time, low = lower, high = upper),
                  type = "arearange",
                  hcaes(x = time, low = low, high = high),
                  color = hcl$color[i],
                  fillOpacity = 0.2,
                  lineWidth = 0,
                  marker = list(enabled = FALSE)
                )
            }
          } else {
            comp <- hcl$compartment[i]
            hc <- hc %>%
              hc_add_series(
                name = as.character(comp),
                id = as.character(comp),
                data = df %>% filter(compartment == comp),
                "line", hcaes(x = time, y = value),
                color = hcl$color[i]
              )
            if (show_ribbon && "lower" %in% names(df)) {
              hc <- hc %>%
                hc_add_series(
                  name = as.character(comp),
                  linkedTo = as.character(comp),
                  data = df %>%
                    filter(compartment == comp) %>%
                    transmute(time = time, low = lower, high = upper),
                  type = "arearange",
                  hcaes(x = time, low = low, high = high),
                  color = hcl$color[i],
                  fillOpacity = 0.2,
                  lineWidth = 0,
                  marker = list(enabled = FALSE)
                )
            }
          }
        }
      } else {
        ## Add one series per age
        for (i in seq_len(nrow(hcl))) {
          ag <- hcl$age[i]
          hc <- hc %>%
            hc_add_series(
              name = as.character(ag),
              id = as.character(ag),
              data = df %>% filter(age == ag),
              "line", hcaes(x = time, y = value),
              color = hcl$color[i]
            )
          if (show_ribbon && "lower" %in% names(df)) {
            hc <- hc %>%
              hc_add_series(
                name = as.character(ag),
                linkedTo = as.character(ag),
                data = df %>%
                  filter(age == ag) %>%
                  transmute(time = time, low = lower, high = upper),
                type = "arearange",
                hcaes(x = time, low = low, high = high),
                color = hcl$color[i],
                fillOpacity = 0.2,
                lineWidth = 0,
                marker = list(enabled = FALSE)
              )
          }
        }
      }

      ## Export buttons: toggle u/v only when colouring by compartment
      export_buttons <- list(
        contextButton = list(
          align = "left",
          verticalAlign = "top",
          x = 0,
          y = -10,
          menuItems = list(
            "viewFullscreen",
            "downloadJPEG",
            "downloadPDF",
            "downloadCSV"
          )
        )
      )
      if (stratify_by == "compartment" && split_vax) {
        export_buttons$customButton <- list(
          text = "Toggle Unvaccinated",
          onclick = JS(
            "function() {
            recalc_legend = false;
            var seriesIDs = ['S_u', 'E_u', 'C_u', 'H_u', 'R_u', 'D_u'];
            seriesIDs.forEach(function(seriesID) {
              var series = this.get(seriesID);
              if (series.visible) {
                series.hide();
              } else {
                series.show();
              }
            }.bind(this));
            recalc_legend = true;
            }"
          ),
          align = "right",
          verticalAlign = "bottom",
          x = 0,
          y = 0
        )
        export_buttons$customButton2 <- list(
          text = "Toggle Vaccinated",
          onclick = JS(
            "function() {
            recalc_legend = false;
            var seriesIDs = ['S_v', 'E_v', 'C_v', 'H_v', 'R_v', 'D_v'];
            seriesIDs.forEach(function(seriesID) {
              var series = this.get(seriesID);
              if (series.visible) {
                series.hide();
              } else {
                series.show();
              }
            }.bind(this));
            recalc_legend = true;
          }"
          ),
          align = "right",
          verticalAlign = "bottom",
          x = 0,
          y = -30
        )
      }

      ## yAxis: add hospital capacity line when showing H by age
      yaxis_plotLines <- NULL
      if (stratify_by == "age" &&
        show_compartment == "H" &&
        what == "prevalence" &&
        show_hosp_capacity) {
        yaxis_plotLines <- list(
          list(
            value = hosp_capacity,
            color = "grey",
            width = 1.5,
            zIndex = 1
          )
        )
      }

      ## add further options
      hc %>%
        hc_legend(
          align = "center",
          width = "80%",
          useHTML = TRUE
        ) %>%
        hc_chart(
          type = "line",
          events = list(load = JS(js_code1), render = JS(js_code2))
        ) %>%
        hc_yAxis(
          title = list(
            text = ifelse(use_absolute_numbers, "Daily count", "Proportion")
          ),
          labels = list(format = "{value}"),
          min = 0,
          plotLines = yaxis_plotLines
        ) %>%
        hc_xAxis(title = list(text = "Day")) %>%
        hc_boost(enabled = TRUE) %>%
        hc_plotOptions(
          line = list(
            lineWidth = 5,
            marker = list(enabled = FALSE)
          ),
          events = list(
            legendItemClick = JS(
              "function (event) {
            recalc_legend = false;
                var series = this;
                if (series.visible) {
                    series.hide();
                } else {
                    series.show();
                }
            recalc_legend = true;
             }"
            )
          )
        ) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_exporting(enabled = TRUE, buttons = export_buttons)
    } else {
      df <- extract(
        simex, what,
        cri = show_ribbon, cri_alpha,
        stratify_by = c("time", "vax", "compartment")
      )

      ## define horizontal line for hospital capacity if needed
      hline <-
        if (what == "prevalence" && show_hosp_capacity) {
          geom_hline(
            data = tibble(
              compartment = factor("H", levels(df$compartment)),
              y = hosp_capacity
            ),
            aes(yintercept = y),
            color = "grey",
            linewidth = 1.5
          )
        } else {
          NULL
        }

      ## plot
      df %>%
        ggplot(aes(time, if (use_absolute_numbers) value else value / pop, linetype = vax)) +
        hline +
        geom_line(linewidth = 1.5) +
        facet_wrap(
          ~compartment,
          scales = ifelse(freescales, "free_y", "fixed"),
          labeller = labeller(compartment = cat)
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0.01, 0.05)),
          trans = ifelse(log, "log10", "identity"),
          labels = if (use_absolute_numbers) waiver() else scales::percent
        ) +
        scale_linetype(name = "Vaccinated") +
        labs(x = "Day", y = tools::toTitleCase(what)) +
        theme_minimal(base_size = base_size) +
        theme(
          legend.position = "bottom",
          plot.background = element_rect(fill = "white", color = "white")
        )
    }
  } else if (format == "summary") {
    ## Summary = sum across time of incidence only; 95% CRI when particles > 1
    inc <- simex[["incidence"]]
    n_particles <- 1L
    if (!is.null(inc)) {
      if (inherits(inc, "data.frame") && "particle" %in% names(inc)) {
        n_particles <- length(unique(inc[["particle"]]))
      } else if (is.array(inc)) {
        dns <- dimnames(inc)
        if (!is.null(dns) && "particle" %in% names(dns)) {
          n_particles <- length(dns[["particle"]])
        }
      }
    }
    if (type == "highchart") {
      if (stratify_by == "compartment") {
        strat_comp <- if (split_vax) c("vax", "compartment") else c("compartment")
        df <- extract(
          simex, "incidence",
          cri = TRUE, cri_alpha = 0.95,
          stratify_by = strat_comp
        )
        df <- as.data.frame(df)
        if (!use_absolute_numbers) {
          df$value <- df$value / pop
          if ("lower" %in% names(df)) {
            df$lower <- df$lower / pop
            df$upper <- df$upper / pop
          }
        }
        cols <- RColorBrewer::brewer.pal(7, "Set1")[c(2, 1, 4, 5, 3, 7)]
        has_cri <- n_particles > 1L &&
          "lower" %in% names(df) &&
          "upper" %in% names(df)
        if (split_vax) {
          comps <- sort(unique(df$compartment))
          hcl <- tibble(compartment = comps, color = cols[seq_along(comps)])
          df <- df %>% arrange(compartment)
          val_u <- setNames(rep(0, length(comps)), comps)
          val_v <- setNames(rep(0, length(comps)), comps)
          ru <- df %>% filter(!vax)
          rv <- df %>% filter(vax)
          if (nrow(ru)) val_u[ru$compartment] <- ru$value
          if (nrow(rv)) val_v[rv$compartment] <- rv$value
          val_u <- unname(val_u)
          val_v <- unname(val_v)
          hc <- highchart() %>%
            hc_chart(
              type = "column", backgroundColor = "#FFFFFF", animation = FALSE
            ) %>%
            hc_add_series(
              data = val_u,
              type = "column",
              id = "col_unvax",
              name = "Unvaccinated",
              colorByPoint = TRUE,
              colors = hcl$color
            ) %>%
            hc_add_series(
              data = val_v,
              type = "column",
              id = "col_vax",
              name = "Vaccinated",
              colorByPoint = TRUE,
              colors = hcl$color,
              opacity = 0.7,
              borderWidth = 1.5,
              borderColor = "#333333",
              dashStyle = "Dot"
            ) %>%
            hc_xAxis(categories = as.character(comps)) %>%
            hc_plotOptions(
              column = list(
                pointPadding = 0.2,
                borderWidth = 0,
                grouping = TRUE
              )
            )
          if (has_cri) {
            df_u <- df %>% filter(!vax)
            df_v <- df %>% filter(vax)
            err_u <- lapply(comps, function(c) {
              r <- df_u %>% filter(compartment == c)
              if (nrow(r)) list(low = r$lower[1], high = r$upper[1]) else list(low = 0, high = 0)
            })
            err_v <- lapply(comps, function(c) {
              r <- df_v %>% filter(compartment == c)
              if (nrow(r)) list(low = r$lower[1], high = r$upper[1]) else list(low = 0, high = 0)
            })
            hc <- hc %>%
              hc_add_series(
                data = err_u,
                type = "errorbar",
                linkedTo = "col_unvax",
                color = "black",
                stemWidth = 1,
                whiskerLength = 5
              ) %>%
              hc_add_series(
                data = err_v,
                type = "errorbar",
                linkedTo = "col_vax",
                color = "black",
                stemWidth = 1,
                whiskerLength = 5
              )
          }
        } else {
          comps <- sort(unique(df$compartment))
          hcl <- tibble(compartment = comps, color = cols[seq_along(comps)])
          df <- df %>% arrange(compartment)
          hc <- highchart() %>%
            hc_chart(type = "column", backgroundColor = "#FFFFFF", animation = FALSE) %>%
            hc_add_series(
              data = df$value,
              type = "column",
              id = "col_main",
              name = "Incidence",
              colorByPoint = TRUE,
              colors = hcl$color
            ) %>%
            hc_xAxis(categories = as.character(comps))
          if (has_cri) {
            err_data <- lapply(seq_len(nrow(df)), function(j) {
              list(low = df$lower[j], high = df$upper[j])
            })
            hc <- hc %>%
              hc_add_series(
                data = err_data,
                type = "errorbar",
                linkedTo = "col_main",
                color = "black",
                stemWidth = 1,
                whiskerLength = 5
              )
          }
        }
        hc <- hc %>%
          hc_yAxis(
            title = list(text = ifelse(use_absolute_numbers, "Count", "Proportion")),
            min = 0
          ) %>%
          hc_tooltip(valueDecimals = 0) %>%
          hc_legend(enabled = split_vax) %>%
          hc_exporting(enabled = TRUE)
      } else {
        df <- extract(
          simex, "incidence",
          cri = TRUE, cri_alpha = 0.95,
          stratify_by = c("age", "compartment")
        )
        df <- as.data.frame(df) %>%
          filter(compartment == show_compartment)
        if (!use_absolute_numbers) {
          df$value <- df$value / pop
          if ("lower" %in% names(df)) {
            df$lower <- df$lower / pop
            df$upper <- df$upper / pop
          }
        }
        ages <- df$age
        n_age <- nrow(df)
        age_order <- order(as.numeric(gsub("age_", "", ages)))
        df <- df[age_order, ]
        age_cols <- grDevices::colorRampPalette(
          c("#89A236", "#277455", "#323776")
        )(n_age)
        has_cri <- n_particles > 1L &&
          "lower" %in% names(df) &&
          "upper" %in% names(df)
        hc <- highchart() %>%
          hc_chart(type = "column", backgroundColor = "#FFFFFF", animation = FALSE) %>%
          hc_add_series(
            data = df$value,
            type = "column",
            id = "col_age",
            name = show_compartment,
            colorByPoint = TRUE,
            colors = age_cols
          ) %>%
          hc_xAxis(categories = as.character(df$age)) %>%
          hc_yAxis(
            title = list(text = ifelse(use_absolute_numbers, "Count", "Proportion")),
            min = 0
          ) %>%
          hc_tooltip(valueDecimals = 0) %>%
          hc_legend(enabled = FALSE) %>%
          hc_exporting(enabled = TRUE)
        if (has_cri) {
          err_data <- lapply(seq_len(nrow(df)), function(j) {
            list(low = df$lower[j], high = df$upper[j])
          })
          hc <- hc %>%
            hc_add_series(
              data = err_data,
              type = "errorbar",
              linkedTo = "col_age",
              color = "black",
              stemWidth = 1,
              whiskerLength = 5
            )
        }
      }
      return(hc)
    } else {
      ## ggplot summary: sum over time of incidence by age, with CRI
      df_sum <- extract(
        simex, "incidence",
        cri = TRUE, cri_alpha = 0.95,
        stratify_by = c("age", "compartment")
      )
      df_sum <- as.data.frame(df_sum) %>%
        filter(compartment == show_compartment)
      if (!use_absolute_numbers) {
        df_sum$value <- df_sum$value / pop
        if ("lower" %in% names(df_sum)) {
          df_sum$lower <- df_sum$lower / pop
          df_sum$upper <- df_sum$upper / pop
        }
      }
      p <- df_sum %>%
        ggplot(aes(age, value)) +
        geom_col() +
        scale_y_continuous(
          expand = expansion(mult = c(0.01, 0.05)),
          labels = if (use_absolute_numbers) waiver() else \(x) scales::percent(x, 0.001)
        ) +
        labs(
          x = "Age",
          y = ifelse(use_absolute_numbers, "Count", "Proportion")
        ) +
        theme_minimal(base_size = base_size) +
        theme(plot.background = element_rect(fill = "white", color = "white"))
      if (n_particles > 1L &&
        "lower" %in% names(df_sum) &&
        "upper" %in% names(df_sum)) {
        p <- p + geom_errorbar(
          aes(ymin = lower, ymax = upper),
          width = 0.2,
          linewidth = 0.5,
          color = "black"
        )
      }
      return(p)
    }
  }
}
