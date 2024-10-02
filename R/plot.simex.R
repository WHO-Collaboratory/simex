#' Plot timeline of prevalence or incidence.
#'
#' @param simex The solved system of equations as returned by \code{run_model}.
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
#' @param freescales Logical indicating whether the different comparments should
#'   use different y-axes.
#'
#' @param show_hosp_capacity Logical indicating whether hospital capacity should be
#'   displayed in the "H" compartment (only possible when what = "prevalence").
#'
#' @param use_absolute_numbers Logical indicating whether absolute absolute
#'   numbers (in terms of individuals) should be shown or relative to population
#'   size.
#'
#' @param show_compartment The compartment to show when using \code{format = "endpoint"}.
#'
#' @param base_size Base size passed to theme_*.
#'
#' @param type "ggplot" will return a static ggplot figure, "highchart" will
#'   return a dynamic figure.
#'
#' @author Finlay Campbell
#'
#' @importFrom stringr str_to_title
#'
#' @export
#'
plot.simex <- function(simex,
                       what = c("prevalence", "deltas", "incidence"),
                       format = c("timeline", "endpoint"),
                       log = FALSE,
                       freescales = TRUE,
                       show_hosp_capacity = TRUE,
                       use_absolute_numbers = FALSE,
                       show_compartment = c("D", "S", "E", "C", "H", "R"),
                       base_size = 11,
                       type = c("ggplot", "highchart")) {

  ## check arguments
  type <- match.arg(type)
  what <- match.arg(what)
  format <- match.arg(format)
  show_compartment <- match.arg(show_compartment)

  ## get population
  pop <- sum(simex$pars[[1]]$population)

  ## get hospital capacity
  hosp_capacity <- simex$pars[[1]]$hosp_capacity
  if(use_absolute_numbers) hosp_capacity <- hosp_capacity * pop

  ## category labels
  cat <- c(S = "Susceptible", E = "Exposed", C = "Community Infection",
           H = "Hospital Infection", R = "Recovered", D = "Dead")

  if(format == "timeline") {

    ## extract relevant data
    df <- extract(simex, what, stratify_by = c("day", "vax", "compartment"))

    ## define horizontal line for hospital capacity if needed
    hline <-
      if(what == "prevalence" & show_hosp_capacity)
        geom_hline(
          data = tibble(
            compartment = factor("H", levels(df$compartment)),
            y = hosp_capacity
          ),
          aes(yintercept = y),
          color = "grey",

          linewidth = 1.5
        )
      else NULL

    if(type == "highchart") {

      ## define different series and their colour
      cols <- RColorBrewer::brewer.pal(7, "Set1")[c(2, 1, 4, 5, 3, 7)]
      hcl <- distinct(df, compartment, vax) %>%
        mutate(color = rep(cols, 2))

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

      ## add these serie
      for(i in seq_len(nrow(hcl)))
        hc <- hc %>%
          hc_add_series(
            name = HTML(paste0(
              hcl$compartment[i], "<sub>",
              ifelse(hcl$vax[i], "v", "u"), "</sub>"
            )),
            id = paste0(hcl$compartment[i], "_", ifelse(hcl$vax[i], "v", "u")),
            data = df %>%
              filter(vax == hcl$vax[i], compartment == hcl$compartment[i]) %>%
              mutate(value = value*100),
            "line", hcaes(x = day, y = value),
            dashStyle = ifelse(!hcl$vax[i], "Solid", "ShortDash"),
            color = hcl$color[i]
          )

      ## add further optionsg
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
          title = list(text = "Proportion"),
          labels = list(format = "{value}%"),
          min = 0
        ) %>%
        hc_xAxis(title = list(text = "Day")) %>%
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
             }"))
        ) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "%") %>%
        hc_exporting(
          enabled = TRUE,
          buttons = list(
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
            ),
            customButton = list(
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
            }"),
            align = "right",
            verticalAlign = "bottom",
            x = 0,
            y = 0),
            customButton2 = list(
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
          }"),
          align = "right",
          verticalAlign = "bottom",
          x = 0,
          y = -30
          )
          )
        )

    } else {

      ## plot
      df %>%
        ggplot(aes(day, if(use_absolute_numbers) value*pop else value, linetype = vax)) +
        hline +
        geom_line(linewidth = 1.5) +
        facet_wrap(
          ~ compartment,
          scales = ifelse(freescales, "free_y", "fixed"),
          labeller = labeller(compartment = cat)
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0.01, 0.05)),
          trans = ifelse(log, "log10", "identity"),
          labels = if(use_absolute_numbers) waiver() else percent
        ) +
        scale_linetype(name = "Vaccinated") +
        labs(x = "Day", y = str_to_title(what)) +
        theme_minimal(base_size = base_size) +
        theme(
          legend.position = 'bottom',
          plot.background = element_rect(fill = "white", color = "white")
        )

    }

  } else if(format == "endpoint") {

    ## define age groupings
    groupings <- list(
      "0-14" = paste0("age_", 1:3),
      "15-34" = paste0("age_", 4:7),
      "35-64" = paste0("age_", 8:13),
      "65+" = paste0("age_", 14:16)
    )
    groupings <- unlist(unname(imap(groupings, ~ setNames(rep(.y, length(.x)), .x))))

    ## extract relevant data
    df <- extract(simex, what, stratify_by = c("day", "compartment", "age")) %>%
      filter(day == max(day) & compartment == show_compartment) %>%
      arrange(age) %>%
      mutate(
        age_frac = simex$pars[[1]]$age_frac[age],
        age = fct_inorder(groupings[as.character(age)])
      ) %>%
      group_by(age) %>%
      summarise(
        value = if(use_absolute_numbers) sum(value)*pop else sum(value)/sum(age_frac)
      )

    df %>%
      ggplot(aes(age, value)) +
      geom_col() +
      scale_x_discrete(labels = get_age_cat()) +
      scale_y_continuous(
        expand = expansion(mult = c(0.01, 0.05)),
        labels = if(use_absolute_numbers) waiver() else function(x) percent(x, 0.001)
      ) +
      labs(
        x = "Age category",
        y = ifelse(use_absolute_numbers, "Number of deaths", "Proportion of population that died")
      ) +
      theme_minimal(base_size = base_size) +
      theme(
        plot.background = element_rect(fill = "white", color = "white")
      )

  }

}
