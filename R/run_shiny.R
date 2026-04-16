#' This function launches the simex Shiny app .
#'
#' @importFrom shinyMatrix matrixInput
#' @importFrom shinyjs click disable enable useShinyjs
#' @importFrom shiny icon showNotification
#' @importFrom shinyWidgets setBackgroundColor switchInput radioGroupButtons
#' @importFrom waiter waiter_preloader spin_3
#' @importFrom stringr str_to_title
#'
#' @author Finlay Campbell
#'
#' @export
#'
run_shiny <- function() {
  ## define labels
  labs <- c(
    iso3 = "Country code",
    population = "Population",
    init_infections = "Number of initial infections",
    R0 = "Basic reproduction number",
    generation_time = "Generation time (days)",
    incubation_period = "Incubation period (days)",
    infectiousness_presymp = "Presymptomatic infectiousness (relative to symptomatic)",
    frac_symp = "Proportion of cases that eventually develop symptoms (%)",
    agestrat = "Age-stratified parameters",
    ifr = "Infection fatality rate (%)",
    hosp_mortality = "Proportion of hospitalisations that die (%)",
    hosp_protection_death = "Proportion of deaths averted by hospitalisation (%)",
    hosp_duration = "Duration of hospitalisation (days)",
    hosp_capacity = "Hospital capacity (per 100k population)",
    comm_mortality = "Proportions of community infections that die (%)",
    vax_rate = "Vaccination rate (doses per day)",
    vax = "Vaccination protection",
    vax_infectiousness = "Infectiousness",
    vax_infection = "Infection",
    vax_hosp = "Hospitalisation",
    vax_death = "Death",
    isolation_adherence = "Proportion adherence to isolation guidlines (%)",
    isolation_effectiveness = "Proportion of contacts reducted by isolation (%)",
    isolation_delay = "Delay from symptom onset to isolation (days)",
    social_distancing = "Social Distancing",
    vax_prioritised = "Are elderly individuals vaccinated first?",
    hosp_prioritised = paste(
      "Are elderly individuals given priority",
      "when hospital bed capacity is exceeded?"
    )
  )

  ## get default values for parameters
  simex_defaults <- map(formals(get_parameters), get_default, "value")

  ## define age-stratified varnames that get grouped into matrix input
  agestrat_nms <- c("ifr", "hosp_mortality", "hosp_duration", "comm_mortality")

  ## define vaccination varnames that get grouped into matrix input
  vax_nms <- c("vax_infectiousness", "vax_infection", "vax_hosp", "vax_death")

  ## variable names expressed in percent
  percent_nms <- c(
    vax_nms, "ifr", "hosp_mortality", "frac_symp",
    "hosp_protection_death", "comm_mortality",
    "isolation_adherence", "isolation_effectiveness"
  )

  ## Last column of shiny_to_simex() matrix = most recent period's pars (avoid
  ## dplyr::last(matrix), which slices by row and breaks simex_to_shiny).
  last_period_pars <- function(par_matrix) {
    if (is.matrix(par_matrix)) {
      return(par_matrix[[ncol(par_matrix)]])
    }
    par_matrix
  }

  ## generate shiny output for a given parameter tab
  simex_to_shiny <- function(simex_input, tab_id) {
    ## keep only variables in default simex input
    simex_input[setdiff(names(simex_input), names(simex_defaults))] <- NULL

    ## adjust units
    for (i in percent_nms) simex_input[[i]] <- simex_input[[i]] * 100
    simex_input$hosp_capacity <- simex_input$hosp_capacity * 1e5

    ## collapse age-stratified parameters into one matrix
    agestrat <- lengths(simex_input) == nrow(cdat[[1]]$pop)
    simex_input$agestrat <- do.call(cbind, simex_input[agestrat])
    dimnames(simex_input$agestrat) <- list(
      "Age" = get_age_cat(),
      "Variable" = labs[colnames(simex_input$agestrat)]
    )
    simex_input[agestrat] <- NULL

    ## convert social distancing to matrix for matrix input
    simex_input$social_distancing <- matrix(
      simex_input$social_distancing,
      nrow = 1,
      dimnames = list("Reduction (%)", str_to_title(names(simex_input$social_distancing)))
    )

    ## convert vax parameters to matrix for matrix input
    simex_input$vax <- matrix(
      unlist(simex_input[vax_nms]),
      nrow = 1,
      dimnames = list("Protection (%)", labs[vax_nms])
    )
    simex_input[vax_nms] <- NULL

    ## re-order
    simex_input <- simex_input[order(match(names(simex_input), names(labs)))]

    ## send values to input maker
    shiny_output <- imap(simex_input, make_input, tab_id)

    return(shiny_output)
  }

  ## shape shiny parameter to fit simex model input
  shiny_to_simex <- function(input, active_par) {
    ## so we have a modifieable list
    pars <- reactiveValuesToList(input)
    pars <- pars[grepl("__", names(pars))]

    ## get parameter set
    set <- map_chr(strsplit(names(pars), "__"), pluck, 1)

    ## split by set
    pars <- split(pars, set)

    ## only keep active ones
    pars <- pars[active_par]

    ## extract day
    days <- map_dbl(pars, \(par) par[[grep("day", names(par))]])

    out <- map(
      setNames(pars, days),
      function(par) {
        names(par) <- map_chr(strsplit(names(par), "__"), pluck, 2)

        ## reshape social distancing and agestrat to useable formats
        par$social_distancing <- setNames(
          as.numeric(unlist(par$social_distancing)),
          tolower(colnames(par$social_distancing))
        )

        ## get age-stratified parameters
        agestrat <- setNames(
          map(data.frame(par$agestrat), as.numeric), agestrat_nms
        )
        par$agestrat <- NULL
        par <- c(par, agestrat)

        ## get vax parameters
        vax <- setNames(map(data.frame(par$vax), as.numeric), vax_nms)
        par$vax <- NULL
        par <- c(par, vax)

        ## adjust units
        for (i in percent_nms) par[[i]] <- par[[i]] / 100
        par$hosp_capacity <- par$hosp_capacity / 1e5

        ## remove input values that are not arguments of get_parameters
        par[setdiff(names(par), names(simex_defaults))] <- NULL

        ## pass to get parameters
        do.call(get_parameters, par)
      }
    )

    ## order by day and return as matrix
    out <- out[order(as.numeric(names(out)))]
    out <- t(as.matrix(out))
  }

  ## function for extracting active values of a parameter
  extract_active_par <- function(input, name, active_par) {
    x <- unlist(reactiveValuesToList(input)[grep(name, names(input))])
    x <- x[grep(paste(active_par(), collapse = "|"), names(x))]
    return(x)
  }

  ## rename function
  rn <- function(name, i) paste0(i, "__", name)

  get_tabname <- function(n) {
    x <- if (n <= 26) {
      LETTERS[n]
    } else {
      apply(expand.grid(LETTERS, LETTERS), 1, paste0, collapse = "")[n]
    }
    paste("Period", x)
  }

  ## function for generating an input panel for one panel ID
  make_input <- function(value, name, id) {
    if (name == "iso3") {
      selectInput(rn(name, id), labs[name], names(cdat), selected = value)
    } ## else if(name == "hosp_capacity") numericInput(rn(name, id), labs[name], value*1e5)
    else if (is.numeric(value)) {
      if (is.matrix(value)) {
        matrixInput(rn(name, id), labs[name], value)
      } else if (!is.null(names(value))) {
        do.call(
          fluidRow,
          unname(imap(value, ~ column(2, numericInput(rn(.y, id), labs[.y], .x))))
        )
      } else if (length(value) == 1) numericInput(rn(name, id), labs[name], value)
    } else if (is.logical(value)) {
      checkboxInput(rn(name, id), labs[name], value)
    }
  }

  ## define color palette
  background_col <- "#ffffff"
  sidebar_col <- "#fefae0"

  # Define the UI
  ui <- page_sidebar(
    useShinyjs(),
    setBackgroundColor(background_col),
    tags$head(tags$style(HTML(paste(
      paste0("#sidebar{background-color:", sidebar_col, "}"),

      ## Run / period / reset: same edge as shinyMatrix cells (1px gray = #808080);
      ## Bootstrap --bs-border-color (#dee2e6) is lighter than those inputs
      paste0(
        "#run_scenario,#add_period,#remove_period,#reset{",
        "background-color:#fff!important;color:#212529!important;",
        "border:1px solid #808080!important;",
        "min-height:2.65rem!important;padding:0.5rem 0.75rem!important;}",
        "#run_scenario:hover,#run_scenario:active,#run_scenario:focus-visible,",
        "#add_period:hover,#add_period:active,#add_period:focus-visible,",
        "#remove_period:hover,#remove_period:active,#remove_period:focus-visible,",
        "#reset:hover,#reset:active,#reset:focus-visible{",
        "background-color:#e9ecef!important;color:#000!important;",
        "border-color:#808080!important;}"
      ),

      ## Default shinyWidgets toggles: explicit white on unselected options
      paste0(
        "#main_plots_nav .radio-group-buttons button.radiobtn:not(.active){",
        "background-color:#fff!important;}"
      ),
      "#main_plots_nav .nav { display: flex; width: 100%; }",
      "#main_plots_nav .nav-item { flex: 1; min-width: 0; }",
      "#main_plots_nav .nav-link { text-align: center; }",
      ".csv-upload-dropzone{border:2px dashed #c5c5c5;border-radius:10px;padding:28px 20px;",
      "background:#faf9f6;min-height:220px;margin-top:8px;text-align:center;}",
      ".csv-upload-dropzone .shiny-input-container{width:100%;max-width:520px;margin:16px auto 0;}",
      ".csv-upload-dropzone .input-group{margin:auto;}"
    )))),

    ### next two lines for class - use class attribute (.inline instead of #inline)
    tags$head(
      tags$style(
        type = "text/css",
        ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } .inline .form-group { display: table-row;} p.indent {margin-right: 10px}"
      )
    ),
    title = h4(strong(
      em("simex:"),
      "simulating outbreaks and public health interventions"
    )),
    sidebar = sidebar(
      width = "40%",
      navset_card_underline(
        nav_panel(
          title = "Introduction",
          strong("Introduction to simex"),
          p("simex is a simulation excercise tool for epidemic and pandemic preparedness that lets you explore different outbreak scenarios and the effect of different public health interventions. The text below will give a brief overview of how to use the tool."),
          strong("Running a default scenario"),
          p("A typical workflow: upload observed incidence on ", em("Timeline"), " or ", em("Summary"), " (drag and drop or browse), with ", em("Incidence"), " selected on Timeline to explore the data. Adjust parameters under ", em("Parameters"), ", then use ", em("Run Scenario"), " to run the model, store the result under the selected scenario name, and compare it with the data on the Timeline. ", em("Summary"), " compares saved scenarios. The compartments are as follows:"),
          htmlOutput("compartments_description"),
          p("The subscript u and v correspond to unvaccinated and vaccinated populations, respectively. You can toggle whether you want to see prevalence or incidence using the buttons above the plot, and you can toggle whether you want to hide/show the vaccinated and unvaccinated populations using the buttons in the bottom right."),
          strong("Running a custom scenario"),
          p("To run a scenario under your own parameter settings, simply change any of the values in the parameter tab as you desire and press ", em("Run Scenario"), " again and the plot on the right will update."),
          strong("Changing parameter values over time"),
          p("The settings so far run a single set of parameters over the entire time period. To simulate a scenario where the parameters change at a given point in the pandemic, use the ", em("Add Period"), "button. This will add a new set of parameters, associated with a given start day. Try adding a new period and changing the reproduction number from 3 to 5 with a start day of 75. Then run the scenario and see how the increased reproduction number (e.g. due to the introduction of a new variant) changes the course of the pandemic. You can add as many periods as you would like, and can navigate between the parameter settings of each period by clicking on the respective tabs. You can also delete a period using the ", em("Remove Period"), "button."),
          strong("Comparing different scenarios"),
          p("Use ", em("Run Scenario"), " to add a simulation or overwrite one with the same name (each call re-runs the model with current parameters). Under ", em("Scenario"), ", pick a saved run or type a new name; remove a saved run with the ", em("\u00d7"), " button. ", em("Timeline"), " shows one outcome at a time; with ", em("Incidence"), ", uploaded data can be shown alone or with saved models. CSV columns: ", strong("time"), ", ", strong("age"), ", ", strong("compartment"), " (E, H, or D), ", strong("value"), ".")
        ),
        nav_panel(
          title = "Parameters",
          tags$label(`for` = "scenario_select", class = "control-label", "Scenario"),
          div(
            style = "display: flex; flex-direction: row; align-items: center; gap: 10px;",
            div(
              style = "flex: 1; min-width: 0;",
              selectizeInput(
                inputId = "scenario_select",
                label = NULL,
                choices = character(0),
                selected = "Default",
                options = list(
                  placeholder = "Select saved or type a new name",
                  create = TRUE,
                  createOnBlur = TRUE
                )
              )
            ),
            actionButton(
              inputId = "remove_saved_scenario",
              label = NULL,
              icon = icon("times"),
              title = "Remove selected scenario",
              class = "btn-danger",
              style = "padding: 6px 12px; flex-shrink: 0;"
            )
          ),
          div(
            style = "display: flex; align-items: stretch; margin-top: 0px",
            actionButton(
              "run_scenario", "Run Scenario",
              width = "100%",
              style = "margin-right:2px; margin-left: 2px"
            )
          ),
          div(
            style = "display: flex; align-items: stretch; margin-top: -20px",
            actionButton(
              "add_period", "Add Period",
              width = "33%",
              style = "margin-right:2px; margin-left: 2px"
            ),
            actionButton(
              "remove_period", "Remove Period",
              width = "33%",
              style = "margin-right:2px; margin-left: 2px"
            ),
            actionButton(
              "reset", "Reset",
              width = "33%",
              style = "margin-right:2px; margin-left: 2px"
            )
          ),
          div(
            style = "margin-top: 8px",
            numericInput(
              "n_particles",
              "Number of simulations",
              value = 50L,
              min = 1L,
              step = 1L
            )
          ),
          navset_card_underline(id = "parameters_panel")
        )
      )
    ),
    navset_card_underline(
      id = "main_plots_nav",
      nav_panel(
        title = "Timeline",
        fluidRow(
          column(
            width = 12,
            div(
              style = "display: inline-block; vertical-align: top; margin-right: 12px;",
              radioGroupButtons(
                inputId = "ehd_timeline_outcome",
                selected = "Cases",
                label = NULL,
                choices = c("Cases", "Hospitalisations", "Deaths"),
                size = "sm"
              )
            ),
            div(
              style = "display: inline-block; vertical-align: top; margin-right: 12px;",
              radioGroupButtons(
                inputId = "ehd_timeline_what",
                selected = "Incidence",
                label = NULL,
                choices = c("Incidence", "Prevalence"),
                size = "sm"
              )
            ),
            div(
              style = "display: inline-block; vertical-align: middle; margin-right: 12px;",
              shinyWidgets::materialSwitch(
                inputId = "ehd_stratify_age",
                label = "Stratify by age",
                value = FALSE,
                status = "primary"
              )
            )
          )
        ),
        uiOutput("ehd_plot_container")
      ),
      nav_panel(
        title = "Summary",
        radioGroupButtons(
          inputId = "summary_what",
          selected = "Cases",
          label = NULL,
          choices = c("Cases", "Hospitalisations", "Deaths")
        ),
        uiOutput("summary_plot_container")
      ),
    ),

    # start up loading spinner
    waiter::waiter_preloader(
      html = tagList(
        tags$img(
          src = "simex/img/collaboratory_logo.jpg",
          width = 600,
          style = "padding: 20px;"
        ),
        tags$br(),
        waiter::spin_3()
      ),
      color = "#FFFFFF"
    )
  )


  ## FRAC SYMP DOESNT SEEM TO BE WORKING? ##
  ## Maybe something odd with hospital admissions when shortening gentime?

  ## Variant escape from immunity?
  ## Waning immunity / birth/death?

  ## Define the server
  server <- function(input, output, session) {
    ## Run model from current inputs (used by Run Scenario)
    run_model_from_inputs <- function() {
      if (!any(grepl("agestrat", names(input)))) {
        parlist <- get_parameters()
      } else {
        parlist <- shiny_to_simex(input, active_par())
        start_days <- as.numeric(colnames(parlist))
        if (!any(start_days == 1)) {
          showNotification("One period must start on day 1!", type = "warning")
          return(NULL)
        }
        if (!all(table(start_days) == 1)) {
          showNotification("Periods can't have the same start day!", type = "warning")
          return(NULL)
        }
      }
      run_simex(parlist, n_particles = as.integer(input$n_particles))
    }

    ## saved models (named list; never NULL so length() is safe)
    scenarios <- reactiveVal(list())

    ## Single string from scenario selectize (handles NULL / length-0)
    scenario_sel_str <- function(x) {
      if (is.null(x) || length(x) == 0L) {
        return("")
      }
      trimws(as.character(x[[1L]]))
    }

    ## number and names of active parameters
    n_par <- reactiveVal(0)
    active_par <- reactiveVal()

    ## total number of parameters
    total_par <- reactiveVal(0)

    ## initiate server with clicking add_period button
    o <- observe({
      click("add_period")
      o$destroy()
    })

    observeEvent(
      input$add_period,
      {
        ## add new id and update number of parameters
        active_par(c(active_par(), paste0(sample(letters, 10, TRUE), collapse = "")))
        n_par(n_par() + 1)
        total_par(total_par() + 1)

        ## define active max day for default new day value
        days <- extract_active_par(input, "day", active_par)
        start_day <- if (length(days) == 0) 1 else max(days) + 50

        ## insert new parameters tab
        nav_insert(
          id = "parameters_panel",
          nav_panel(
            title = get_tabname(total_par()),
            value = last(active_par()),
            do.call(
              div,
              list(
                style = "margin-left: 10px; margin-right: 10px",
                headerPanel(""),
                numericInput(rn("day", last(active_par())), "Start day", start_day),
                ## generate shiny UI for newest tab, taking defaults from most recent tab
                simex_to_shiny(
                  ## if no tabs existent yet, use defaults simex values
                  if (length(days) == 0) {
                    simex_defaults
                  } ## otherwise use most recent tab (have to remove last active tab
                  ## because it hasn't been initialised yet)
                  else {
                    last_period_pars(shiny_to_simex(input, head(active_par(), -1)))
                  },
                  ## assign tab ID
                  tab_id = last(active_par())
                )
              )
            )
          )
        )

        nav_select(
          id = "parameters_panel",
          select = last(active_par()),
          session = session
        )
      }
    )

    ## remove a tab
    observeEvent(input$remove_period, {
      if (n_par() != 1) {
        ## remove parameter set
        active_par(setdiff(active_par(), input$parameters_panel))
        n_par(n_par() - 1)
        nav_remove(id = "parameters_panel", target = input$parameters_panel)
        nav_select(
          id = "parameters_panel",
          select = last(active_par()),
          session = session
        )
      }
    })

    ## Run scenario: fresh run_simex; store under selected name (overwrites same name)
    observeEvent(input$run_scenario, {
      out <- run_model_from_inputs()
      if (is.null(out)) {
        return()
      }
      nm <- scenario_sel_str(input$scenario_select)
      if (!nzchar(nm)) {
        nm <- if (length(scenarios()) == 0L) {
          "Default"
        } else {
          paste0("Saved_", length(scenarios()) + 1L)
        }
      }
      cur <- scenarios()
      cur[[nm]] <- out
      scenarios(cur)
    })

    ## Drop selected scenario from the saved list
    observeEvent(input$remove_saved_scenario, {
      nm <- scenario_sel_str(input$scenario_select)
      if (!nzchar(nm)) {
        showNotification("Select a scenario to remove.", type = "warning")
        return()
      }
      cur <- scenarios()
      if (!nm %in% names(cur)) {
        return()
      }
      cur[[nm]] <- NULL
      scenarios(cur)
    })

    ## Scenario select: saved names plus optional typed (not-yet-saved) name
    observe({
      nms <- names(scenarios())
      sel <- scenario_sel_str(isolate(input$scenario_select))
      if (length(nms) == 0L) {
        updateSelectizeInput(
          session,
          "scenario_select",
          choices = character(0),
          selected = "Default",
          server = TRUE
        )
      } else {
        new_sel <- if (nzchar(sel) && sel %in% nms) {
          sel
        } else if (nzchar(sel) && !sel %in% nms) {
          sel
        } else {
          nms[[length(nms)]]
        }
        ch <- unique(c(nms, if (nzchar(new_sel) && !new_sel %in% nms) new_sel))
        ch <- ch[vapply(ch, nzchar, logical(1))]
        updateSelectizeInput(
          session,
          "scenario_select",
          choices = stats::setNames(ch, ch),
          selected = new_sel,
          server = TRUE
        )
      }
    })

    ## Remove only when the current value is a saved scenario name
    observe({
      nms <- names(scenarios())
      nm <- scenario_sel_str(input$scenario_select)
      if (length(nms) == 0L || !nzchar(nm) || !nm %in% nms) {
        disable("remove_saved_scenario")
      } else {
        enable("remove_saved_scenario")
      }
    })

    ## reset
    observeEvent(
      input$reset,
      session$reload()
    )

    ## Parsed incidence table (persists when Timeline file input is not in DOM)
    timeline_data_rv <- reactiveVal(NULL)

    read_timeline_csv <- function(fi) {
      if (is.null(fi)) {
        return(NULL)
      }
      df <- tryCatch(
        utils::read.csv(fi$datapath, check.names = FALSE),
        error = function(e) {
          showNotification("Could not read that CSV.", type = "warning")
          NULL
        }
      )
      if (is.null(df)) {
        return(NULL)
      }
      need_cols <- c("time", "age", "compartment", "value")
      if (!all(need_cols %in% names(df))) {
        showNotification(
          "CSV needs columns: time, age, compartment, value.",
          type = "warning"
        )
        return(NULL)
      }
      df[, need_cols, drop = FALSE]
    }

    observeEvent(input$fit_data_file,
      {
        df <- read_timeline_csv(input$fit_data_file)
        if (!is.null(df)) {
          timeline_data_rv(df)
        }
      },
      ignoreNULL = TRUE
    )

    observeEvent(input$fit_data_summary,
      {
        df <- read_timeline_csv(input$fit_data_summary)
        if (!is.null(df)) {
          timeline_data_rv(df)
        }
      },
      ignoreNULL = TRUE
    )

    timeline_obs_data <- reactive({
      timeline_data_rv()
    })

    ## Valid incidence table in memory (hide upload UI after first successful CSV)
    has_uploaded_data <- reactive({
      d <- timeline_data_rv()
      !is.null(d) &&
        is.data.frame(d) &&
        nrow(d) > 0L &&
        all(c("time", "age", "compartment", "value") %in% names(d))
    })

    ## Timeline: models and/or incidence data (data-only when no saved scenarios)
    ehd_plot_result <- reactive({
      d <- timeline_obs_data()
      has_cols <- !is.null(d) &&
        all(c("time", "age", "compartment", "value") %in% names(d))
      can_plot_data <- has_cols && tolower(input$ehd_timeline_what) == "incidence"
      has_models <- length(scenarios()) > 0L
      if (!has_models && !can_plot_data) {
        return(NULL)
      }
      cmpt <- c(
        Cases = "E",
        Hospitalisations = "H",
        Deaths = "D"
      )[[input$ehd_timeline_outcome]]
      plot_simex(
        scenarios(),
        mode = "timeline",
        renderer = "highcharter",
        what = tolower(input$ehd_timeline_what),
        compartments = cmpt,
        stratify_by_age = isTRUE(input$ehd_stratify_age),
        show_ribbon = TRUE,
        period_days = 7L,
        data = if (can_plot_data) d else NULL
      )
    })

    output$ehd_plot_container <- renderUI({
      res <- ehd_plot_result()
      if (is.null(res)) {
        if (!has_uploaded_data()) {
          return(tagList(
            div(
              class = "csv-upload-dropzone",
              p(style = "margin-bottom:10px;", strong("Upload Data")),
              fileInput(
                inputId = "fit_data_file",
                label = NULL,
                accept = c(".csv", "text/csv"),
                buttonLabel = "Browse...",
                width = "100%"
              )
            )
          ))
        }
        return(div())
      }
      if (inherits(res, "highchart")) {
        highchartOutput("ehd_plot_single", height = "100vh")
      } else {
        fluidRow(lapply(seq_along(res), function(i) {
          column(6, highchartOutput(paste0("ehd_plot_", i), height = "350px"))
        }))
      }
    })

    observe({
      res <- ehd_plot_result()
      if (is.null(res)) {
        return()
      }
      if (inherits(res, "highchart")) {
        output$ehd_plot_single <- renderHighchart(res)
      } else {
        for (i in seq_along(res)) {
          local({
            ii <- i
            output[[paste0("ehd_plot_", ii)]] <- renderHighchart(res[[ii]])
          })
        }
      }
    })

    ## Summary: upload only before first valid CSV; chart uses full viewport height
    output$summary_plot_container <- renderUI({
      has_scen <- length(scenarios()) > 0L
      if (!has_scen) {
        if (!has_uploaded_data()) {
          return(tagList(
            div(
              class = "csv-upload-dropzone",
              p(style = "margin-bottom:10px;", strong("Upload Data")),
              fileInput(
                inputId = "fit_data_summary",
                label = NULL,
                accept = c(".csv", "text/csv"),
                buttonLabel = "Browse...",
                width = "100%"
              )
            )
          ))
        }
        return(div())
      }
      highchartOutput("summary_endpoint", height = "100vh")
    })

    output$summary_endpoint <- renderHighchart({
      req(length(scenarios()) > 0L)
      plot_simex(
        scenarios(),
        mode = "endpoint",
        renderer = "highcharter",
        compartments = c(
          Cases = "E",
          Hospitalisations = "H",
          Deaths = "D"
        )[[input$summary_what]],
        show_ribbon = TRUE,
        period_days = 1L
      )
    })

    output$compartments_description <- renderText({
      format_html_list(c(
        "S: susceptible",
        "E: exposed but not symptomatic",
        "C: symptomatic in the community",
        "H: symptomatic in the hospital",
        "R: recovered",
        "D: dead"
      ))
    })
  }

  ## Run the Shiny app
  shinyApp(ui = ui, server = server)
}
