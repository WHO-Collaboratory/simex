#' This function launches the simex Shiny app .
#'
#' @importFrom brochure brochureApp server_redirect
#' @importFrom cachem cache_mem
#' @importFrom shinyMatrix matrixInput
#' @importFrom shinyjs click disable enable hide show useShinyjs
#' @importFrom shiny icon showNotification
#' @importFrom shinyWidgets setBackgroundColor switchInput radioGroupButtons
#' @importFrom waiter waiter_preloader spin_3
#' @importFrom stringr str_to_title
#' @importFrom monty monty_dsl_distributions
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

  ## Turn get_parameters()-style argument lists into the same structure
  ## simex_to_shiny() uses (percents * 100, matrices, column order).
  args_to_simex_shiny_shape <- function(raw_args) {
    simex_input <- utils::modifyList(as.list(simex_defaults), as.list(raw_args))
    simex_input[setdiff(names(simex_input), names(simex_defaults))] <- NULL

    for (i in percent_nms) {
      if (i %in% names(simex_input)) {
        simex_input[[i]] <- simex_input[[i]] * 100
      }
    }
    if ("hosp_capacity" %in% names(simex_input)) {
      simex_input$hosp_capacity <- simex_input$hosp_capacity * 1e5
    }

    agestrat <- lengths(simex_input) == nrow(cdat[[1]]$pop)
    if (any(agestrat)) {
      simex_input$agestrat <- do.call(cbind, simex_input[agestrat])
      dimnames(simex_input$agestrat) <- list(
        "Age" = get_age_cat(),
        "Variable" = labs[colnames(simex_input$agestrat)]
      )
      simex_input[agestrat] <- NULL
    }

    simex_input$social_distancing <- matrix(
      simex_input$social_distancing,
      nrow = 1,
      dimnames = list(
        "Reduction (%)",
        str_to_title(names(simex_input$social_distancing))
      )
    )

    simex_input$vax <- matrix(
      unlist(simex_input[vax_nms]),
      nrow = 1,
      dimnames = list("Protection (%)", labs[vax_nms])
    )
    simex_input[vax_nms] <- NULL

    simex_input <- simex_input[order(match(names(simex_input), names(labs)))]
    simex_input
  }

  ## Map a simex_to_shiny section name to get_parameters / unpack argument names.
  scenario_section_to_argnames <- function(sec) {
    if (identical(sec, "agestrat")) {
      return(agestrat_nms)
    }
    if (identical(sec, "vax")) {
      return(vax_nms)
    }
    if (identical(sec, "social_distancing")) {
      return("social_distancing")
    }
    sec
  }

  ## Named choices for scen_section_select (values = section ids).
  scenario_section_choice_labels <- function(ids) {
    stats::setNames(
      ids,
      vapply(ids, function(s) {
        if (s %in% names(labs)) {
          z <- as.character(labs[[s]])[[1L]]
          if (!is.na(z) && nzchar(z)) {
            return(z)
          }
        }
        s
      }, character(1))
    )
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

  ## Single-tab Shiny inputs -> get_parameters() argument list (not expanded).
  shiny_to_get_parameters_args_from_tab <- function(input, tab_id) {
    pars <- reactiveValuesToList(input)
    prefix <- paste0("^", tab_id, "__")
    pars <- pars[grepl(prefix, names(pars))]
    if (length(pars) == 0L) {
      return(NULL)
    }
    par <- pars
    names(par) <- map_chr(strsplit(names(par), "__"), pluck, 2)

    ## Only expand grouped inputs when that widget is present (scenario tab may
    ## send a single section; empty matrixInput can yield length-0 values with
    ## non-zero colnames and break setNames()).
    if ("social_distancing" %in% names(par)) {
      sd <- par$social_distancing
      sd_vals <- as.numeric(unlist(sd))
      sd_nms <- tolower(colnames(sd))
      if (length(sd_vals) > 0L && length(sd_vals) == length(sd_nms)) {
        par$social_distancing <- setNames(sd_vals, sd_nms)
      } else {
        par$social_distancing <- NULL
      }
    }

    if ("agestrat" %in% names(par)) {
      ag_cols <- map(data.frame(par$agestrat), as.numeric)
      if (length(ag_cols) == length(agestrat_nms)) {
        agestrat <- setNames(ag_cols, agestrat_nms)
        par$agestrat <- NULL
        par <- c(par, agestrat)
      } else {
        par$agestrat <- NULL
      }
    }

    if ("vax" %in% names(par)) {
      vx_cols <- map(data.frame(par$vax), as.numeric)
      if (length(vx_cols) == length(vax_nms)) {
        vax <- setNames(vx_cols, vax_nms)
        par$vax <- NULL
        par <- c(par, vax)
      } else {
        par$vax <- NULL
      }
    }

    for (i in percent_nms) {
      if (i %in% names(par)) {
        par[[i]] <- par[[i]] / 100
      }
    }
    if ("hosp_capacity" %in% names(par)) {
      par$hosp_capacity <- par$hosp_capacity / 1e5
    }

    par[setdiff(names(par), names(simex_defaults))] <- NULL

    return(par)
  }

  ## Single-tab parameters for the Fitting panel (ids: fit__*).
  shiny_to_simex_single <- function(input, tab_id) {
    par <- shiny_to_get_parameters_args_from_tab(input, tab_id)
    if (is.null(par)) {
      return(NULL)
    }
    do.call(get_parameters, par)
  }

  ## Build get_settings() call from fitset__* inputs (snapshots/groups fixed).
  collect_fit_settings <- function(input) {
    defs <- map(formals(get_settings), get_default, "value")
    nm <- setdiff(names(defs), c("snapshots", "groups"))
    args <- list()
    for (f in nm) {
      id <- paste0("fitset__", f)
      v <- input[[id]]
      if (is.null(v)) {
        return(NULL)
      }
      if (is.logical(defs[[f]])) {
        args[[f]] <- isTRUE(v)
      } else {
        nv <- as.numeric(v)
        if (length(nv) != 1L || is.na(nv)) {
          return(NULL)
        }
        args[[f]] <- nv
      }
    }
    do.call(get_settings, args)
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
    ## Scenarios tab: section title is already in the Parameter dropdown — no label.
    input_lbl <- function(nm) {
      if (identical(id, "scen")) {
        return(NULL)
      }
      labs[[nm]]
    }
    if (name == "iso3") {
      selectInput(rn(name, id), input_lbl(name), names(cdat), selected = value)
    } ## else if(name == "hosp_capacity") numericInput(rn(name, id), labs[name], value*1e5)
    else if (is.numeric(value)) {
      if (is.matrix(value)) {
        matrixInput(rn(name, id), input_lbl(name), value)
      } else if (!is.null(names(value))) {
        do.call(
          fluidRow,
          unname(imap(value, ~ column(
            2,
            numericInput(rn(.y, id), input_lbl(.y), .x)
          )))
        )
      } else if (length(value) == 1) {
        numericInput(rn(name, id), input_lbl(name), value)
      }
    } else if (is.logical(value)) {
      checkboxInput(rn(name, id), input_lbl(name), value)
    }
  }

  ## define color palette
  background_col <- "#ffffff"
  sidebar_col <- "#fefae0"

  ## Monty prior distribution names and fit settings inputs (built once).
  prior_dist_names <- monty::monty_dsl_distributions()$name
  fit_settings_defaults <- map(formals(get_settings), get_default, "value")
  fit_setting_ids <- setdiff(names(fit_settings_defaults), c("snapshots", "groups"))
  fit_settings_ui <- tagList(lapply(fit_setting_ids, function(f) {
    val <- fit_settings_defaults[[f]]
    id <- paste0("fitset__", f)
    lbl <- str_to_title(gsub("_", " ", f, fixed = TRUE))
    if (is.logical(val)) {
      checkboxInput(id, lbl, value = isTRUE(val))
    } else {
      st <- if (identical(f, "proposal_sd")) 0.001 else 1
      numericInput(id, lbl, value = as.numeric(val)[[1]], min = 0, step = st)
    }
  }))

  ## Tooltip copy (HTML title): former Introduction text, kept off the page.
  tip_explore_scenario <- paste0(
    "Name for this run. Pick an existing name to replace it, or type a new one ",
    "to add another saved scenario."
  )
  tip_explore_run <- paste0(
    "Simulate with the numbers on the open parameter tab and store the result ",
    "under the scenario name. Open Timeline or Summary to view charts."
  )
  tip_explore_add_period <- paste0(
    "Add another time segment with its own settings (for example when an ",
    "intervention starts). One segment must begin on day 1."
  )
  tip_explore_remove_period <- "Remove the time segment you are currently editing."
  tip_explore_reset <- "Restore defaults for the open time segment."
  tip_explore_n_particles <- paste0(
    "How many stochastic runs to average; more gives smoother bands but takes ",
    "longer."
  )
  tip_fit_params <- paste0(
    "Epidemic and programme settings held fixed while the fit runs; open each ",
    "field to adjust."
  )
  tip_fit_priors <- "How strongly the model believes transmission sits before seeing your data."
  tip_fit_settings <- "Sampler and filter choices for the Bayesian run."
  tip_fit_run <- paste0(
    "Match the model to your uploaded series; when it finishes, open Timeline ",
    "to see the fit and your data together."
  )
  tip_fit_scenario <- paste0(
    "Name for this scenario run. Pick an existing name to replace it, or type ",
    "a new one to add another saved result."
  )

  ## Left sidebar: Exploration vs Fitting (single column; details in tooltips).
  sidebar_ui_exploration <- function() {
    tagList(
      tags$label(
        `for` = "scenario_select",
        class = "control-label",
        title = tip_explore_scenario,
        style = "cursor: help;",
        "Scenario"
      ),
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
              placeholder = "Name this run",
              create = TRUE,
              createOnBlur = TRUE
            )
          )
        ),
        actionButton(
          inputId = "remove_saved_scenario",
          label = NULL,
          icon = icon("times"),
          title = "Drop the selected saved run from this session.",
          class = "btn-danger",
          style = "padding: 6px 12px; flex-shrink: 0;"
        )
      ),
      div(
        style = "display: flex; align-items: stretch; margin-top: 0px",
        actionButton(
          inputId = "run_scenario",
          label = "Run Scenario",
          width = "100%",
          title = tip_explore_run,
          style = "margin-right:2px; margin-left: 2px"
        )
      ),
      div(
        style = "display: flex; align-items: stretch; margin-top: -20px",
        actionButton(
          inputId = "add_period",
          label = "Add Period",
          width = "33%",
          title = tip_explore_add_period,
          style = "margin-right:2px; margin-left: 2px"
        ),
        actionButton(
          inputId = "remove_period",
          label = "Remove Period",
          width = "33%",
          title = tip_explore_remove_period,
          style = "margin-right:2px; margin-left: 2px"
        ),
        actionButton(
          inputId = "reset",
          label = "Reset",
          width = "33%",
          title = tip_explore_reset,
          style = "margin-right:2px; margin-left: 2px"
        )
      ),
      tags$div(
        style = "margin-top: 8px; cursor: help;",
        title = tip_explore_n_particles,
        numericInput(
          inputId = "n_particles",
          label = "Simulations",
          value = 50L,
          min = 1L,
          step = 1L
        )
      ),
      navset_card_underline(id = "parameters_panel")
    )
  }

  sidebar_ui_fitting <- function() {
    calibration_panel <- tagList(
      tags$div(
        class = "mb-2",
        title = tip_fit_params,
        style = "cursor: help;",
        tags$strong("Parameters")
      ),
      div(
        style = "margin-left: 10px; margin-right: 10px",
        tagList(simex_to_shiny(simex_defaults, "fit"))
      ),
      hr(),
      tags$div(
        class = "mb-1",
        title = tip_fit_priors,
        style = "cursor: help;",
        tags$strong("Priors")
      ),
      selectInput(
        inputId = "prior_p_trans_dist",
        label = "Distribution",
        choices = prior_dist_names,
        selected = "Beta"
      ),
      uiOutput("prior_p_trans_args_ui"),
      hr(),
      tags$div(
        class = "mb-1",
        title = tip_fit_settings,
        style = "cursor: help;",
        tags$strong("Settings")
      ),
      fit_settings_ui,
      hr(),
      actionButton(
        inputId = "run_fit",
        label = "Run fit",
        width = "100%",
        class = "btn-primary",
        title = tip_fit_run
      ),
      ## Indeterminate progress while fit_simex / post-fit sim run (no caption text).
      tags$div(
        id = "fit_progress_container",
        style = "display: none; margin-top: 10px;",
        tags$div(
          class = "progress rounded-pill",
          style = "height: 5px; background-color: #e9ecef;",
          tags$div(
            class = paste0(
              "progress-bar progress-bar-striped progress-bar-animated ",
              "bg-primary"
            ),
            style = "width: 100%;",
            role = "progressbar",
            `aria-valuenow` = 100,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100
          )
        )
      )
    )
    scenarios_panel <- tagList(
      ## Empty-state only; controls stay in static HTML so they are not rebuilt
      ## when Calibration inputs change (that was resetting the dropdown/fields).
      uiOutput("fitting_scenarios_status"),
      tags$label(
        `for` = "fitting_scenario_select",
        class = "control-label",
        title = tip_fit_scenario,
        style = "cursor: help;",
        "Scenario"
      ),
      div(
        style = paste0(
          "display: flex; flex-direction: row; align-items: center; gap: 10px;"
        ),
        div(
          style = "flex: 1; min-width: 0;",
          selectizeInput(
            inputId = "fitting_scenario_select",
            label = NULL,
            choices = character(0),
            selected = "Default",
            options = list(
              placeholder = "Name this run",
              create = TRUE,
              createOnBlur = TRUE
            )
          )
        ),
        actionButton(
          inputId = "fitting_remove_saved_scenario",
          label = NULL,
          icon = icon("times"),
          title = "Drop the selected saved run from this session.",
          class = "btn-danger",
          style = "padding: 6px 12px; flex-shrink: 0;"
        )
      ),
      hr(),
      selectInput(
        inputId = "scen_section_select",
        label = "Parameter",
        choices = c("…" = ""),
        selected = "",
        width = "100%"
      ),
      uiOutput("scenario_section_inputs"),
      actionButton(
        inputId = "scen_add_patch",
        label = "Save scenario",
        width = "100%",
        class = "btn-secondary"
      ),
      hr(),
      actionButton(
        inputId = "scen_run",
        label = "Run scenario",
        width = "100%",
        class = "btn-primary"
      )
    )
    navset_card_underline(
      id = "fitting_sidebar_tabs",
      nav_panel(title = "Calibration", calibration_panel),
      nav_panel(title = "Scenarios", scenarios_panel)
    )
  }

  ## Main model UI (served at /app via brochure; top-level page_sidebar).
  main_simex_ui <- page_sidebar(
    useShinyjs(),
    setBackgroundColor(background_col),
    tags$head(tags$style(HTML(paste(
      paste0("#sidebar{background-color:", sidebar_col, "}"),
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
    tags$head(
      tags$style(
        type = "text/css",
        ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } .inline .form-group { display: table-row;} p.indent {margin-right: 10px}"
      )
    ),
    title = tags$div(
      style = paste0(
        "display:flex;justify-content:space-between;align-items:center;",
        "width:100%;gap:0.75rem;flex-wrap:wrap;"
      ),
      tags$img(
        src = "simex/img/simex_logo.png",
        alt = "simex",
        style = paste0(
          "max-height: 2.75rem; width: auto; flex: 0 0 auto;",
          "object-fit: contain;"
        )
      ),
      tags$a(
        href = "/",
        class = "btn btn-sm btn-outline-secondary border-0 rounded-circle",
        style = paste0(
          "flex-shrink:0;text-decoration:none;width:2.25rem;",
          "height:2.25rem;padding:0;display:inline-flex;",
          "align-items:center;justify-content:center;"
        ),
        title = "Home",
        `aria-label` = "Home",
        icon("home")
      )
    ),
    window_title = "simex",
    sidebar = sidebar(
      width = "40%",
      ## Mode comes from brochure landing + cache only (hydrated server-side).
      shinyjs::hidden(tags$div(
        id = "app_mode_hidden_wrap",
        shinyWidgets::radioGroupButtons(
          inputId = "app_mode",
          label = NULL,
          choices = c("Exploration", "Fitting"),
          selected = "Exploration",
          justified = TRUE
        )
      )),
      uiOutput("sidebar_mode_nav")
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
      )
    ),
    waiter::waiter_preloader(
      html = tagList(
        tags$img(
          src = "simex/img/collaboratory_log.jpg",
          width = 600,
          style = "padding: 20px;"
        ),
        tags$br(),
        waiter::spin_3()
      ),
      color = "#FFFFFF"
    )
  )

  ## Brochure: each route is a new Shiny session; use an in-memory cache keyed
  ## by `cache_tok` in the query string to pass mode choice and uploaded CSV.
  simex_br_cache <- cachem::cache_mem(max_size = 512 * 1024^2, max_n = 2000L)

  ## cache_mem keys must be lowercase letters and digits only (see ?cache_mem).
  new_simex_br_token <- function() {
    paste0(sample(c(letters, as.character(0:9)), 24L, replace = TRUE), collapse = "")
  }

  ## Shared CSV reader for incidence uploads (landing/upload/main app).
  parse_uploaded_csv <- function(fi) {
    if (is.null(fi)) {
      return(NULL)
    }
    df <- tryCatch(
      utils::read.csv(fi$datapath, check.names = FALSE),
      error = function(e) {
        showNotification("That file could not be read.", type = "warning")
        NULL
      }
    )
    if (is.null(df)) {
      return(NULL)
    }
    need_cols <- c("time", "age", "compartment", "value")
    if (!all(need_cols %in% names(df))) {
      showNotification(
        "This file does not look like the expected table layout.",
        type = "warning"
      )
      return(NULL)
    }
    df[, need_cols, drop = FALSE]
  }

  ## Route `/`: pick Exploration vs Fitting, then redirect with a cache token.
  ## Logo is served from `inst/assets/` via `simex` resource path (see zzz.R).
  br_landing_ui <- page_fillable(
    padding = c("2.5rem", "1.25rem"),
    fillable = FALSE,
    setBackgroundColor(background_col),
    div(
      class = "container",
      style = "max-width: 720px; margin-left: auto; margin-right: auto;",
      div(
        class = "d-flex justify-content-end mb-2",
        tags$a(
          href = "https://github.com/WHO-Collaboratory/simex",
          class = paste0(
            "text-decoration-none text-body d-inline-flex align-items-center ",
            "gap-2 small"
          ),
          target = "_blank",
          rel = "noopener noreferrer",
          title = "simex source repository (GitHub)",
          icon("github"),
          tags$span("GitHub")
        )
      ),
      div(
        class = "text-center mb-2",
        tags$img(
          src = "simex/img/simex_logo.png",
          alt = "simex",
          style = "max-width: min(100%, 320px); height: auto;"
        )
      ),
      p(
        class = "text-center text-muted lead mb-4",
        style = paste0(
          "font-size: 1.1rem; max-width: 28rem; ",
          "margin-left: auto; margin-right: auto;"
        ),
        "An outbreak simulation tool for decision-making"
      ),
      div(
        class = "d-flex flex-wrap gap-2 align-items-stretch mb-2",
        style = "width: 100%;",
        div(
          style = "flex: 1 1 240px; min-width: 0;",
          actionButton(
            inputId = "br_go_explore",
            label = tags$span(
              class = paste0(
                "d-flex w-100 align-items-center justify-content-between gap-2 ",
                "text-start"
              ),
              tags$span(
                tagList(tags$strong("Explore"), " without data")
              ),
              icon("arrow-right")
            ),
            class = "btn btn-primary btn-lg py-3",
            width = "100%"
          )
        ),
        div(
          style = "flex: 1 1 240px; min-width: 0;",
          actionButton(
            inputId = "br_go_fitting",
            label = tags$span(
              class = paste0(
                "d-flex w-100 align-items-center justify-content-between gap-2 ",
                "text-start"
              ),
              tags$span(
                tagList(tags$strong("Calibrate"), " with data")
              ),
              icon("arrow-right")
            ),
            class = "btn btn-primary btn-lg py-3",
            width = "100%"
          )
        )
      ),
      card(
        class = "mb-2 mt-5 border-0 shadow-sm",
        style = paste0(
          "border-radius: 14px; overflow: hidden;",
          "background: linear-gradient(180deg, #f8fafc 0%, #ffffff 48%);",
          "border: 1px solid #e9ecef !important;"
        ),
        card_header(
          class = "border-0 pt-4 px-4 pb-2 bg-transparent",
          tags$div(
            class = "text-uppercase small fw-semibold text-muted",
            style = "letter-spacing: 0.06em;",
            "Features"
          )
        ),
        card_body(
          class = "pt-2 pb-4 px-4",
          do.call(
            layout_column_wrap,
            c(
              list(width = 1 / 2, gap = "0.75rem"),
              {
                ## Must not use name `labs` here — it would overwrite parameter
                ## labels for the whole `run_shiny()` closure.
                landing_feature_labs <- c(
                  "Country-specific demographics",
                  "Hospital capacity",
                  "Vaccination campaigns",
                  "Flexible pathogen characteristics",
                  "Flexible disease progression",
                  "Stochastic simulation",
                  "Bayesian model calibration",
                  "Scenario modelling"
                )
                lapply(landing_feature_labs, function(lab) {
                  tags$div(
                    class = paste0(
                      "rounded-3 px-3 py-3 small fw-semibold ",
                      "text-body border bg-white"
                    ),
                    style = paste0(
                      "border-color: #e2e8f0 !important;",
                      "box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);"
                    ),
                    lab
                  )
                })
              }
            )
          )
        )
      ),
      tags$div(
        class = "text-center mt-5 pt-4 border-top",
        tags$p(class = "text-muted small mb-2", "Developed by"),
        tags$a(
          href = "https://collaboratory.who.int",
          class = "d-inline-block text-decoration-none",
          target = "_blank",
          rel = "noopener noreferrer",
          title = "WHO Collaboratory",
          tags$img(
            src = "simex/img/collaboratory_log.jpg",
            alt = "Collaboratory",
            style = "max-width: 220px; width: 100%; height: auto;"
          )
        )
      )
    )
  )

  br_landing_server <- function(input, output, session) {
    observeEvent(input$br_go_explore, {
      tok <- new_simex_br_token()
      simex_br_cache$set(tok, list(app_mode = "Exploration", fitting_df = NULL))
      brochure::server_redirect(
        paste0("/app?cache_tok=", utils::URLencode(tok, reserved = TRUE)),
        session = session
      )
    })
    observeEvent(input$br_go_fitting, {
      tok <- new_simex_br_token()
      simex_br_cache$set(tok, list(app_mode = "Fitting", fitting_df = NULL))
      brochure::server_redirect(
        paste0(
          "/fitting-data?cache_tok=",
          utils::URLencode(tok, reserved = TRUE)
        ),
        session = session
      )
    })
  }

  ## Route `/fitting-data`: CSV upload for Fitting only, then continue to /app.
  br_upload_ui <- page_fillable(
    padding = c("2rem", "1.25rem"),
    fillable = FALSE,
    shinyjs::useShinyjs(),
    setBackgroundColor(background_col),
    div(
      class = "container",
      style = "max-width: 640px; margin-left: auto; margin-right: auto;",
      div(
        style = paste0(
          "display:flex;justify-content:space-between;",
          "align-items:flex-start;gap:1rem;margin-bottom:1rem;"
        ),
        div(
          tags$img(
            src = "simex/img/simex_logo.png",
            alt = "simex",
            style = paste0(
              "max-height: 2.5rem; width: auto; object-fit: contain;",
              "display:block;"
            )
          ),
          h3(style = "margin:0.35rem 0 0 0;", strong("Upload data"))
        ),
        tags$a(
          href = "/",
          class = "btn btn-sm btn-outline-secondary border-0 rounded-circle",
          style = paste0(
            "flex-shrink:0;text-decoration:none;width:2.25rem;",
            "height:2.25rem;padding:0;display:inline-flex;",
            "align-items:center;justify-content:center;"
          ),
          title = "Home",
          `aria-label` = "Home",
          icon("home")
        )
      ),
      card(
        fileInput(
          inputId = "br_incidence_file",
          label = NULL,
          buttonLabel = icon("folder-open"),
          placeholder = "Choose file…",
          accept = c("text/csv", ".csv"),
          width = "100%"
        )
      )
    )
  )

  br_upload_server <- function(input, output, session) {
    ## One-shot gate so this observer does not re-run on every reactive flush.
    br_upload_checked <- reactiveVal(FALSE)
    observe(
      {
        if (isTRUE(br_upload_checked())) {
          return()
        }
        url_q <- session$clientData$url_search
        if (is.null(url_q)) {
          return()
        }
        qs <- shiny::parseQueryString(sub("^\\?", "", url_q))
        tok <- qs[["cache_tok"]]
        if (is.null(tok) || !nzchar(tok) || !isTRUE(simex_br_cache$exists(tok))) {
          br_upload_checked(TRUE)
          brochure::server_redirect("/", session = session)
          return()
        }
        ent <- simex_br_cache$get(tok)
        if (!identical(ent$app_mode, "Fitting")) {
          br_upload_checked(TRUE)
          brochure::server_redirect("/", session = session)
          return()
        }
        br_upload_checked(TRUE)
      },
      priority = 10L
    )

    ## After a valid CSV is chosen, continue to `/app` (same checks as before).
    observeEvent(input$br_incidence_file,
      {
        url_q <- session$clientData$url_search
        if (is.null(url_q)) {
          showNotification("Please start again from the home screen.", type = "warning")
          return()
        }
        qs <- shiny::parseQueryString(sub("^\\?", "", url_q))
        tok <- qs[["cache_tok"]]
        if (is.null(tok) || !nzchar(tok) || !isTRUE(simex_br_cache$exists(tok))) {
          showNotification("That link is no longer valid. Head home and try again.", type = "warning")
          return()
        }
        ent <- simex_br_cache$get(tok)
        if (!identical(ent$app_mode, "Fitting")) {
          brochure::server_redirect("/", session = session)
          return()
        }
        df <- parse_uploaded_csv(input$br_incidence_file)
        if (is.null(df)) {
          return()
        }
        cmp <- toupper(trimws(as.character(df$compartment)))
        if (!any(cmp == "E")) {
          showNotification(
            "This dataset needs exposed-case rows for a fit to run.",
            type = "warning"
          )
          return()
        }
        ent$fitting_df <- df
        simex_br_cache$set(tok, ent)
        brochure::server_redirect(
          paste0("/app?cache_tok=", utils::URLencode(tok, reserved = TRUE)),
          session = session
        )
      },
      ignoreNULL = TRUE
    )
  }

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
          showNotification("Keep one segment starting on day 1.", type = "warning")
          return(NULL)
        }
        if (!all(table(start_days) == 1)) {
          showNotification("Each segment needs a different start day.", type = "warning")
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

    ## After switching to Exploration, seed one period tab once `add_period`
    ## exists (avoids onFlushed racing the renderUI sidebar).
    exploration_periods_seeded <- reactiveVal(FALSE)

    ## After defaults are on-screen, run once so Timeline/Summary are warm.
    exploration_default_run_done <- reactiveVal(FALSE)

    ## Exploration vs Fitting: default Exploration when app_mode is briefly NULL.
    is_exploration <- reactive({
      m <- input$app_mode
      is.null(m) || identical(as.character(m[[1L]]), "Exploration")
    })

    is_fitting <- reactive({
      !is.null(input$app_mode) &&
        identical(as.character(input$app_mode[[1L]]), "Fitting")
    })

    ## Parsed incidence for fitting (uploaded on `/fitting-data` in brochure).
    fitting_data_rv <- reactiveVal(NULL)

    ## Mode taken from brochure cache (avoids racing `input$app_mode` on load).
    cache_app_mode <- reactiveVal(NULL)
    ## Previous `app_mode` value (see observeEvent below).
    app_mode_prev <- reactiveVal(NULL)

    ## Hydrate mode and optional fitting data from brochure cache (see landing).
    brochure_hydrated <- reactiveVal(FALSE)
    observe(
      {
        if (isTRUE(brochure_hydrated())) {
          return()
        }
        url_q <- session$clientData$url_search
        if (is.null(url_q)) {
          return()
        }
        qs <- shiny::parseQueryString(sub("^\\?", "", url_q))
        tok <- qs[["cache_tok"]]
        if (is.null(tok) || !nzchar(tok)) {
          brochure::server_redirect("/", session = session)
          brochure_hydrated(TRUE)
          return()
        }
        if (!isTRUE(simex_br_cache$exists(tok))) {
          brochure::server_redirect("/", session = session)
          brochure_hydrated(TRUE)
          return()
        }
        ent <- simex_br_cache$get(tok)
        mode <- ent$app_mode
        df <- ent$fitting_df
        if (identical(mode, "Fitting") &&
          (is.null(df) || !is.data.frame(df) || nrow(df) == 0L)) {
          brochure::server_redirect(
            paste0(
              "/fitting-data?cache_tok=",
              utils::URLencode(tok, reserved = TRUE)
            ),
            session = session
          )
          brochure_hydrated(TRUE)
          return()
        }
        shinyWidgets::updateRadioGroupButtons(
          session,
          inputId = "app_mode",
          selected = mode
        )
        if (!is.null(df) && is.data.frame(df) && nrow(df) > 0L) {
          fitting_data_rv(df)
        }
        cache_app_mode(mode)
        brochure_hydrated(TRUE)
      },
      priority = 10L
    )

    ## Only reset Exploration tabs when switching from Fitting (not on hydrate).
    observeEvent(input$app_mode,
      {
        req(!is.null(input$app_mode))
        cur <- as.character(input$app_mode[[1L]])
        prev <- app_mode_prev()
        if (identical(cur, "Exploration") && identical(prev, "Fitting")) {
          n_par(0L)
          active_par(character())
          total_par(0L)
          exploration_periods_seeded(FALSE)
          exploration_default_run_done(FALSE)
          cache_app_mode("Exploration")
          ## Drop calibration-only outputs from the shared scenario list.
          sc <- scenarios()
          if ("Fitted" %in% names(sc)) {
            sc[["Fitted"]] <- NULL
            scenarios(sc)
          }
          scenario_patches_rv(list())
        }
        app_mode_prev(cur)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    ## Single sidebar nav tree so inputIds are not duplicated across modes.
    output$sidebar_mode_nav <- renderUI({
      if (is_exploration()) {
        sidebar_ui_exploration()
      } else {
        sidebar_ui_fitting()
      }
    })

    ## First period tab: runs when Exploration UI is up and `add_period` exists.
    observe({
      req(is_exploration())
      req(!is.null(input$add_period))
      if (isTRUE(exploration_periods_seeded())) {
        return()
      }
      req(n_par() == 0L)
      exploration_periods_seeded(TRUE)
      click("add_period")
    })

    ## One automatic Default scenario from packaged defaults (Exploration only).
    observe(
      {
        req(isTRUE(brochure_hydrated()))
        req(identical(cache_app_mode(), "Exploration"))
        req(!isTRUE(exploration_default_run_done()))
        req(n_par() >= 1L)
        req(any(grepl("agestrat", names(input))))
        req(!is.null(input$n_particles))
        out <- run_model_from_inputs()
        if (is.null(out)) {
          return()
        }
        cur <- scenarios()
        cur[["Default"]] <- out
        scenarios(cur)
        exploration_default_run_done(TRUE)
        nav_select(
          id = "main_plots_nav",
          select = "Timeline",
          session = session
        )
      },
      priority = -10L
    )

    observeEvent(
      input$add_period,
      {
        req(is_exploration())
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
                simex_to_shiny(
                  if (length(days) == 0) {
                    simex_defaults
                  } else {
                    last_period_pars(shiny_to_simex(input, head(active_par(), -1)))
                  },
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
      req(is_exploration())
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
      req(is_exploration())
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
      req(is_exploration())
      nm <- scenario_sel_str(input$scenario_select)
      if (!nzchar(nm)) {
        showNotification("Pick a saved run to remove.", type = "warning")
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
      req(is_exploration())
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
      req(is_exploration())
      nms <- names(scenarios())
      nm <- scenario_sel_str(input$scenario_select)
      if (length(nms) == 0L || !nzchar(nm) || !nm %in% nms) {
        disable("remove_saved_scenario")
      } else {
        enable("remove_saved_scenario")
      }
    })

    ## Fitting / Scenarios tab: same selectize + delete pattern as Exploration.
    observe({
      req(is_fitting())
      nms <- names(scenarios())
      sel <- scenario_sel_str(isolate(input$fitting_scenario_select))
      if (length(nms) == 0L) {
        updateSelectizeInput(
          session,
          "fitting_scenario_select",
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
          "fitting_scenario_select",
          choices = stats::setNames(ch, ch),
          selected = new_sel,
          server = TRUE
        )
      }
    })

    observeEvent(input$fitting_remove_saved_scenario, {
      req(is_fitting())
      nm <- scenario_sel_str(input$fitting_scenario_select)
      if (!nzchar(nm)) {
        showNotification("Pick a saved run to remove.", type = "warning")
        return()
      }
      cur <- scenarios()
      if (!nm %in% names(cur)) {
        return()
      }
      cur[[nm]] <- NULL
      scenarios(cur)
    })

    observe({
      req(is_fitting())
      nms <- names(scenarios())
      nm <- scenario_sel_str(input$fitting_scenario_select)
      if (length(nms) == 0L || !nzchar(nm) || !nm %in% nms) {
        disable("fitting_remove_saved_scenario")
      } else {
        enable("fitting_remove_saved_scenario")
      }
    })

    ## reset
    observeEvent(
      input$reset,
      {
        req(is_exploration())
        session$reload()
      }
    )

    ## Monty prior argument inputs for p_trans (rebuilt when distribution changes).
    output$prior_p_trans_args_ui <- renderUI({
      req(is_fitting())
      dist_nm <- input$prior_p_trans_dist
      if (is.null(dist_nm) || !nzchar(as.character(dist_nm))) {
        return(NULL)
      }
      ref <- monty::monty_dsl_distributions()
      idx <- match(dist_nm, ref$name)
      if (is.na(idx)) {
        return(p("Unknown distribution.", class = "text-danger"))
      }
      argnms <- ref$args[[idx]]
      if (length(argnms) == 0L) {
        return(NULL)
      }
      default_for_arg <- function(dist, argnm) {
        if (identical(dist, "Beta") && argnm %in% c("a", "b")) {
          return(1)
        }
        if (identical(dist, "Normal") && identical(argnm, "mean")) {
          return(0)
        }
        if (identical(dist, "Normal") && identical(argnm, "sd")) {
          return(1)
        }
        1
      }
      do.call(
        tagList,
        c(
          list(p("Prior hyperparameters:")),
          lapply(argnms, function(a) {
            numericInput(
              paste0("prior_p_trans__", a),
              label = as.character(a),
              value = default_for_arg(dist_nm, a),
              step = 0.05
            )
          })
        )
      )
    })

    ## Posterior fit state (Fitted scenario on Timeline in Fitting mode).
    fitted_samples_rv <- reactiveVal(NULL)
    fitted_simex_rv <- reactiveVal(NULL)
    ## Time indices from the last successful fit (scenario runs reuse).
    fitting_time_seq_rv <- reactiveVal(NULL)
    ## Names of fitted (Monty) parameters — sections touching these are hidden.
    fitted_par_names_rv <- reactiveVal(character())
    ## Lists of partial arg lists from shiny_to_get_parameters_args_from_tab(...,"scen").
    scenario_patches_rv <- reactiveVal(list())

    ## Calibration args shaped like simex_to_shiny() input (for scenario defaults).
    calibration_args_shaped <- reactive({
      req(is_fitting())
      a <- shiny_to_get_parameters_args_from_tab(input, "fit")
      if (is.null(a)) {
        a <- as.list(simex_defaults)
      }
      args_to_simex_shiny_shape(a)
    })

    ## Section ids (iso3, agestrat, vax, …) excluding anything that overlaps fitted pars.
    scenario_section_choice_ids <- reactive({
      req(is_fitting())
      shaped <- calibration_args_shaped()
      fitnms <- fitted_par_names_rv()
      nms <- names(shaped)
      if (length(fitnms) > 0L) {
        nms <- nms[vapply(nms, function(sec) {
          !any(scenario_section_to_argnames(sec) %in% fitnms)
        }, logical(1))]
      }
      nms
    })

    observeEvent(
      input$run_fit,
      {
        req(is_fitting())
        df <- fitting_data_rv()
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
          showNotification(
            "Add a dataset from the upload step first (home, then Fitting).",
            type = "warning"
          )
          return()
        }
        need_cols <- c("time", "age", "compartment", "value")
        if (!all(need_cols %in% names(df))) {
          showNotification(
            "That dataset is not in the shape this app expects.",
            type = "warning"
          )
          return()
        }
        cmp <- toupper(trimws(as.character(df$compartment)))
        if (!any(cmp == "E")) {
          showNotification(
            "That dataset needs exposed-case rows for a fit to run.",
            type = "warning"
          )
          return()
        }

        parameters <- shiny_to_simex_single(input, "fit")
        if (is.null(parameters)) {
          showNotification("Check the parameter fields on the left.", type = "warning")
          return()
        }

        settings <- collect_fit_settings(input)
        if (is.null(settings)) {
          showNotification("Check the settings fields on the left.", type = "warning")
          return()
        }

        dist_nm <- input$prior_p_trans_dist
        ref <- monty::monty_dsl_distributions()
        idx <- match(dist_nm, ref$name)
        if (is.na(idx)) {
          showNotification("Pick another prior distribution.", type = "warning")
          return()
        }
        argnms <- ref$args[[idx]]
        pri_list <- list(dist = as.character(dist_nm))
        for (a in argnms) {
          ida <- paste0("prior_p_trans__", a)
          if (is.null(input[[ida]])) {
            showNotification(
              paste("Missing prior argument:", a),
              type = "warning"
            )
            return()
          }
          pri_list[[a]] <- as.numeric(input[[ida]])
        }
        priors <- tryCatch(
          get_priors(p_trans = pri_list),
          error = function(e) {
            showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        if (is.null(priors)) {
          return()
        }
        fitted_par_names_rv(priors$parameters)

        fit_df <- as.data.frame(df)[, need_cols, drop = FALSE]
        fit_df <- fit_df[as.numeric(fit_df$time) > 0, , drop = FALSE]
        if (nrow(fit_df) == 0L) {
          showNotification("The series needs at least one time point after day 0.", type = "warning")
          return()
        }
        fit_df$age <- forcats::fct_inorder(as.factor(as.character(fit_df$age)))
        tseq <- seq(
          as.integer(floor(min(as.numeric(fit_df$time), na.rm = TRUE))),
          as.integer(ceiling(max(as.numeric(fit_df$time), na.rm = TRUE)))
        )
        fitting_time_seq_rv(tseq)

        shinyjs::show("fit_progress_container", anim = FALSE)
        on.exit(shinyjs::hide("fit_progress_container", anim = FALSE), add = TRUE)

        samples <- tryCatch(
          fit_simex(fit_df, parameters, priors, settings),
          error = function(e) {
            showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        if (is.null(samples)) {
          return()
        }

        sx <- tryCatch(
          run_simex_from_samples(samples, time = tseq, modification = NULL),
          error = function(e) {
            showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        if (is.null(sx)) {
          return()
        }

        fitted_samples_rv(samples)
        fitted_simex_rv(sx)
        sc <- scenarios()
        sc[["Fitted"]] <- sx
        scenarios(sc)
        nav_select(
          id = "main_plots_nav",
          select = "Timeline",
          session = session
        )
      }
    )

    ## Scenarios tab: empty-state message only (controls are static in sidebar UI).
    output$fitting_scenarios_status <- renderUI({
      if (!is_fitting()) {
        return(NULL)
      }
      ids <- scenario_section_choice_ids()
      if (length(ids) > 0L) {
        return(NULL)
      }
      p(
        class = "text-muted small",
        paste0(
          "No parameters are available for scenario steps ",
          "(all overlap fitted parameters)."
        )
      )
    })

    ## Keep scen_section_select in sync without recreating the input (avoids reset).
    observe({
      req(is_fitting())
      ids <- scenario_section_choice_ids()
      if (length(ids) == 0L) {
        updateSelectInput(
          session,
          "scen_section_select",
          choices = c("—" = ""),
          selected = ""
        )
        shinyjs::disable("scen_section_select")
        return()
      }
      shinyjs::enable("scen_section_select")
      ch <- scenario_section_choice_labels(ids)
      cur <- isolate(input$scen_section_select)
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% ids) {
        cur
      } else {
        ids[[1L]]
      }
      updateSelectInput(
        session,
        "scen_section_select",
        choices = ch,
        selected = sel
      )
    })

    output$scenario_section_inputs <- renderUI({
      if (!is_fitting()) {
        return(NULL)
      }
      sec <- input$scen_section_select
      if (is.null(sec) || !nzchar(sec)) {
        return(NULL)
      }
      ## Only re-build widgets when the section changes, not when Calibration edits
      ## change calibration_args_shaped() (that was wiping scenario field values).
      shaped <- isolate(calibration_args_shaped())
      if (!sec %in% names(shaped)) {
        return(NULL)
      }
      make_input(shaped[[sec]], sec, "scen")
    })

    observeEvent(input$scen_add_patch, {
      req(is_fitting())
      patch <- shiny_to_get_parameters_args_from_tab(input, "scen")
      if (is.null(patch) || length(patch) == 0L) {
        showNotification("Adjust the fields for this parameter, then save.", type = "warning")
        return()
      }
      cur <- scenario_patches_rv()
      scenario_patches_rv(c(cur, list(patch)))
    })

    observeEvent(input$scen_run, {
      req(is_fitting())
      samples <- fitted_samples_rv()
      if (is.null(samples)) {
        showNotification("Run a calibration fit first.", type = "warning")
        return()
      }
      patches <- scenario_patches_rv()
      if (length(patches) == 0L) {
        showNotification("Save at least one scenario step first.", type = "warning")
        return()
      }
      tseq <- fitting_time_seq_rv()
      if (is.null(tseq) || length(tseq) == 0L) {
        showNotification("Run a fit first so a time grid is available.", type = "warning")
        return()
      }
      modification <- Reduce(
        function(a, b) utils::modifyList(a, b),
        patches,
        init = list()
      )
      sx <- tryCatch(
        run_simex_from_samples(
          samples,
          time = tseq,
          modification = modification
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(sx)) {
        return()
      }
      nm <- scenario_sel_str(input$fitting_scenario_select)
      if (!nzchar(nm)) {
        nm <- if (length(scenarios()) == 0L) {
          "Default"
        } else {
          paste0("Saved_", length(scenarios()) + 1L)
        }
      }
      sc <- scenarios()
      sc[[nm]] <- sx
      scenarios(sc)
      nav_select(
        id = "main_plots_nav",
        select = "Timeline",
        session = session
      )
    })

    ## Overlay data for Timeline: only in Fitting mode (incidence rule in plot block).
    timeline_plot_data <- reactive({
      if (!is_fitting()) {
        return(NULL)
      }
      fitting_data_rv()
    })

    ## Named list for Timeline / Summary: includes Fitted after a successful fit
    ## (stored in scenarios() alongside Exploration runs).
    timeline_plot_scenarios <- reactive({
      scenarios()
    })

    ## Timeline: saved scenarios (+ Fitted in Fitting mode) and optional data overlay.
    ehd_plot_result <- reactive({
      req(!is.null(input$ehd_timeline_outcome))
      d <- timeline_plot_data()
      has_cols <- !is.null(d) &&
        is.data.frame(d) &&
        nrow(d) > 0L &&
        all(c("time", "age", "compartment", "value") %in% names(d))
      can_plot_data <- has_cols && tolower(input$ehd_timeline_what) == "incidence"
      scen <- timeline_plot_scenarios()
      has_models <- length(scen) > 0L
      if (!has_models && !can_plot_data) {
        return(NULL)
      }
      cmpt <- c(
        Cases = "E",
        Hospitalisations = "H",
        Deaths = "D"
      )[[input$ehd_timeline_outcome]]
      plot_simex(
        scen,
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
        return(div(class = "ehd-timeline-empty", style = "min-height: 70vh;"))
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

    ## Summary: scenarios only (no CSV upload); mode-specific empty copy.
    output$summary_plot_container <- renderUI({
      has_scen <- length(scenarios()) > 0L
      if (!has_scen) {
        if (is_exploration()) {
          return(div(
            class = "text-muted",
            p("Save at least one scenario with ", em("Run Scenario"), " to compare here.")
          ))
        }
        return(div(
          class = "text-muted",
          p("Save scenarios or finish a fit to compare endpoints here.")
        ))
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
  }

  ## Run the multi-page Shiny app ({brochure}: `/`, `/fitting-data`, `/app`).
  brochure::brochureApp(
    brochure::page(
      href = "/",
      ui = br_landing_ui,
      server = br_landing_server
    ),
    brochure::page(
      href = "/fitting-data",
      ui = br_upload_ui,
      server = br_upload_server
    ),
    brochure::page(
      href = "/app",
      ui = main_simex_ui,
      server = server
    )
  )
}
