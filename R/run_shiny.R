#' This function launches the simex Shiny app .
#'
#' @importFrom brochure brochureApp server_redirect
#' @importFrom cachem cache_mem
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyjs disable enable hide show useShinyjs
#' @importFrom shiny icon showNotification
#' @importFrom shinyWidgets setBackgroundColor switchInput radioGroupButtons
#' @importFrom waiter waiter_preloader spin_3
#' @importFrom stringr str_match str_to_title
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
    hosp_capacity = "Hospital capacity (beds)",
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

    ## collapse age-stratified parameters into one matrix
    agestrat <- lengths(simex_input) == nrow(cdat[[1]]$pop)
    simex_input$agestrat <- do.call(cbind, simex_input[agestrat])
    ## Row labels must be human-readable ages (vector values), not age_* names.
    dimnames(simex_input$agestrat) <- list(
      unname(as.character(get_age_cat())),
      as.character(labs[colnames(simex_input$agestrat)])
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
    agestrat <- lengths(simex_input) == nrow(cdat[[1]]$pop)
    if (any(agestrat)) {
      simex_input$agestrat <- do.call(cbind, simex_input[agestrat])
      dimnames(simex_input$agestrat) <- list(
        unname(as.character(get_age_cat())),
        as.character(labs[colnames(simex_input$agestrat)])
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

  ## Tab title text: always "Period 1", "Period 2", … by position in the scenario.
  exploration_period_label <- function(i) {
    paste("Period", as.integer(i))
  }

  ## Trailing nav tab used to insert new periods (always last; never in active_par).
  period_plus_value <- ".__period_plus__."

  ## Period tab label with an inline close control (handled via JS + input).
  period_nav_title <- function(period_label, tab_id) {
    shiny::tags$span(
      class = "d-inline-flex align-items-center gap-1",
      shiny::tags$span(class = "simex-period-tab-label", period_label),
      shiny::tags$span(
        class = "simex-period-tab-close",
        `data-tab-id` = tab_id,
        style = paste0(
          "cursor:pointer;opacity:0.65;line-height:1;",
          "font-size:0.85em;padding:0 0.2rem;border-radius:2px;"
        ),
        shiny::HTML("&#215;")
      )
    )
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
    } else if (is.logical(value) ||
      (name %in% c("vax_prioritised", "hosp_prioritised") &&
        is.numeric(value) && length(value) == 1L)) {
      ## get_parameters() stores these as integer 0/1; still render as checkboxes.
      chk <- if (is.logical(value)) {
        isTRUE(value)
      } else {
        as.numeric(value)[[1L]] != 0
      }
      checkboxInput(rn(name, id), input_lbl(name), value = chk)
    } else if (is.numeric(value)) {
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

  ## `navset_card_underline(id = "calib_tabs")` value per Calibration step (order matters).
  calib_tab_order <- c(
    "calib_params",
    "calib_priors",
    "calib_settings",
    "calib_run"
  )

  ## Tooltip copy (HTML title): former Introduction text, kept off the page.
  tip_explore_scenario <- paste0(
    "Pick a saved scenario to load its parameters into the period tabs, or use ",
    "Add to name a new run before you adjust parameters and Save."
  )
  tip_explore_run <- paste0(
    "Run the simulation and save results under the selected scenario name ",
    "(save icon). Open Timeline or Summary to view charts. Add periods with ",
    "the + tab; remove a period from the x on its tab (keep at least one)."
  )
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
    "Pick a saved scenario to load its parameters into Calibration, or use ",
    "Add to name a new run; use Save to run from samples and store the result."
  )

  ## Left sidebar: Exploration vs Fitting (single column; details in tooltips).
  sidebar_ui_exploration <- function() {
    tagList(
      selectizeInput(
        inputId = "scenario_select",
        label = tags$span(
          class = "control-label",
          style = "cursor: help;",
          title = tip_explore_scenario,
          "Scenario"
        ),
        choices = character(0),
        selected = NULL,
        width = "100%",
        options = list(placeholder = "Select scenario")
      ),
      div(
        class = "d-flex flex-row gap-2 align-items-stretch simex-scenario-actions",
        style = "margin-top: 0.35rem;",
        actionButton(
          inputId = "explore_add_scenario",
          label = shiny::tagList(shiny::icon("plus"), " Add"),
          title = "Add scenario (name in dialog), then adjust parameters and save.",
          class = "btn-secondary flex-fill",
          style = "min-width: 0;"
        ),
        actionButton(
          inputId = "remove_saved_scenario",
          label = shiny::tagList(shiny::icon("minus"), " Remove"),
          title = "Remove the selected scenario (saved or not yet saved).",
          class = "btn-secondary flex-fill",
          style = "min-width: 0;"
        ),
        actionButton(
          inputId = "run_scenario",
          label = shiny::tagList(shiny::icon("save"), " Save"),
          title = tip_explore_run,
          class = "btn-secondary flex-fill",
          style = "min-width: 0;"
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
      navset_card_underline(
        id = "parameters_panel",
        nav_panel(
          title = shiny::tags$span(
            style = "font-weight:700;font-size:1.05rem;line-height:1;",
            "+"
          ),
          value = period_plus_value,
          shiny::tags$div(class = "simex-period-plus-panel")
        )
      ),
      div(
        style = "display: flex; align-items: stretch; margin-top: 8px;",
        actionButton(
          inputId = "reset",
          label = "Reset",
          width = "100%",
          title = tip_explore_reset,
          style = "margin-right:2px; margin-left: 2px"
        )
      )
    )
  }

  sidebar_ui_fitting <- function() {
    ## Calibration: same nav style as Calibration vs Scenarios (`navset_card_underline`).
    calibration_panel <- tagList(
      navset_card_underline(
        id = "calib_tabs",
        selected = calib_tab_order[[1L]],
        footer = tags$div(
          class = paste0(
            "d-flex align-items-center justify-content-between ",
            "gap-2 flex-wrap px-2 pb-2 pt-1"
          ),
          actionButton(
            inputId = "calib_prev",
            label = "Back",
            class = "btn-outline-secondary"
          ),
          tags$div(
            id = "calib_next_wrap",
            actionButton(
              inputId = "calib_next",
              label = "Next",
              class = "btn-primary"
            )
          )
        ),
        nav_panel(
          title = "Parameters",
          value = calib_tab_order[[1L]],
          div(
            style = paste0(
              "margin-left: 10px; margin-right: 10px; cursor: help;"
            ),
            title = tip_fit_params,
            tagList(simex_to_shiny(simex_defaults, "fit"))
          )
        ),
        nav_panel(
          title = "Priors",
          value = calib_tab_order[[2L]],
          div(
            style = "cursor: help;",
            title = tip_fit_priors,
            selectInput(
              inputId = "prior_p_trans_dist",
              label = "Distribution",
              choices = prior_dist_names,
              selected = "Beta"
            ),
            uiOutput("prior_p_trans_args_ui")
          )
        ),
        nav_panel(
          title = "Settings",
          value = calib_tab_order[[3L]],
          div(
            style = "cursor: help;",
            title = tip_fit_settings,
            fit_settings_ui
          )
        ),
        nav_panel(
          title = "Run",
          value = calib_tab_order[[4L]],
          actionButton(
            inputId = "run_fit",
            label = "Run fit",
            width = "100%",
            class = "btn-primary",
            title = tip_fit_run
          ),
          ## Shown after flush while fit_simex / post-fit sim run (not a modal).
          tags$div(
            id = "fit_progress_container",
            style = "display: none; margin-top: 10px;",
            tags$div(
              class = "progress rounded-pill",
              style = "height: 5px; background-color: #e9ecef;",
              tags$div(
                id = "fit_progress_bar",
                class = paste0(
                  "progress-bar progress-bar-striped progress-bar-animated ",
                  "bg-primary"
                ),
                style = "width: 100%;",
                role = "progressbar",
                `aria-valuenow` = 0,
                `aria-valuemin` = 0,
                `aria-valuemax` = 100
              )
            )
          )
        )
      )
    )
    scenarios_panel <- tagList(
      ## Empty-state only; controls stay in static HTML so they are not rebuilt
      ## when Calibration inputs change (that was resetting the dropdown/fields).
      uiOutput("fitting_scenarios_status"),
      selectizeInput(
        inputId = "fitting_scenario_select",
        label = tags$span(
          class = "control-label",
          style = "cursor: help;",
          title = tip_fit_scenario,
          "Scenario"
        ),
        choices = character(0),
        selected = NULL,
        width = "100%",
        options = list(placeholder = "Select scenario")
      ),
      div(
        class = "d-flex flex-row gap-2 align-items-stretch simex-scenario-actions",
        style = "margin-top: 0.35rem;",
        actionButton(
          inputId = "fit_add_scenario",
          label = shiny::tagList(shiny::icon("plus"), " Add"),
          title = "Name a new scenario, then save steps and run.",
          class = "btn-secondary flex-fill",
          style = "min-width: 0;"
        ),
        actionButton(
          inputId = "fitting_remove_saved_scenario",
          label = shiny::tagList(shiny::icon("minus"), " Remove"),
          title = "Remove the selected scenario (saved or not yet saved).",
          class = "btn-secondary flex-fill",
          style = "min-width: 0;"
        ),
        actionButton(
          inputId = "scen_run",
          label = shiny::tagList(shiny::icon("save"), " Save"),
          title = paste0(
            "Run the scenario from posterior samples and store under the ",
            "selected name (same as Exploration Save)."
          ),
          class = "btn-secondary flex-fill",
          style = "min-width: 0;"
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
      )
    )
    navset_card_underline(
      id = "fitting_sidebar_tabs",
      nav_panel(title = "Calibration", calibration_panel),
      nav_panel(title = "Scenarios", scenarios_panel)
    )
  }

  ## Compact modal: scenario name, Enter submits, top-right close (no Cancel row).
  ## Shiny applies `modalDialog(..., class=)` to `.modal-body` only; header is a
  ## sibling, so rules use `#shiny-modal` (this app has no other modals).
  scenario_add_modal <- function() {
    shiny::modalDialog(
      title = shiny::tags$button(
        type = "button",
        class = "btn-close simex-scenario-add-modal-close",
        `data-bs-dismiss` = "modal",
        `aria-label` = "Close"
      ),
      size = "s",
      class = "simex-scenario-add-modal",
      footer = NULL,
      shiny::tagList(
        shiny::textInput(
          inputId = "scenario_modal_new_name",
          label = shiny::tags$strong("Scenario name"),
          value = "",
          width = "100%"
        ),
        shiny::tags$div(
          class = "simex-scenario-add-modal-actions text-center",
          shiny::actionButton(
            inputId = "scenario_modal_confirm",
            label = "Add",
            class = "btn-primary"
          )
        ),
        shiny::tags$script(shiny::HTML(paste0(
          "(function(){",
          "var i=document.getElementById('scenario_modal_new_name');",
          "var b=document.getElementById('scenario_modal_confirm');",
          "if(!i||!b){return;}",
          "if(i._simexScenarioKd){i.removeEventListener('keydown',i._simexScenarioKd);}",
          "i._simexScenarioKd=function(e){",
          "if(e.key!=='Enter'&&e.keyCode!==13){return;}",
          "e.preventDefault();",
          "b.click();",
          "};",
          "i.addEventListener('keydown',i._simexScenarioKd);",
          "})();"
        )))
      ),
      easyClose = TRUE
    )
  }

  ## Favicon (tab icon); `simex/` maps to `inst/assets/` (see `zzz.R`).
  simex_favicon_head <- shiny::tags$head(
    shiny::tags$link(
      rel = "icon",
      href = "simex/img/simex_icon.png",
      type = "image/png"
    )
  )

  ## Main model UI (served at /app via brochure; top-level page_sidebar).
  main_simex_ui <- page_sidebar(
    useShinyjs(),
    simex_favicon_head,
    setBackgroundColor(background_col),
    tags$head(tags$style(HTML(paste(
      paste0("#sidebar{background-color:", sidebar_col, "}"),
      paste0(
        "#explore_add_scenario,#remove_saved_scenario,",
        "#fit_add_scenario,#fitting_remove_saved_scenario,",
        "#scen_run,#run_scenario,#reset{",
        "background-color:#fff!important;color:#212529!important;",
        "border:1px solid #808080!important;",
        "min-height:2.65rem!important;padding:0.5rem 0.75rem!important;}",
        "#explore_add_scenario:hover,#explore_add_scenario:active,",
        "#explore_add_scenario:focus-visible,",
        "#remove_saved_scenario:hover,#remove_saved_scenario:active,",
        "#remove_saved_scenario:focus-visible,",
        "#fit_add_scenario:hover,#fit_add_scenario:active,#fit_add_scenario:focus-visible,",
        "#fitting_remove_saved_scenario:hover,#fitting_remove_saved_scenario:active,",
        "#fitting_remove_saved_scenario:focus-visible,",
        "#scen_run:hover,#scen_run:active,#scen_run:focus-visible,",
        "#run_scenario:hover,#run_scenario:active,#run_scenario:focus-visible,",
        "#reset:hover,#reset:active,#reset:focus-visible{",
        "background-color:#e9ecef!important;color:#000!important;",
        "border-color:#808080!important;}",
        ".simex-scenario-actions .btn{display:inline-flex!important;",
        "align-items:center!important;justify-content:center!important;",
        "gap:0.35rem!important;min-height:2.65rem!important;padding:0.5rem 0.5rem!important;}",
        ".simex-period-tab-close:hover{opacity:1!important;",
        "background-color:rgba(0,0,0,0.06);border-radius:2px;}",
        "#shiny-modal .modal-content{padding:10px 12px 6px 12px!important;}",
        "#shiny-modal .modal-header{border-bottom:none!important;",
        "padding:0.2rem 0.45rem 0.15rem 0.25rem!important;margin:0!important;",
        "display:flex!important;justify-content:flex-end!important;",
        "align-items:flex-start!important;}",
        "#shiny-modal .modal-title{width:100%!important;margin:0!important;",
        "padding:0!important;border:none!important;font-size:inherit!important;",
        "line-height:1!important;display:flex!important;",
        "justify-content:flex-end!important;align-items:flex-start!important;}",
        "#shiny-modal .btn-close.simex-scenario-add-modal-close{",
        "transform:scale(0.55)!important;transform-origin:100% 0!important;",
        "margin:0!important;padding:0.2rem!important;opacity:0.75!important;}",
        "#shiny-modal .modal-body.simex-scenario-add-modal{padding:6px 8px 14px 8px!important;",
        "margin:0!important;}",
        "#shiny-modal .modal-body.simex-scenario-add-modal .shiny-input-container{",
        "margin:0 0 6px 0!important;padding-top:0!important;margin-top:0!important;}",
        "#shiny-modal .modal-body.simex-scenario-add-modal .form-group{",
        "margin:0!important;padding:0!important;}",
        "#shiny-modal .modal-body.simex-scenario-add-modal .form-label,",
        "#shiny-modal .modal-body.simex-scenario-add-modal label{",
        "margin:0 0 8px 0!important;padding-top:0!important;font-weight:700!important;}",
        "#shiny-modal .simex-scenario-add-modal-actions{margin:0!important;",
        "padding-top:10px!important;padding-bottom:2px!important;}",
        "#shiny-modal .simex-scenario-add-modal-actions .btn{",
        "min-width:7.5rem!important;min-height:2.35rem!important;}"
      ),
      ## Plot toggles: under BS5, `radioGroupButtons` uses `input.btn-check` + `label`
      ## (not `button`); `status='default'` becomes `outline-primary` in shinyWidgets.
      paste0(
        ".simex-period-plus-panel{min-height:0!important;padding:0!important;",
        "margin:0!important;}",
        "#ehd_timeline_outcome label.radiobtn,",
        "#ehd_timeline_what label.radiobtn,",
        "#summary_what label.radiobtn{",
        "background-color:#fff!important;color:#212529!important;",
        "border-color:#adb5bd!important;",
        "box-shadow:none!important;",
        "--bs-btn-color:#212529!important;--bs-btn-bg:#fff!important;",
        "--bs-btn-border-color:#adb5bd!important;",
        "--bs-btn-hover-bg:#f8f9fa!important;",
        "--bs-btn-hover-border-color:#adb5bd!important;",
        "--bs-btn-active-bg:#e9ecef!important;",
        "--bs-btn-active-border-color:#6c757d!important;",
        "--bs-btn-active-color:#212529!important;}",
        "#ehd_timeline_outcome .btn-check:checked+label.radiobtn,",
        "#ehd_timeline_what .btn-check:checked+label.radiobtn,",
        "#summary_what .btn-check:checked+label.radiobtn{",
        "background-color:#e9ecef!important;color:#212529!important;",
        "border-color:#6c757d!important;",
        "--bs-btn-active-bg:#e9ecef!important;",
        "--bs-btn-active-border-color:#6c757d!important;",
        "--bs-btn-active-color:#212529!important;}",
        "#ehd_timeline_outcome .btn-check:not(:checked)+label.radiobtn:hover,",
        "#ehd_timeline_what .btn-check:not(:checked)+label.radiobtn:hover,",
        "#summary_what .btn-check:not(:checked)+label.radiobtn:hover{",
        "background-color:#f8f9fa!important;}",
        "#ehd_timeline_outcome .btn-check:checked+label.radiobtn:hover,",
        "#ehd_timeline_what .btn-check:checked+label.radiobtn:hover,",
        "#summary_what .btn-check:checked+label.radiobtn:hover{",
        "background-color:#dee2e6!important;color:#000!important;}",
        "#ehd_timeline_outcome button.radiobtn,",
        "#ehd_timeline_what button.radiobtn,",
        "#summary_what button.radiobtn{",
        "background-color:#fff!important;color:#212529!important;",
        "border:1px solid #adb5bd!important;",
        "box-shadow:none!important;}",
        "#ehd_timeline_outcome button.radiobtn.active,",
        "#ehd_timeline_outcome button.radiobtn.active.focus,",
        "#ehd_timeline_what button.radiobtn.active,",
        "#ehd_timeline_what button.radiobtn.active.focus,",
        "#summary_what button.radiobtn.active,",
        "#summary_what button.radiobtn.active.focus{",
        "background-color:#e9ecef!important;color:#212529!important;",
        "border-color:#6c757d!important;",
        "box-shadow:none!important;}",
        "#ehd_timeline_outcome button.radiobtn:hover:not(.active),",
        "#ehd_timeline_what button.radiobtn:hover:not(.active),",
        "#summary_what button.radiobtn:hover:not(.active){",
        "background-color:#f8f9fa!important;}",
        "#ehd_timeline_outcome button.radiobtn.active:hover,",
        "#ehd_timeline_what button.radiobtn.active:hover,",
        "#summary_what button.radiobtn.active:hover{",
        "background-color:#dee2e6!important;color:#000!important;}",
        "#ehd_stratify_age + label.switch::before{",
        "background:#ced4da!important;",
        "box-shadow:inset 0 0 6px rgba(0,0,0,0.06)!important;",
        "opacity:1!important;}",
        "#ehd_stratify_age + label.switch::after{",
        "background:#fff!important;border:1px solid #adb5bd!important;",
        "box-shadow:0 1px 2px rgba(0,0,0,0.1)!important;}",
        "#ehd_stratify_age:checked + label.switch::before{",
        "background:#868e96!important;opacity:1!important;}",
        "#ehd_stratify_age:checked + label.switch::after{",
        "background:#f8f9fa!important;border-color:#6c757d!important;}"
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
      ),
      ## Monty simple runner: map MONTY-PROGRESS to #fit_progress_bar (no text).
      tags$script(HTML(paste0(
        "(function(){",
        "if(window.simexFitMontyProgress){return;}",
        "window.simexFitMontyProgress=1;",
        "Shiny.addCustomMessageHandler('fit_monty_progress',function(msg){",
        "var bar=document.getElementById('fit_progress_bar');",
        "if(!bar){return;}",
        "if(msg.reset){",
        "bar.classList.add('progress-bar-striped','progress-bar-animated');",
        "bar.style.width='100%';",
        "return;",
        "}",
        "if(msg.phase==='sim'){",
        "bar.classList.add('progress-bar-striped','progress-bar-animated');",
        "bar.style.width='100%';",
        "return;",
        "}",
        "bar.classList.remove('progress-bar-striped','progress-bar-animated');",
        "var p=typeof msg.pct==='number'?msg.pct:0;",
        "bar.style.width=Math.min(100,Math.max(0,p))+'%';",
        "bar.setAttribute('aria-valuenow',String(Math.round(p)));",
        "});",
        "})();"
      ))),
      tags$script(HTML(paste0(
        "(function(){",
        "if(window.simexPeriodTabLabelsReg){return;}",
        "window.simexPeriodTabLabelsReg=1;",
        "Shiny.addCustomMessageHandler('simex_period_tab_labels',function(msg){",
        "var labels=msg&&msg.labels;",
        "if(!labels||typeof labels!=='object'){return;}",
        "document.querySelectorAll('.simex-period-tab-close[data-tab-id]').forEach(",
        "function(closeEl){",
        "var id=closeEl.getAttribute('data-tab-id');",
        "if(!id||labels[id]===undefined){return;}",
        "var root=closeEl.parentElement;",
        "if(!root){return;}",
        "var lab=root.querySelector('.simex-period-tab-label');",
        "if(lab){lab.textContent=labels[id];}",
        "});",
        "});",
        "})();"
      ))),
      tags$script(HTML(paste0(
        "document.addEventListener('click',function(ev){",
        "var el=ev.target.closest('.simex-period-tab-close');",
        "if(!el)return;",
        "ev.preventDefault();ev.stopPropagation();",
        "if(!window.Shiny)return;",
        "var id=el.getAttribute('data-tab-id');",
        "if(!id)return;",
        "Shiny.setInputValue('period_tab_close_req',",
        "{id:id,_:Date.now()},{priority:'event'});",
        "},true);"
      )))
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
                size = "sm",
                status = "outline-secondary"
              )
            ),
            div(
              style = "display: inline-block; vertical-align: top; margin-right: 12px;",
              radioGroupButtons(
                inputId = "ehd_timeline_what",
                selected = "Incidence",
                label = NULL,
                choices = c("Incidence", "Prevalence"),
                size = "sm",
                status = "outline-secondary"
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
          choices = c("Cases", "Hospitalisations", "Deaths"),
          size = "sm",
          status = "outline-secondary"
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
    simex_favicon_head,
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
    simex_favicon_head,
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
        ## Avoid shiny_to_simex() before every period tab has reached the client
        ## (otherwise map over periods can see an empty par list for a new tab).
        for (tid in active_par()) {
          if (is.null(input[[rn("day", tid)]])) {
            return(NULL)
          }
        }
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

    ## selectize `choices` as a named list (named atomic vector triggers jsonlite's
    ## keep_vec_names deprecation during Shiny JSON serialization).
    simex_selectize_choices <- function(ch) {
      if (length(ch) == 0L) {
        return(character(0))
      }
      ch <- as.character(ch)
      ch <- ch[vapply(ch, nzchar, logical(1L))]
      if (length(ch) == 0L) {
        return(character(0))
      }
      stats::setNames(as.list(ch), ch)
    }

    ## Which sidebar opened the add-scenario modal ("exploration" | "fitting").
    scenario_modal_context <- reactiveVal("exploration")

    ## Not-yet-saved scenario name from Add dialog (so Remove can drop it without
    ## treating it like a stale orphan after deletion).
    explore_scenario_pending <- reactiveVal(NULL)
    fit_scenario_pending <- reactiveVal(NULL)

    ## Merge one stored simex parameter column into the list shape simex_to_shiny()
    ## expects (fractional percents, separate vax_* keys, etc.).
    simex_col_to_simex_input <- function(col_obj) {
      z <- utils::modifyList(as.list(simex_defaults), as.list(col_obj))
      z[setdiff(names(z), names(simex_defaults))] <- NULL
      z
    }

    ## Push args_to_simex_shiny_shape() output onto existing period / fit widgets.
    apply_shaped_pars_to_tab <- function(tab_id, shaped) {
      for (nm in names(shaped)) {
        val <- shaped[[nm]]
        wid_base <- rn(nm, tab_id)
        if (identical(nm, "iso3")) {
          shiny::updateSelectInput(session, wid_base, selected = as.character(val))
        } else if (is.matrix(val)) {
          shinyMatrix::updateMatrixInput(session, wid_base, value = val)
        } else if (is.logical(val)) {
          shiny::updateCheckboxInput(session, wid_base, value = isTRUE(val))
        } else if (is.numeric(val)) {
          if (length(val) == 1L && is.null(names(val))) {
            shiny::updateNumericInput(session, wid_base, value = as.numeric(val))
          } else {
            for (subnm in names(val)) {
              shiny::updateNumericInput(
                session,
                rn(subnm, tab_id),
                value = as.numeric(val[[subnm]])
              )
            }
          }
        }
      }
    }

    ## number and names of active parameters (used by exploration_load_simex etc.)
    n_par <- reactiveVal(0)
    active_par <- reactiveVal()

    ## Update visible "Period n" labels after reordering (e.g. delete mid-period).
    ## Called from `session$onFlushed()` — must isolate reactiveVals (no reactive ctx).
    sync_exploration_period_tab_labels <- function() {
      ap <- shiny::isolate(active_par())
      if (length(ap) == 0L) {
        return()
      }
      ## Named list of length-1 strings (not a named vector) for jsonlite / toJSON.
      labs <- list()
      for (i in seq_along(ap)) {
        labs[[as.character(ap[[i]])]] <- paste("Period", i)
      }
      session$sendCustomMessage("simex_period_tab_labels", list(labels = labs))
    }

    ## Align Exploration period tabs and inputs with a saved simex object.
    exploration_load_simex <- function(sx) {
      req(is_exploration())
      pm <- sx$pars
      if (is.null(pm) || !is.matrix(pm) || ncol(pm) < 1L) {
        return()
      }
      ord <- order(as.numeric(colnames(pm)))
      pm <- pm[, ord, drop = FALSE]
      days <- as.numeric(colnames(pm))
      nneed <- ncol(pm)
      while (n_par() > nneed) {
        tid <- dplyr::last(active_par())
        active_par(utils::head(active_par(), -1L))
        n_par(n_par() - 1L)
        nav_remove(id = "parameters_panel", target = tid)
      }
      while (n_par() < nneed) {
        new_id <- paste0(sample(letters, 10, TRUE), collapse = "")
        active_par(c(active_par(), new_id))
        n_par(n_par() + 1L)
        k <- n_par()
        nav_insert(
          id = "parameters_panel",
          nav = nav_panel(
            title = period_nav_title(exploration_period_label(k), new_id),
            value = new_id,
            div(
              style = "margin-left: 10px; margin-right: 10px",
              headerPanel(""),
              numericInput(rn("day", new_id), "Start day", days[[k]]),
              tagList(
                simex_to_shiny(simex_col_to_simex_input(pm[[1L, k]]), tab_id = new_id)
              )
            )
          ),
          target = period_plus_value,
          position = "before",
          session = session
        )
      }
      for (i in seq_len(nneed)) {
        tid <- active_par()[[i]]
        shaped <- args_to_simex_shiny_shape(as.list(pm[[1L, i]]))
        shiny::updateNumericInput(session, rn("day", tid), value = days[[i]])
        apply_shaped_pars_to_tab(tid, shaped)
      }
      nav_select(
        id = "parameters_panel",
        select = active_par()[[1L]],
        session = session
      )
      session$onFlushed(
        function() {
          sync_exploration_period_tab_labels()
        },
        once = TRUE
      )
    }

    ## After defaults are on-screen, run once so Timeline/Summary are warm.
    exploration_default_run_done <- reactiveVal(FALSE)

    ## After any period insert, `input$parameters_panel` can briefly stay on the
    ## + tab while `n_par() >= 1`, which would spuriously add a second period.
    ## Suppress the + handler until the next flush.
    exploration_suppress_plus_add <- reactiveVal(FALSE)
    ## Last selected panel value that is not the trailing + (user must move from
    ## a real period to + to add another period).
    last_exploration_non_plus_panel <- reactiveVal(NULL)

    ## Insert a new Exploration period tab immediately before the trailing + tab.
    insert_new_period_tab <- function() {
      req(is_exploration())
      exploration_suppress_plus_add(TRUE)
      session$onFlushed(
        function() {
          exploration_suppress_plus_add(FALSE)
        },
        once = TRUE
      )
      new_id <- paste0(sample(letters, 10, TRUE), collapse = "")
      active_par(c(active_par(), new_id))
      n_par(n_par() + 1L)
      days <- extract_active_par(input, "day", active_par)
      start_day <- if (length(days) == 0) 1 else max(days) + 50
      nav_insert(
        id = "parameters_panel",
        nav = nav_panel(
          title = period_nav_title(exploration_period_label(n_par()), new_id),
          value = new_id,
          do.call(
            div,
            list(
              style = "margin-left: 10px; margin-right: 10px",
              headerPanel(""),
              numericInput(rn("day", new_id), "Start day", start_day),
              simex_to_shiny(
                if (length(days) == 0) {
                  simex_defaults
                } else {
                  last_period_pars(shiny_to_simex(input, head(active_par(), -1)))
                },
                tab_id = new_id
              )
            )
          )
        ),
        target = period_plus_value,
        position = "before",
        session = session
      )
      nav_select(
        id = "parameters_panel",
        select = new_id,
        session = session
      )
    }

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
          exploration_default_run_done(FALSE)
          exploration_suppress_plus_add(FALSE)
          last_exploration_non_plus_panel(NULL)
          cache_app_mode("Exploration")
          fit_scenario_pending(NULL)
          ## Drop calibration-only outputs from the shared scenario list.
          sc <- scenarios()
          if ("Fitted" %in% names(sc)) {
            sc[["Fitted"]] <- NULL
            scenarios(sc)
          }
          scenario_patches_rv(list())
        }
        ## Return to Fitting: first calibration tab (after sidebar flush).
        if (identical(cur, "Fitting") && identical(prev, "Exploration")) {
          explore_scenario_pending(NULL)
          session$onFlushed(
            function() {
              nav_select(
                id = "calib_tabs",
                select = calib_tab_order[[1L]],
                session = session
              )
            },
            once = TRUE
          )
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

    observeEvent(input$calib_next, {
      req(is_fitting())
      cur <- input$calib_tabs
      if (is.null(cur) || !nzchar(cur)) {
        return()
      }
      idx <- match(cur, calib_tab_order, nomatch = 0L)
      if (idx < 1L || idx >= length(calib_tab_order)) {
        return()
      }
      nav_select(
        id = "calib_tabs",
        select = calib_tab_order[[idx + 1L]],
        session = session
      )
    })

    observeEvent(input$calib_prev, {
      req(is_fitting())
      cur <- input$calib_tabs
      if (is.null(cur) || !nzchar(cur)) {
        return()
      }
      idx <- match(cur, calib_tab_order, nomatch = 0L)
      if (idx <= 1L) {
        return()
      }
      nav_select(
        id = "calib_tabs",
        select = calib_tab_order[[idx - 1L]],
        session = session
      )
    })

    ## Back / Next mirror tab position (user can also click tab headers).
    observe({
      req(is_fitting())
      cur <- input$calib_tabs
      if (is.null(cur) || !nzchar(cur)) {
        return()
      }
      idx <- match(cur, calib_tab_order, nomatch = 0L)
      if (idx < 1L) {
        return()
      }
      if (idx <= 1L) {
        shinyjs::disable("calib_prev")
      } else {
        shinyjs::enable("calib_prev")
      }
      if (idx >= length(calib_tab_order)) {
        shinyjs::hide("calib_next_wrap", anim = FALSE)
      } else {
        shinyjs::show("calib_next_wrap", anim = FALSE)
      }
    })

    ## First period: only the + placeholder tab exists until we insert Period A.
    observe(
      {
        req(is_exploration())
        req(!is.null(input$parameters_panel))
        if (n_par() > 0L) {
          return()
        }
        insert_new_period_tab()
      },
      priority = 10L
    )

    ## Further periods: user moved from a real period tab to the trailing + tab.
    observeEvent(input$parameters_panel,
      {
        req(is_exploration())
        if (!identical(input$parameters_panel, period_plus_value)) {
          return()
        }
        if (n_par() < 1L) {
          return()
        }
        if (isTRUE(exploration_suppress_plus_add())) {
          return()
        }
        prev_np <- last_exploration_non_plus_panel()
        if (is.null(prev_np) || identical(prev_np, period_plus_value)) {
          return()
        }
        insert_new_period_tab()
      },
      ignoreNULL = TRUE,
      priority = 0L
    )

    ## Remember the last non-+ panel so we only add a period on deliberate + selection.
    observe({
      req(is_exploration())
      cur <- input$parameters_panel
      if (is.null(cur) || !nzchar(as.character(cur[[1L]]))) {
        return()
      }
      cur <- as.character(cur[[1L]])
      if (!identical(cur, period_plus_value)) {
        last_exploration_non_plus_panel(cur)
      }
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

    ## Remove a period via the x on the tab (JS sets `period_tab_close_req`).
    observeEvent(input$period_tab_close_req,
      {
        req(is_exploration())
        ev <- input$period_tab_close_req
        if (is.null(ev) || is.null(ev$id)) {
          return()
        }
        tid <- as.character(ev$id)[[1L]]
        if (!nzchar(tid) || identical(tid, period_plus_value)) {
          return()
        }
        if (!tid %in% active_par()) {
          return()
        }
        if (n_par() <= 1L) {
          showNotification("Keep at least one period.", type = "warning")
          return()
        }
        active_par(setdiff(active_par(), tid))
        n_par(n_par() - 1L)
        nav_remove(id = "parameters_panel", target = tid)
        nav_select(
          id = "parameters_panel",
          select = dplyr::last(active_par()),
          session = session
        )
        session$onFlushed(
          function() {
            sync_exploration_period_tab_labels()
          },
          once = TRUE
        )
      },
      ignoreNULL = TRUE
    )

    ## Run scenario: fresh run_simex; store under selected name (overwrites same name)
    observeEvent(input$run_scenario, {
      req(is_exploration())
      nm <- scenario_sel_str(input$scenario_select)
      if (!nzchar(nm)) {
        showNotification(
          "Add or select a scenario name before running.",
          type = "warning"
        )
        return()
      }
      out <- run_model_from_inputs()
      if (is.null(out)) {
        return()
      }
      cur <- scenarios()
      cur[[nm]] <- out
      scenarios(cur)
      nav_select(
        id = "main_plots_nav",
        select = "Timeline",
        session = session
      )
    })

    ## Remove selected scenario: drop saved runs from `scenarios()` and/or clear a
    ## not-yet-saved pending name; dropdown + selection refresh in `observe()`.
    observeEvent(input$remove_saved_scenario, {
      req(is_exploration())
      nm <- scenario_sel_str(input$scenario_select)
      if (!nzchar(nm)) {
        showNotification("Pick a scenario to remove.", type = "warning")
        return()
      }
      cur <- scenarios()
      if (nm %in% names(cur)) {
        cur[[nm]] <- NULL
        scenarios(cur)
      }
      pend <- explore_scenario_pending()
      if (!is.null(pend) && identical(nm, pend)) {
        explore_scenario_pending(NULL)
      }
    })

    observeEvent(input$explore_add_scenario, {
      req(is_exploration())
      scenario_modal_context("exploration")
      shiny::showModal(scenario_add_modal())
    })

    observeEvent(input$fit_add_scenario, {
      req(is_fitting())
      scenario_modal_context("fitting")
      shiny::showModal(scenario_add_modal())
    })

    observeEvent(input$scenario_modal_confirm, {
      ctx <- scenario_modal_context()
      raw_nm <- input$scenario_modal_new_name
      nm <- if (is.null(raw_nm) || length(raw_nm) == 0L) {
        ""
      } else {
        trimws(as.character(raw_nm[[1L]]))
      }
      if (!nzchar(nm)) {
        showNotification("Enter a scenario name.", type = "warning")
        return()
      }
      if (nm %in% names(scenarios())) {
        showNotification(
          paste0(
            "The name \"", nm, "\" is already used. ",
            "Choose another name or select that scenario to edit it."
          ),
          type = "warning"
        )
        return()
      }
      if (identical(ctx, "exploration")) {
        po <- explore_scenario_pending()
        if (!is.null(po) && identical(nm, po)) {
          showNotification(
            paste0(
              "The name \"", nm, "\" is already the pending scenario. ",
              "Select it in the list or choose another name."
            ),
            type = "warning"
          )
          return()
        }
      } else {
        po <- fit_scenario_pending()
        if (!is.null(po) && identical(nm, po)) {
          showNotification(
            paste0(
              "The name \"", nm, "\" is already the pending scenario. ",
              "Select it in the list or choose another name."
            ),
            type = "warning"
          )
          return()
        }
      }
      nms <- names(scenarios())
      ch <- unique(c(nms, nm))
      ch <- ch[vapply(ch, nzchar, logical(1))]
      shiny::removeModal()
      if (identical(ctx, "exploration")) {
        explore_scenario_pending(nm)
        shiny::updateSelectizeInput(
          session,
          "scenario_select",
          choices = stats::setNames(ch, ch),
          selected = nm,
          server = TRUE
        )
      } else {
        fit_scenario_pending(nm)
        shiny::updateSelectizeInput(
          session,
          "fitting_scenario_select",
          choices = simex_selectize_choices(ch),
          selected = nm,
          server = TRUE
        )
      }
    })

    ## Exploration: clear pending when switching to another saved name; load saved
    ## scenario periods when the selection is a stored run.
    observeEvent(input$scenario_select, {
      req(is_exploration())
      nm <- scenario_sel_str(input$scenario_select)
      pend <- explore_scenario_pending()
      nms <- names(scenarios())
      if (nzchar(nm) && nm %in% nms && !is.null(pend) && !identical(nm, pend)) {
        explore_scenario_pending(NULL)
      }
      if (!nzchar(nm)) {
        return()
      }
      sx <- scenarios()[[nm]]
      if (is.null(sx)) {
        return()
      }
      exploration_load_simex(sx)
    }, ignoreNULL = TRUE)

    ## Scenario dropdown: saved names plus one not-yet-saved name from Add
    ## (`explore_scenario_pending`), without re-adding a name after Remove.
    observe({
      req(is_exploration())
      nms <- names(scenarios())
      pend <- explore_scenario_pending()
      if (!is.null(pend) && pend %in% nms) {
        explore_scenario_pending(NULL)
        pend <- explore_scenario_pending()
      }
      sel <- scenario_sel_str(isolate(input$scenario_select))
      ch <- unique(c(
        nms,
        if (!is.null(pend) && nzchar(pend) && !pend %in% nms) pend
      ))
      ch <- ch[vapply(ch, nzchar, logical(1))]
      if (length(ch) == 0L) {
        shiny::updateSelectizeInput(
          session,
          "scenario_select",
          choices = character(0),
          selected = NULL,
          server = TRUE
        )
        return()
      }
      ## Prefer pending (not-yet-saved) over `isolate(input)` — after Add, input
      ## can still be the previous value for one flush, which would revert choice.
      new_sel <- if (
        !is.null(pend) &&
        nzchar(pend) &&
        pend %in% ch &&
        !pend %in% nms
      ) {
        pend
      } else if (nzchar(sel) && sel %in% ch) {
        sel
      } else {
        ch[[length(ch)]]
      }
      shiny::updateSelectizeInput(
        session,
        "scenario_select",
        choices = simex_selectize_choices(ch),
        selected = new_sel,
        server = TRUE
      )
    })

    ## Enable Remove when selection is saved or matches pending (not-yet-saved).
    observe({
      req(is_exploration())
      nms <- names(scenarios())
      nm <- scenario_sel_str(input$scenario_select)
      pend <- explore_scenario_pending()
      can_remove <- nzchar(nm) &&
        (nm %in% nms || (!is.null(pend) && nzchar(pend) && identical(nm, pend)))
      if (can_remove) {
        shinyjs::enable("remove_saved_scenario")
      } else {
        shinyjs::disable("remove_saved_scenario")
      }
    })

    ## Fitting: clear pending when switching to another saved name; load Calibration
    ## from earliest period when the selection is a stored scenario.
    observeEvent(input$fitting_scenario_select, {
      req(is_fitting())
      nm <- scenario_sel_str(input$fitting_scenario_select)
      pend <- fit_scenario_pending()
      nms <- names(scenarios())
      if (nzchar(nm) && nm %in% nms && !is.null(pend) && !identical(nm, pend)) {
        fit_scenario_pending(NULL)
      }
      if (!nzchar(nm)) {
        return()
      }
      sx <- scenarios()[[nm]]
      if (is.null(sx)) {
        return()
      }
      pm <- sx$pars
      if (is.null(pm) || !is.matrix(pm) || ncol(pm) < 1L) {
        return()
      }
      j <- which.min(as.numeric(colnames(pm)))
      shaped <- args_to_simex_shiny_shape(as.list(pm[[1L, j]]))
      apply_shaped_pars_to_tab("fit", shaped)
    }, ignoreNULL = TRUE)

    ## Fitting / Scenarios tab: same selectize + pending + delete as Exploration.
    observe({
      req(is_fitting())
      nms <- names(scenarios())
      pend <- fit_scenario_pending()
      if (!is.null(pend) && pend %in% nms) {
        fit_scenario_pending(NULL)
        pend <- fit_scenario_pending()
      }
      sel <- scenario_sel_str(isolate(input$fitting_scenario_select))
      ch <- unique(c(
        nms,
        if (!is.null(pend) && nzchar(pend) && !pend %in% nms) pend
      ))
      ch <- ch[vapply(ch, nzchar, logical(1))]
      if (length(ch) == 0L) {
        shiny::updateSelectizeInput(
          session,
          "fitting_scenario_select",
          choices = character(0),
          selected = NULL,
          server = TRUE
        )
        return()
      }
      new_sel <- if (
        !is.null(pend) &&
        nzchar(pend) &&
        pend %in% ch &&
        !pend %in% nms
      ) {
        pend
      } else if (nzchar(sel) && sel %in% ch) {
        sel
      } else {
        ch[[length(ch)]]
      }
      shiny::updateSelectizeInput(
        session,
        "fitting_scenario_select",
        choices = simex_selectize_choices(ch),
        selected = new_sel,
        server = TRUE
      )
    })

    observeEvent(input$fitting_remove_saved_scenario, {
      req(is_fitting())
      nm <- scenario_sel_str(input$fitting_scenario_select)
      if (!nzchar(nm)) {
        showNotification("Pick a scenario to remove.", type = "warning")
        return()
      }
      cur <- scenarios()
      if (nm %in% names(cur)) {
        cur[[nm]] <- NULL
        scenarios(cur)
      }
      pend <- fit_scenario_pending()
      if (!is.null(pend) && identical(nm, pend)) {
        fit_scenario_pending(NULL)
      }
    })

    observe({
      req(is_fitting())
      nms <- names(scenarios())
      nm <- scenario_sel_str(input$fitting_scenario_select)
      pend <- fit_scenario_pending()
      can_remove <- nzchar(nm) &&
        (nm %in% nms || (!is.null(pend) && nzchar(pend) && identical(nm, pend)))
      if (can_remove) {
        shinyjs::enable("fitting_remove_saved_scenario")
      } else {
        shinyjs::disable("fitting_remove_saved_scenario")
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

        ## Show progress only after the client flushes; otherwise the long
        ## fit_simex() call blocks before the browser can paint (busy overlay).
        shinyjs::disable("run_fit")
        shinyjs::show("fit_progress_container", anim = FALSE)

        ## Positional callback: some session proxies reject `fun =` (unused arg).
        session$onFlushed(
          function() {
            on.exit(
              {
                shinyjs::hide("fit_progress_container", anim = FALSE)
                shinyjs::enable("run_fit")
              },
              add = TRUE
            )

            n_steps_total <- as.integer(settings$n_steps + settings$burnin)
            n_chains_fit <- as.integer(settings$n_chains)
            if (n_chains_fit < 1L) {
              n_chains_fit <- 1L
            }
            total_sampler_steps <- n_chains_fit * n_steps_total
            session$sendCustomMessage("fit_monty_progress", list(reset = TRUE))

            samples <- tryCatch(
              withCallingHandlers(
                fit_simex(fit_df, parameters, priors, settings),
                message = function(cond) {
                  txt <- conditionMessage(cond)
                  re_txt <- paste0(
                    "^MONTY-PROGRESS: chain: ([0-9]+), step: ([0-9]+)\\s*$"
                  )
                  m <- stringr::str_match(txt, re_txt)
                  if (isTRUE(is.na(m[1L, 1L]))) {
                    return()
                  }
                  ch <- as.integer(m[1L, 2L])
                  st <- as.integer(m[1L, 3L])
                  if (anyNA(c(ch, st))) {
                    return()
                  }
                  ## Serial chains: overall fraction across all chains × steps.
                  done_steps <- (ch - 1L) * n_steps_total + st
                  pct <- if (total_sampler_steps > 0L) {
                    100 * done_steps / total_sampler_steps
                  } else {
                    0
                  }
                  session$sendCustomMessage(
                    "fit_monty_progress",
                    list(pct = max(0, min(100, pct)))
                  )
                  invokeRestart("muffleMessage")
                }
              ),
              error = function(e) {
                showNotification(conditionMessage(e), type = "error")
                NULL
              }
            )
            if (is.null(samples)) {
              return()
            }

            session$sendCustomMessage("fit_monty_progress", list(phase = "sim"))

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
            ## onFlushed runs outside a reactive consumer; read reactives with isolate().
            sc <- isolate(scenarios())
            sc[["Fitted"]] <- sx
            scenarios(sc)
            nav_select(
              id = "main_plots_nav",
              select = "Timeline",
              session = session
            )
          },
          once = TRUE
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
        showNotification(
          "Add or select a scenario name before running.",
          type = "warning"
        )
        return()
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
