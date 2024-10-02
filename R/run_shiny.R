#' This function launches the simex Shiny app .
#'
#' @importFrom shinyMatrix matrixInput
#' @importFrom shinyjs click useShinyjs
#' @importFrom shinyWidgets setBackgroundColor switchInput radioGroupButtons
#'
#' @author Finlay Campbell
#'
#' @export
#'
run_shiny <- function() {

  ## define labels
  labs <- c(
    iso3 = "Country code",
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
    vax_rate = "Vaccination rate (% population per day)",
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
    hosp_prioritised = paste("Are elderly individuals given priority",
                             "when hospital bed capacity is exceeded?")
  )

  ## get default values for parameters
  simex_defaults <- map(formals(get_parameters), get_default, "value")

  ## define age-stratified varnames that get grouped into matrix input
  agestrat_nms <- c("ifr", "hosp_mortality", "hosp_duration", "comm_mortality")

  ## define vaccination varnames that get grouped into matrix input
  vax_nms <- c("vax_infectiousness", "vax_infection", "vax_hosp", "vax_death")

  ## variable names expressed in percent
  percent_nms <- c(vax_nms, "ifr", "hosp_mortality", "frac_symp",
                   "hosp_protection_death", "comm_mortality",
                   "isolation_adherence", "isolation_effectiveness", "vax_rate")

  ## generate shiny output for a given parameter tab
  simex_to_shiny <- function(simex_input, tab_id) {

    ## keep only variables in default simex input
    simex_input[setdiff(names(simex_input), names(simex_defaults))] <- NULL

    ## adjust units
    for(i in percent_nms) simex_input[[i]] <- simex_input[[i]]*100
    simex_input$hosp_capacity <- simex_input$hosp_capacity*1e5

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
      simex_input$social_distancing, nrow = 1,
      dimnames = list("Reduction (%)", str_to_title(names(simex_input$social_distancing)))
    )

    ## convert vax parameters to matrix for matrix input
    simex_input$vax <- matrix(
      unlist(simex_input[vax_nms]), nrow = 1,
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
        agestrat <- setNames(map(data.frame(par$agestrat), as.numeric), agestrat_nms)
        par$agestrat <- NULL
        par <- c(par, agestrat)

        ## get vax parameters
        vax <- setNames(map(data.frame(par$vax), as.numeric), vax_nms)
        par$vax <- NULL
        par <- c(par, vax)

        ## adjust units
        for(i in percent_nms) par[[i]] <- par[[i]]/100
        par$hosp_capacity <- par$hosp_capacity/1e5

        ## remove input values that are not arguments of get_parameters
        par[setdiff(names(par), names(simex_defaults))] <- NULL

        ## pass to get parameters
        do.call(get_parameters, par)
      }
    )

    ## order by day
    out[order(as.numeric(names(out)))]

  }

  ## function for extracting active values of a parameter
  extract_active_par <- function(input, name, active_par) {
    x <- unlist(reactiveValuesToList(input)[grep(name, names(input))])
    x <- x[grep(paste(active_par(),collapse="|"), names(x))]
    return(x)
  }

  ## rename function
  rn <- function(name, i) paste0(i, "__", name)

  get_tabname <- function(n) {
    x <- if(n <= 26) LETTERS[n]
         else apply(expand.grid(LETTERS, LETTERS), 1, paste0, collapse = "")[n]
    paste("Period", x)
  }

  ## function for generating an input panel for one panel ID
  make_input <- function(value, name, id) {
    if(name == "iso3") selectInput(rn(name, id), labs[name], names(cdat), selected = value)
    ## else if(name == "hosp_capacity") numericInput(rn(name, id), labs[name], value*1e5)
    else if(is.numeric(value)) {
      if(is.matrix(value)) matrixInput(rn(name, id), labs[name], value)
      else if(!is.null(names(value)))
        do.call(
          fluidRow,
          unname(imap(value, ~ column(2, numericInput(rn(.y, id), labs[.y], .x))))
        )
      else if(length(value) == 1) numericInput(rn(name, id), labs[name], value)
    } else if(is.logical(value))
      checkboxInput(rn(name, id), labs[name], value)
  }

  ## define color palette
  background_col <- "#ffffff"
  sidebar_col <- "#fefae0"
  button_col <- "#dda15e"
  button_active_col <- "#bc6c25"
  button2_col <- "#D2596A"
  button2_active_col <- "#B43546"

  ## Define the UI
  ui <- page_sidebar(

    useShinyjs(),
    setBackgroundColor(background_col),
    tags$head(tags$style(HTML(paste(
                     paste0("#sidebar{background-color:", sidebar_col, "}"),

                     paste0("#run_scenario{background-color:", button2_col, "}"),
                     paste0("#run_scenario:hover{background-color:", button2_active_col, "}"),

                     paste0("#save_scenario{background-color:", button2_col, "}"),
                     paste0("#save_scenario:hover{background-color:", button2_active_col, "}"),

                     paste0("#reset{background-color:", button_col, "}"),
                     paste0("#reset:hover{background-color:", button_active_col, "}"),

                     paste0("#add_period{background-color:", button_col, "}"),
                     paste0("#add_period:hover{background-color:", button_active_col, "}"),

                     paste0("#remove_period{background-color:", button_col, "}"),
                     paste0("#remove_period:hover{background-color:", button_active_col, "}")
                   )))
              ),

### next two lines for class - use class attribute (.inline instead of #inline)
    tags$head(
           tags$style(
                  type="text/css",
                  ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } .inline .form-group { display: table-row;} p.indent {margin-right: 10px}")
         ),

    title = h4(strong(
      em("simex:"),
      "simulating pandemics and public health interventions")
      ),

    sidebar = sidebar(

      width = "40%",

      navset_card_underline(

        nav_panel(
          title = "Introduction",
          strong("Introduction to simex"),
          p("simex is a simulation excercise tool for epidemic and pandemic preparedness that lets you explore different outbreak scenarios and the effect of different public health interventions. The text below will give a brief overview of how to use the tool."),
          strong("Running a default scenario"),
          p("A scenario represents a single run of the simulation from beginning to end. To run a simulation using the default settings, navigate to the", em("Entry tab"), "and click ", em("Run Scenario"), ". You will see the simulation outputs in the ", em("Timeline"), "panel on the right. The compartments are as follows:"),
          htmlOutput("compartments_description"),
          p("The subscript u and v correspond to unvaccinated and vaccinated populations, respectively. You can toggle whether you want to see prevalence or incidence using the buttons above the plot, and you can toggle whether you want to hide/show the vaccinated and unvaccinated populations using the buttons in the bottom right."),
          strong("Running a custom scenario"),
          p("To run a scenario under your own parameter settings, simply change any of the values in the parameter tab as you desire and press the ", em("Run Scenario"), "button again and the plot on the right will update."),
          strong("Changing parameter values over time"),
          p("The settings so far run a single set of parameters over the entire time period. To simulate a scenario where the parameters change at a given point in the pandemic, use the ", em("Add Period"), "button. This will add a new set of parameters, associated with a given start day. Try adding a new period and changing the reproduction number from 3 to 5 with a start day of 75. Then run the scenario and see how the increased reproduction number (e.g. due to the introduction of a new variant) changes the course of the pandemic. You can add as many periods as you would like, and can navigate between the parameter settings of each period by clicking on the respective tabs. You can also delete a period using the ", em("Remove Period"), "button."),
          strong("Comparing different scenarios"),
          p("You can also compare different scenarios to see e.g. what the effect of an intervention is. To do so, define your first scenario (with as a many periods as you like), provid a Scenario name and save it using the ", em("Save Scenario"), "button. Then define a second scenario, name it and and once again save it. Now navigate to the ", em("Comparison"), "panel and see how the total number of cases across age groups differ. You can toggle whether you want to see cases, hospitalisations or deaths. Try comparing two scenarios, one with a reproduction number of 2 and one with 5.")
        ),

        nav_panel(
          title = "Entry",
          textInput(inputId = "scenario_name", label = "Scenario name", "Baseline"),
          div(
            style = "display: flex; align-items: stretch; margin-top: 0px",
            actionButton("run_scenario", "Run Scenario", width = "50%",
                         style = "margin-right:2px; margin-left: 2px"),
            actionButton("save_scenario", "Save Scenario", width = "50%",
                         style = "margin-right:2px; margin-left: 2px")
          ),
          div(
            style = "display: flex; align-items: stretch; margin-top: -20px",
            actionButton("add_period", "Add Period", width = "33%",
                         style = "margin-right:2px; margin-left: 2px"),
            actionButton("remove_period", "Remove Period", width = "33%",
                         style = "margin-right:2px; margin-left: 2px"),
            actionButton("reset", "Reset", width = "33%",
                         style = "margin-right:2px; margin-left: 2px")
          ),
          navset_card_underline(id = "parameters_panel")
        )

      )


    ),

    navset_card_underline(

      nav_panel(
        title = "Timeline",

        radioGroupButtons( # or radioGroupButtons
          inputId = "timeline_what",
          selected = "Prevalence",
          label = NULL,
          choices = c("Prevalence", "Incidence")
        ),

        highchartOutput("timeline")

      ),

      nav_panel(
        title = "Comparison",
        radioGroupButtons( # or radioGroupButtons
          inputId = "comparison_what",
          selected = "Cases",
          label = NULL,
          choices = c("Cases", "Hospitalisations", "Deaths")
        ),

        highchartOutput("endpoint")
      )

    )

  )


  ## FRAC SYMP DOESNT SEEM TO BE WORKING? ##
  ## Maybe something odd with hospital admissions when shortening gentime?

  ## Variant escape from immunity?
  ## Waning immunity / birth/death?

  ## Define the server
  server <- function(input, output, session) {

    ## saved models
    scenarios <- reactiveVal()

    ## number and names of active parameters
    n_par <- reactiveVal(0)
    active_par <- reactiveVal()

    ## total number of parameters
    total_par <- reactiveVal(0)

    ## initiate server with clicking add_period button
    o <- observe({
      click("add_period")
      click("run_scenario")
      ## destroy observer as it has no use after initial button click
      o$destroy()
    })

    observeEvent(
      input$add_period,
      {

        ## add new id and update number of parameters
        active_par(c(active_par(), paste0(sample(letters, 10, TRUE), collapse = "")))
        n_par(n_par()+1)
        total_par(total_par()+1)

        ## define active max day for default new day value
        days <- extract_active_par(input, "day", active_par)
        start_day <- if(length(days) == 0) 1 else max(days) + 50

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
                  if(length(days) == 0) simex_defaults
                  ## otherwise use most recent tab (have to remove last active tab
                  ## because it hasn't been initialised yet)
                  else last(shiny_to_simex(input, head(active_par(), -1))),
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
      if(n_par() != 1) {
        ## remove parameter set
        active_par(setdiff(active_par(), input$parameters_panel))
        n_par(n_par()-1)
        nav_remove(id = "parameters_panel", target = input$parameters_panel)
        nav_select(
          id = "parameters_panel",
          select = last(active_par()),
          session = session
        )
      }
    })

    ## run the model on a scenario
    simex <- eventReactive(
      input$run_scenario,
      {
        ## if input not initiated call default values
        if(!any(grepl("agestrat", names(input)))) {
          parlist <- get_parameters()
        } else {
          parlist <- shiny_to_simex(input, active_par())
          ## checks
          start_days <- as.numeric(names(parlist))
          validate(
            need(any(start_days == 1), "One period must start on day 1!"),
            need(all(table(start_days) == 1), "Periods can't have the same start day!")
          )
        }
        ## run the model
        run_model(parlist)
      }
    )

    ## save the scenario
    observeEvent(
      input$save_scenario,
      {
        ## validate(need(
        ##   !input$scenario_name %in% names(scenarios()),
        ##   "Scenario names must be unique!"
        ## ))
        ## we update the scenarios object of saved scenarios
        scenarios(
          ## appending to list of saved objects
          append(
            ## call current set of scenarios
            scenarios(),
            ## run model with appropriate scenario name
            setNames(list(run_model(shiny_to_simex(input, active_par()))), input$scenario_name)
          )
        )
      }
    )

    ## reset
    observeEvent(
      input$reset,
      session$reload()
    )

    ## timeline plot
    output$timeline <- renderHighchart(
      plot(simex(),
           what = tolower(input$timeline_what),
           base_size = 20,
           show_hosp_capacity = TRUE,
           type = "highchart")
      ## height = function() 0.7*session$clientData$output_timeline_width
    )

    ## comparison plot
    output$endpoint <- renderHighchart(
      vis_comparison(
        scenarios(), format = "endpoint",
        type = "highchart",
        show_compartment = c(Cases = "E", Hospitalisations = "H", Deaths = "D")[input$comparison_what]
      )
    )

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
