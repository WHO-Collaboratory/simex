#' This function launches the simex Shiny app .
#'
#' @importFrom shinyMatrix matrixInput
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
    frac_symp = "Fraction of cases that eventually develop symptoms",
    agestrat = "Age-stratified parameters",
    ifr = "Infection fatality rate",
    hosp_mortality = "Proportion of hospitalisations that die",
    hosp_protection_death = "Proportion of deaths averted by hospitalisation",
    hosp_duration = "Duration of hospitalisation (days)",
    hosp_capacity = "Hospital capacity (per 100k population)",
    comm_mortality = "Proportions of community infections that die",
    vax_rate = "Vaccination rate (population proportion per day)",
    vax_infectiousness = "Reduction in infectiousness due to vaccination",
    vax_infection = "Protection against infection due to vaccination",
    vax_hosp = "Protection against hospitalisation due to vaccination",
    vax_death = "Protection against death due to vaccination",
    isolation_adherence = "Proportion adherence to isolation guidlines",
    isolation_effectiveness = "Proportion of contacts reducted by isolation",
    isolation_delay = "Delay from symptom onset to isolation",
    social_distancing = "Social Distancing",
    vax_prioritised = "Are elderly individuals vaccinated first?",
    hosp_prioritised = paste("Are elderly individuals given priority",
                             "when hospital bed capacity is exceeded?")
  )

  ## get default values for parameters
  defs <- map(formals(get_parameters), get_default, "value")

  ## collapse age-stratified parameters into one matrix
  agestrat <- lengths(defs) == nrow(cdat[[1]]$pop)
  agestrat_nms <- names(agestrat)[agestrat]
  defs$agestrat <- do.call(cbind, defs[agestrat])
  dimnames(defs$agestrat) <- list(
    "Age" = get_age_cat(),
    "Variable" = labs[colnames(defs$agestrat)]
  )
  defs[agestrat] <- NULL

  ## convert to matrix for matrix input
  defs$social_distancing <- matrix(
    defs$social_distancing, nrow = 1,
    dimnames = list("Reduction", names(defs$social_distancing))
  )

  ## re-order
  defs <- defs[order(match(names(defs), names(labs)))]

  ## Define the UI
  ui <- fluidPage(
    titlePanel(p(strong("Simex"))),
    sidebarLayout(
      sidebarPanel(
        uiOutput("defaults"),
        actionButton("submit", "Submit")
      ),
      mainPanel(
        plotOutput("timeline")
      )
    )
  )

  ## Define the server
  server <- function(input, output) {

    simex <- eventReactive(
      input$submit,
      {
        ## so we have a modifieable list
        pars <- reactiveValuesToList(input)

        ## reshape social distancing and agestrat to useable formats
        pars$social_distancing <- setNames(
          as.numeric(unlist(pars$social_distancing)),
          colnames(pars$social_distancing)
        )
        agestrat <- setNames(map(data.frame(pars$agestrat), as.numeric), agestrat_nms)
        pars$agestrat <- NULL
        pars <- c(pars, agestrat)
        pars$hosp_capacity <- pars$hosp_capacity/1e5

        ## remove input values that are not arguments of get_parameters
        pars[setdiff(names(pars), names(defs))] <- NULL

        ## run model with updated parameters
        run_model(do.call(get_parameters, pars))
      }
    )

    output$defaults <- renderUI(
      imap(
        defs,
        function(value, name) {
          if(name == "iso3") selectInput(name, labs[name], names(cdat))
          else if(name == "hosp_capacity") numericInput(name, labs[name], value*1e5)
          else if(is.numeric(value)) {
            if(is.matrix(value)) shinyMatrix::matrixInput(name, labs[name], value)
            else if(!is.null(names(value)))
              do.call(
                fluidRow,
                unname(imap(value, ~ column(2, numericInput(.y, labs[.y], .x))))
              )
            else if(length(value) == 1) numericInput(name, labs[name], value)
          } else if(is.logical(value))
            checkboxInput(name, labs[name], value)
        }
      )
    )

    output$timeline <- renderPlot(
      plot(simex(), base_size = 20, show_hosp_capacity = TRUE),
      height = 700
    )

  }

  ## Run the Shiny app
  shinyApp(ui = ui, server = server)

}
