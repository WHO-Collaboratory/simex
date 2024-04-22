ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("controller", "Controller", 1:3, 1)
    ),
    mainPanel(
      tabsetPanel(
        id = "hidden_tabs",
        # Hide the tab values.
        # Can only switch tabs by using `updateTabsetPanel()`
        type = "hidden",
        tabPanelBody("panel1", "Panel 1 content"),
        tabPanelBody("panel2", "Panel 2 content"),
        tabPanelBody("panel3", "Panel 3 content")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
  })
}

## if (interactive()) {
##   shinyApp(ui, server)
## }
