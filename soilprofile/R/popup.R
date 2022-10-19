confirmationPopup <- function(){
if (interactive()) {

  library("shiny")
  library("shinyWidgets")


  ui <- fluidPage(
    tags$h1("Confirm sweet alert"),
    actionButton(
      inputId = "go",
      label = "Launch confirmation dialog"
    ),
    verbatimTextOutput(outputId = "res")
  )

  server <- function(input, output, session) {

    observeEvent(input$go, {
      confirmSweetAlert(
        session = session, inputId = "myconfirmation", type = "warning",
        title = "Want to confirm ?", danger_mode = TRUE
      )
    })

    output$res <- renderPrint(input$myconfirmation)

  }

  shinyApp(ui = ui, server = server)

}
}
