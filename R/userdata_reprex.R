library(shiny)

# Module UI ----
mod_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Module: Set User Data"),
    textInput(ns("user_name"), "Enter your name:", value = ""),
    actionButton(ns("save_name"), "Save to session$userData", class = "btn-primary")
  )
}

# Module Server ----
mod_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # When button is clicked, save to session$userData ----
    observe({
      # Make session$userData reactive if it isn't already
      if (!is.reactivevalues(session$userData)) {
        session$userData <- reactiveValues()
      }

      # Set the reactive value
      session$userData$ENTERED_BY <- input$user_name

      showNotification(
        paste("Saved:", input$user_name, "to session$userData"),
        type = "message"  # or "success", "warning", "error"
      )
    }) |> bindEvent(input$save_name)

    # Return the reactive value for use elsewhere
    reactive({
      session$userData$ENTERED_BY
    })
  })
}

# Main UI ----
ui <- fluidPage(
  titlePanel("session$userData Reactive Example"),

  # Module UI
  mod_input_ui("input_module"),

  # Main app UI
  h3("Main App: Monitor User Data"),
  verbatimTextOutput("current_user"),
  textOutput("user_message")
)

# Main Server ----
server <- function(input, output, session) {

  # Initialize session$userData as reactiveValues ----
  session$userData <- reactiveValues(
    ENTERED_BY = "Not set yet"
  )

  # Call the module ----
  user_data <- mod_input_server("input_module")

  # Observer in main server that reacts to session$userData changes ----
  observe({
    cat("session$userData$ENTERED_BY changed to:", session$userData$ENTERED_BY, "\n")
  }) |> bindEvent(session$userData$ENTERED_BY)

  # Output that shows current value ----
  output$current_user <- renderText({
    paste("Current user in session$userData:", session$userData$ENTERED_BY)
  })

  # Output that reacts to changes ----
  output$user_message <- renderText({
    if (isTruthy(session$userData$ENTERED_BY) && session$userData$ENTERED_BY != "Not set yet") {
      paste("Hello,", session$userData$ENTERED_BY, "! (Updated at", Sys.time(), ")")
    } else {
      "Please enter your name in the module above."
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
