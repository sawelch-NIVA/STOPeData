#' landing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon
#' @importFrom shinyjs disabled
#' @importFrom bslib layout_column_wrap card card_body
#' @importFrom bslib input_task_button
#' @export
mod_landing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(
          content_file = "inst/app/www/md/intro_landing.md"
        ),
        layout_column_wrap(
          width = "300px",
          div(
            style = "display: flex; flex-direction: column; align-items: center;",
            h6("Enter data from a paper or dataset manually."),
            input_task_button(
              id = ns("narrative_manual"),
              label = "Enter data manually...",
              icon = icon("keyboard"),
              wdith = "300px"
            )
          ),
          div(
            style = "display: flex; flex-direction: column; align-items: center;",
            h6("Extract data from a pdf using an LLM."),
            input_task_button(
              id = ns("narrative_llm"),
              label = "Import using LLM...",
              icon = icon("brain"),
              wdith = "300px"
            )
          ),
          div(
            style = "display: flex; flex-direction: column; align-items: center;",
            h6("Import data from a structured format (planned)"),
            input_task_button(
              id = ns("structured"),
              label = "Import structured data",
              icon = icon("table"),
              wdith = "300px"
            ) |>
              disabled()
          ),
          div(
            style = "display: flex; flex-direction: column; align-items: center;",
            h6("Load dummy data for testing purposes."),
            input_task_button(
              id = ns("test_dummy"),
              label = "Test with dummy data",
              icon = icon("vial-circle-check"),
              type = "warning",
              wdith = "300px"
            )
          )
        ),
        tags$figure(
          tags$img(
            style = "display: block; margin: auto;",
            src = "www/app_mapp.png",
            align = "center",
            width = 800,
            alt = "Illustration of STOP eData app workflow."
          ),
          tags$figcaption("Illustration of STOP eData app workflow.")
        )
      )
    )
  )
}

#' landing Server Functions
#'
#' @param parent_session The parent session object to access main navbar
#' @noRd
#' @importFrom shiny moduleServer updateNavbarPage
#' @export
mod_landing_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Navigation observers ----

    ## observe: Navigate to LLM extract when LLM button clicked ----
    # Upstream: input$narrative_llm button click
    # Downstream: Updates main navbar to LLM extract tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "00-llm-extract"
      )
    }) |>
      bindEvent(input$narrative_llm)

    ## observe: Navigate to campaign when manual entry button clicked ----
    # Upstream: input$narrative_manual button click
    # Downstream: Updates main navbar to campaign tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "01-campaign"
      )
    }) |>
      bindEvent(input$narrative_manual)

    ## observe: Navigate to structured import when button clicked (future) ----
    # Upstream: input$structured button click
    # Downstream: Updates main navbar to appropriate tab (TBD)
    # Note: Currently disabled - will need to define target tab when implemented
    observe({
      # TODO: Define target tab for structured data import
      # updateNavbarPage(
      #   session = parent_session,
      #   inputId = "main-page",
      #   selected = "XX-structured"
      # )
    }) |>
      bindEvent(input$structured)

    ## observe: Load dummy data for testing ----
    # Upstream: input$test_dummy button click
    # Downstream: TBD - load dummy data into reactiveValues
    observe({
      # TODO: Implement dummy data loading logic
    }) |>
      bindEvent(input$test_dummy)
  })
}

## To be copied in the UI
# mod_landing_ui("landing_1")

## To be copied in the server
# mod_landing_server("landing_1")
