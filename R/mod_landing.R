#' landing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 p div icon updateNavbarPage moduleServer observe
#' @importFrom bslib card card_body layout_column_wrap input_task_button card_header
#' @importFrom shinyjs useShinyjs disabled
#' @export
mod_landing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Enable shinyjs ----
    useShinyjs(),

    # Main landing card ----
    card(
      card_body(
        info_accordion(content_file = "inst/app/www/md/intro_landing.md")
      )
    ),
    card(
      card_body(
        card_header("Get Started"),
        layout_column_wrap(
          width = 1 / 3,
          div(
            style = "display: flex; flex-direction: column; align-items: center; text-align: center;",
            p("I want to import data manually"),
            input_task_button(
              id = ns("go_to_campaign"),
              label = "Go to Campaign Module",
              icon = icon("keyboard")
            ) |>
              disabled()
          ),
          div(
            style = "display: flex; flex-direction: column; align-items: center; text-align: center;",
            p("I want to import data using an LLM"),
            input_task_button(
              id = ns("go_to_llm"),
              label = "Go to LLM Module",
              icon = icon("bolt")
            ) |>
              disabled()
          ),
          div(
            style = "display: flex; flex-direction: column; align-items: center; text-align: center;",
            p("I want to import a structured data format (planned)"),
            input_task_button(
              id = ns("go_to_structured"),
              label = "Go to Structured Data Module",
              icon = icon("database")
            ) |>
              disabled()
          )
        )
      )
    )
  )
}

#' landing Server Functions
#'
#' @noRd
#' @export
mod_landing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe: Go to campaign module ----
    # upstream: input$go_to_campaign click
    # downstream: main navbar page selection
    observe({
      print("Navigating to campaign module")
      updateNavbarPage(
        session = session,
        inputId = "main-page",
        selected = "01-campaign"
      )
    }) |>
      bindEvent(input$go_to_campaign, ignoreInit = TRUE)

    # observe: Go to LLM module ----
    # upstream: input$go_to_llm click
    # downstream: main navbar page selection
    observe({
      updateNavbarPage(session, "main-page", selected = "00-llm")
    }) |>
      bindEvent(input$go_to_llm, ignoreInit = TRUE)

    # observe: Go to structured data module (disabled) ----
    # upstream: input$go_to_structured click
    # downstream: main navbar page selection
    # observe({
    #   updateNavbarPage(session, "main-page", selected = "00-structured")
    # }) |>
    #   bindEvent(input$go_to_structured, ignoreInit = TRUE)
  })
}

## To be copied in the UI ----
# mod_landing_ui("landing_1")

## To be copied in the server ----
# mod_landing_server("landing_1")
