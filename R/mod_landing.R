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
    layout_column_wrap(
      width = 1 / 2,
      card(
        fill = TRUE,
        fillable = TRUE,
        card_body(
          fill = TRUE,
          fillable = TRUE,
          ## Info accordion ----
          info_accordion(
            content_file = "inst/app/www/md/intro_landing.md"
          ),
          layout_column_wrap(
            width = "300px",
            min_height = "200px",
            fill = FALSE,
            div(
              style = "display: flex; flex-direction: column; align-items: center;",
              h6("Enter data manually."),
              input_task_button(
                id = ns("narrative_manual"),
                label = "Enter data manually...",
                icon = icon("keyboard"),
                width = "300px"
              )
            ),
            div(
              style = "display: flex; flex-direction: column; align-items: center;",
              h6("Extract data using an LLM."),
              input_task_button(
                id = ns("narrative_llm"),
                label = HTML(paste(
                  bsicons::bs_icon("cpu"),
                  "Extract using LLM..."
                )),
                width = "300px"
              )
            ),
            div(
              style = "display: flex; flex-direction: column; align-items: center;",
              h6("Upload session data and continue an earlier entry"),
              input_task_button(
                id = ns("upload_zip"),
                label = "Upload session data",
                icon = icon("table"),
                width = "300px"
              )
            )
            # div(
            #   style = "display: flex; flex-direction: column; align-items: center;",
            #   h6("Load dummy data for testing purposes."),
            #   input_task_button(
            #     id = ns("test_dummy"),
            #     label = "Test with dummy data",
            #     icon = icon("vial-circle-check"),
            #     type = "warning",
            #     width = "300px"
            #   )
            # )
          )
        )
      ),
      card(
        fill = TRUE,
        fillable = TRUE,
        card_body(
          div(
            style = "margin-bottom: 10px;",
            style = "display: block !important; margin: auto !important; max-width: 100% !important; max-height: 500px !important;",
            info_accordion(
              content_file = "inst/app/www/md/whats_new.md"
            ),
            br(),
            tags$figure(
              tags$figcaption(
                "Illustration of STOP eData app workflow:"
              ),
              tags$img(
                style = "max-width: 100%",
                src = "www/app_mapp.png",
                alt = "Illustration of STOP eData app workflow."
              )
            )
          )
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
    # Downstream: Session reactiveValues populated with dummy data, navigate to campaign
    observe({
      populate_session_with_dummy_data(
        session = session,
        navigate_to = "01-campaign",
        parent_session = parent_session
      )
    }) |>
      bindEvent(input$test_dummy)
  })
}

## To be copied in the UI
# mod_landing_ui("landing_1")

## To be copied in the server
# mod_landing_server("landing_1")
