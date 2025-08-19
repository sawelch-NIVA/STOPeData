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
            style = "display: flex;",
            h6("Enter data from a paper or dataset manually."),
            input_task_button(
              id = ns("narrative_manual"),
              label = "Enter data manually...",
              icon = icon("keyboard"),
              wdith = "300px"
            )
          ),
          div(
            style = "display: flex;",
            h6("Extract data from a pdf using an LLM."),
            input_task_button(
              id = ns("narrative_llm"),
              label = "Import using LLM...",
              icon = icon("brain"),
              wdith = "300px"
            )
          ),
          div(
            style = "display: flex;",
            h6("Import data from a structured format (planned)"),
            input_task_button(
              id = ns("structured"),
              label = "Import structured data",
              icon = icon("table"),
              wdith = "300px"
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
  })
}

## To be copied in the UI
# mod_landing_ui("landing_1")

## To be copied in the server
# mod_landing_server("landing_1")
