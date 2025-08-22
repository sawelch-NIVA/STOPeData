#' structured UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_structured
#' @export
#'
#' @importFrom shiny NS tagList
mod_structured_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      info_accordion(content_file = "inst/app/www/md/intro_structured.md"),
      layout_column_wrap(
        width = "300px",
        min_height = "200px",
        fill = FALSE,
        div(
          style = "display: flex; flex-direction: column; align-items: center;",
          h6("Vannmiljø (API Call)"),
          input_task_button(
            id = ns("vm_api"),
            label = "Vannmiljø",
            icon = icon("database"),
            width = "300px"
          )
        ),
        div(
          style = "display: flex; flex-direction: column; align-items: center;",
          h6("Import .csv or .xlsx"),
          input_task_button(
            id = ns("spreadsheet"),
            label = HTML(paste(
              bsicons::bs_icon("table"),
              "Extract using LLM..."
            )),
            width = "300px"
          )
        ),
        div(
          style = "display: flex; flex-direction: column; align-items: center;",
          h6("Request other formats (planned feature)"),
          input_task_button(
            id = ns("request_format"),
            label = "Request format",
            icon = icon("table"),
            width = "300px"
          )
        )
      )
    )
  )
}

#' structured Server Functions
#'
#' @rdname mod_structured
#' @export
mod_structured_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_structured_ui("structured_1")

## To be copied in the server
# mod_structured_server("structured_1")
