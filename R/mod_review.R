#' review UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Export"),
    verbatimTextOutput(ns("dataData")),
  )
}

#' review Server Functions
#'
#' @noRd
mod_review_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print_dev("mod_review_server initialised")
    output$dataData <- renderPrint(
      (paste(
        session$userData$reactiveValues$dataData %|truthy|% "dataData not found"
      ))
    )
  })
}

## To be copied in the UI
# mod_review_ui("review_1")

## To be copied in the server
# mod_review_server("review_1")
