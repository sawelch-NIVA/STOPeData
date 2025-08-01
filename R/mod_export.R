#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom golem print_dev
mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id){
  moduleServer(id, function(input, output, session){
    # observe(print_dev(session$userData$reactiveValues$sitesData)) |>
    #   bindEvent(session$userData$reactiveValues$sitesData)

    ns <- session$ns
    # output$sitesData <- renderPrint((paste(session$userData$reactiveValues$sitesData %|truthy|% "oops")))
  })
}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
