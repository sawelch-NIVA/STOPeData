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
    h1("Campaigns"),
    verbatimTextOutput(ns("campaignsData")),
    h1("References"),
    verbatimTextOutput(ns("referencesData")),
    h1("Sites"),
    verbatimTextOutput(ns("sitesData")),
    h1("Parameters"),
    verbatimTextOutput(ns("parametersData")),
    h1("Compartments"),
    verbatimTextOutput(ns("compartmentsData")),
    h1("Methods"),
    verbatimTextOutput(ns("methodsData")),
    h1("Samples"),
    verbatimTextOutput(ns("samplesData")),
    h1("Biota"),
    verbatimTextOutput(ns("biotaData"))
  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$campaignsData <- renderPrint((paste(session$userData$reactiveValues$campaignsData %|truthy|% "campaignsData not found")))
    output$referencesData <- renderPrint((paste(session$userData$reactiveValues$referencesData %|truthy|% "referencesData not found")))
    output$sitesData <- renderPrint((paste(session$userData$reactiveValues$sitesData %|truthy|% "sitesData not found")))
    output$parametersData <- renderPrint((paste(session$userData$reactiveValues$parametersData %|truthy|% "parameters not found")))
    output$compartmentsData <- renderPrint((paste(session$userData$reactiveValues$compartmentsData %|truthy|% "compartmentsData not found")))
    output$methodsData <- renderPrint((paste(session$userData$reactiveValues$methodsData %|truthy|% "methodsData not found")))
    output$samplesData <- renderPrint((paste(session$userData$reactiveValues$samplesData %|truthy|% "samplesData not found")))
    output$biotaData <- renderPrint((paste(session$userData$reactiveValues$biotaData %|truthy|% "biotaData not found")))

  })
}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
