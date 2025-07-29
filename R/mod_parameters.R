#' parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parameters_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' parameters Server Functions
#'
#' @noRd 
mod_parameters_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_parameters_ui("parameters_1")
    
## To be copied in the server
# mod_parameters_server("parameters_1")
