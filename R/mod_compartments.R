#' compartments UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_compartments_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' compartments Server Functions
#'
#' @noRd 
mod_compartments_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_compartments_ui("compartments_1")
    
## To be copied in the server
# mod_compartments_server("compartments_1")
