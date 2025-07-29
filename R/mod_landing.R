#' landing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_landing_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' landing Server Functions
#'
#' @noRd 
mod_landing_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_landing_ui("landing_1")
    
## To be copied in the server
# mod_landing_server("landing_1")
