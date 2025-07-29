#' sites UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sites_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sites Server Functions
#'
#' @noRd 
mod_sites_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sites_ui("sites_1")
    
## To be copied in the server
# mod_sites_server("sites_1")
