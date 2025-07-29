#' samples UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_samples_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' samples Server Functions
#'
#' @noRd 
mod_samples_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_samples_ui("samples_1")
    
## To be copied in the server
# mod_samples_server("samples_1")
