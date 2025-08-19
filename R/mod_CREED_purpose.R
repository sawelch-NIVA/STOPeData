#' CREED_purpose UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_CREED_purpose_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' CREED_purpose Server Functions
#'
#' @noRd 
mod_CREED_purpose_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_CREED_purpose_ui("CREED_purpose_1")
    
## To be copied in the server
# mod_CREED_purpose_server("CREED_purpose_1")
