#' references UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_references_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' references Server Functions
#'
#' @noRd 
mod_references_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_references_ui("references_1")
    
## To be copied in the server
# mod_references_server("references_1")
