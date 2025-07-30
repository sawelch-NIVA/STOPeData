#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  campaign_data <- mod_campaign_server("campaign")
  reference_data <- mod_references_server("references")
}
