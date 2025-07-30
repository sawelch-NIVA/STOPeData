#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib toggle_dark_mode
#' @noRd
app_server <- function(input, output, session) {
  moduleOrder <- c(
    "00-landing",
    "01-campaign",
    "02-references",
    "03-sites",
    "04-parameters",
    "05-compartments",
    "06-methods",
    "07-samples",
    "08-export",
    "09-review"
  )

  # Causes epilepsy, disabled.
  # observe({
  #   toggle_dark_mode()
  # }) |>
  #   bindEvent(input$darkmode, ignoreNULL = TRUE, ignoreInit = TRUE)

  campaign_data <- mod_campaign_server("campaign")
  reference_data <- mod_references_server("references")
  sites_data <- mod_sites_server("sites")
  parameters_data <- mod_parameters_server("parameters")
  compartments_data <- mod_compartments_server("compartments")
  methods_data <- mod_methods_server("methods")
}
