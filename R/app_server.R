#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib toggle_dark_mode
#' @importFrom tibble tibble
#' @noRd
app_server <- function(input, output, session) {
  module_order <- c(
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

  # reactiveValues: initialise reactiveValues in session$userData to store data ----
  if (!is.reactivevalues(session$userData$reactiveValues)) {
    session$userData$reactiveValues <- reactiveValues(ENTERED_BY = character(0),
                                                      sitesData = tibble(NULL),
                                                      parametersData = tibble(NULL),
                                                      compartmentsData = tibble(NULL),
                                                      referenceData = tibble(NULL),
                                                      campaignData = tibble(NULL),
                                                      samplesData = tibble(NULL),
                                                      methodsData = tibble(NULL),
                                                      exportData = tibble(NULL),
                                                      biotaData = tibble(NULL),
                                                      )
  }

  moduleCampaign <- mod_campaign_server("campaign")
  moduleReference <- mod_references_server("references")
  moduleSites <- mod_sites_server("sites")
  moduleParameters <- mod_parameters_server("parameters")
  moduleCompartments <- mod_compartments_server("compartments")
  moduleMethods <- mod_methods_server("methods")
  moduleSamples <- mod_samples_server(
    "samples"
  )
}
