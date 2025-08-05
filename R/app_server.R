#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib toggle_dark_mode
#' @importFrom tibble tibble
#' @importFrom shinyjs enable disable
#' @noRd
app_server <- function(input, output, session) {
  # reactiveValues: initialise reactiveValues in session$userData to store data ----
  if (!is.reactivevalues(session$userData$reactiveValues)) {
    session$userData$reactiveValues <- reactiveValues(
      ENTERED_BY = character(0),
      sitesData = tibble(NULL),
      parametersData = tibble(NULL),
      compartmentsData = tibble(NULL),
      referenceData = tibble(NULL),
      campaignData = tibble(NULL),
      methodsData = tibble(NULL),
      samplesData = tibble(NULL),
      biotaData = tibble(NULL),
      dataData = tibble(NULL)
    )
  }

  # Reactive for DB status (DISABLED) ---
  db_status <- reactive({
    # Add invalidateLater if you want periodic checks
    # invalidateLater(5000)

    FALSE
  })

  moduleCampaign <- mod_campaign_server("campaign")
  moduleReference <- mod_references_server("references")
  moduleSites <- mod_sites_server("sites")
  moduleParameters <- mod_parameters_server("parameters")
  moduleCompartments <- mod_compartments_server("compartments")
  moduleMethods <- mod_methods_server("methods")
  moduleSamples <- mod_samples_server("samples")
  moduleBiota <- mod_biota_server("biota")
  moduleData <- mod_data_server(
    "data"
  )
  moduleReview <- mod_review_server(
    "review"
  )

  # Module navigation ----
  ## Navigation setup ----
  module_order <- c(
    "00-landing",
    "01-campaign",
    "02-references",
    "03-sites",
    "04-parameters",
    "05-compartments",
    "06-methods",
    "07-samples",
    "08-biota",
    "09-data",
    "10-review"
  )

  ## Track current module position ----
  current_position <- reactive({
    current_tab <- input$`main-page`
    if (is.null(current_tab)) return(2) # Default to campaign (position 2)
    match(current_tab, module_order)
  })

  ## observe: Update button states based on current position ----
  observe({
    pos <- current_position()

    # Disable previous button on first active module (campaign)
    if (pos <= 2) {
      disable("previous_section")
    } else {
      enable("previous_section")
    }

    # Disable next button on last module (review)
    if (pos >= length(module_order)) {
      disable("next_section")
    } else {
      enable("next_section")
    }
  }) |>
    bindEvent(input$`main-page`, ignoreInit = FALSE)

  ## observe: Previous section navigation ----
  observe({
    pos <- current_position()
    if (pos > 2) {
      # Don't go before campaign
      new_tab <- module_order[pos - 1]
      updateNavbarPage(session, "main-page", selected = new_tab)
    }
  }) |>
    bindEvent(input$previous_section)

  ## observe: Next section navigation ----
  observe({
    pos <- current_position()
    if (pos < length(module_order)) {
      # Don't go past review
      new_tab <- module_order[pos + 1]
      updateNavbarPage(session, "main-page", selected = new_tab)
    }
  }) |>
    bindEvent(input$next_section)

  ## output: db connection status (disabled) ----
  output$db_connection <- renderUI({
    status <- db_status()

    if (status) {
      # Connected - green circle
      div(
        style = "height: 20px; width: 20px; border-radius: 50%; background-color: #28a745;",
        title = "Database Connected"
      )
    } else {
      # Disconnected - red circle
      div(
        style = "height: 20px; width: 20px; border-radius: 50%; background-color: #dc3545;",
        title = "Database Disconnected"
      )
    }
  })
}
