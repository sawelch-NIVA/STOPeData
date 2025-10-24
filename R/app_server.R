#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib toggle_dark_mode
#' @importFrom tibble tibble
#' @importFrom shinyjs enable disable
#' @importFrom htmltools HTML
#' @noRd
options(shiny.maxRequestSize = 20 * 1024^2) # TODO: Move this to the run call.
`%notin%` <- negate(`%in%`)

app_server <- function(input, output, session) {
  # reactiveValues: initialise reactiveValues in session$userData to store data ----
  if (!is.reactivevalues(session$userData$reactiveValues)) {
    session$userData$reactiveValues <- reactiveValues(
      ENTERED_BY = character(0),

      # Standard validated data
      # All userData and module_state$data data is stored in a tabular (tibble) format centrally, even for campaign and reference (which currently only have one row)
      # This means we can use a consistent set of functions to check for presence (nrow(tibble) > 0), and not have any nasty surprises when we expect one and get the other
      sitesData = initialise_sites_tibble(),
      parametersData = initialise_parameters_tibble(),
      compartmentsData = initialise_compartments_tibble(),
      referenceData = initialise_references_tibble(),
      campaignData = initialise_campaign_tibble(),
      methodsData = initialise_methods_tibble(),
      samplesData = initialise_samples_tibble(),
      biotaData = initialise_biota_tibble(),
      samplesDataWithBiota = tibble(NULL),
      measurementsData = initialise_measurements_tibble(),
      creedData = list(
        purpose_statement = tibble(NULL),
        dataset_details = tibble(NULL),
        gateway_criteria = tibble(NULL),
        reliability_criteria = tibble(NULL),
        relevance_criteria = tibble(NULL),
        CREED_output = tibble(NULL)
      ),

      # LLM extracted data
      campaignDataLLM = tibble(NULL),
      referenceDataLLM = tibble(NULL),
      sitesDataLLM = tibble(NULL),
      parametersDataLLM = tibble(NULL),
      compartmentsDataLLM = tibble(NULL),
      methodsDataLLM = tibble(NULL),
      samplesDataLLM = tibble(NULL),
      biotaDataLLM = tibble(NULL),
      samplesDataLLM = tibble(NULL),

      # LLM extraction status flags
      llmExtractionComplete = FALSE,
      llmExtractionSuccessful = FALSE,

      bookmarkedSessions = NULL
    )
  }

  # Reactive for DB status (DISABLED) ---
  db_status <- reactive({
    # Add invalidateLater if you want periodic checks
    # invalidateLater(5000)
    FALSE
  })

  # Module servers ----
  moduleLanding <- mod_landing_server("landing", parent_session = session) # parent_session = session needed or else updateNavbarPage() doesn't work...
  moduleLLM <- mod_llm_server("llm_extract")
  moduleCampaign <- mod_campaign_server("campaign")
  moduleReference <- mod_references_server("references")
  moduleSites <- mod_sites_server("sites")
  moduleParameters <- mod_parameters_server("parameters")
  moduleCompartments <- mod_compartments_server("compartments")
  moduleMethods <- mod_methods_server("methods")
  moduleSamples <- mod_samples_server("samples")
  moduleBiota <- mod_biota_server("biota")
  moduleData <- mod_data_server(
    "data",
    parent_session = session
  )
  moduleReview <- mod_review_server(
    "review"
  )
  moduleExport <- mod_export_server(
    "export"
  )
  moduleCREED <- mod_CREED_server("CREED")
  # moduleBookmarkManager <- mod_bookmark_manager_server("session_manager")

  # Module navigation ----
  ## Navigation setup ----
  module_order <- c(
    "00-landing",
    "00-llm-extract",
    "01-campaign",
    "02-references",
    "03-sites",
    "04-parameters",
    "05-compartments",
    "06-methods",
    "07-samples",
    "08-biota",
    "09-data",
    "10-review",
    "11-export",
    "12-CREED",
    "info"
  )

  ## Track current module position ----
  current_position <- reactive({
    current_tab <- input$`main-page`

    stopifnot(current_tab %in% module_order)

    if (is.null(current_tab)) {
      return(1)
    } # Default to LLM extract (position 1)
    match(current_tab, module_order)
  })

  ## observe: Update button states based on current position ----
  observe({
    pos <- current_position()

    # Disable previous button on first module (LLM extract)
    if (pos <= 1) {
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
    if (pos > 1) {
      # Don't go before LLM extract
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

  # can we enable reconnect on crashes?
  session$allowReconnect(TRUE)

  # Import Observer Code ----
  # Add these observers directly to app_server.R

  # Observer: Show import modal when upload_zip button clicked ----
  # upstream: input$`landing-upload_zip` (button click from landing module)
  # downstream: modal dialog with file input
  observe({
    showModal(
      modalDialog(
        title = div(
          icon("upload"),
          "Upload Session Data"
        ),
        size = "m",
        easyClose = TRUE,
        footer = div(
          actionButton(
            inputId = "import_cancel",
            label = "Cancel",
            class = "btn-secondary"
          ),
          actionButton(
            inputId = "import_confirm",
            label = "Import Data",
            class = "btn-primary",
            icon = icon("download")
          )
        ),

        # Modal content ----
        card(
          card_body(
            h4("Import Previously Exported Session Data"),
            p(
              "Select a ZIP file that was previously exported from this application. The ZIP should contain CSV files and metadata text files."
            ),

            fileInput(
              inputId = "import_zip_file",
              label = "Choose ZIP File",
              accept = c(".zip"),
              multiple = FALSE
            ),

            div(
              id = "import_status",
              style = "margin-top: 15px; display: none;",
              # Status messages will be shown here
            )
          )
        )
      )
    )
  }) |>
    bindEvent(input$`landing-upload_zip`, ignoreInit = TRUE)

  # Observer: Handle ZIP file import when confirm button clicked ----
  # upstream: input$import_confirm (button click from modal)
  # downstream: session$userData$reactiveValues updated with imported data
  observe({
    req(input$import_zip_file)

    # Show loading status
    show("import_status")
    HTML(
      "import_status",
      '<div class="alert alert-info"><i class="fa fa-spinner fa-spin"></i> Importing data...</div>'
    )

    # Get file path
    zip_path <- input$import_zip_file$datapath

    # Import data using helper function
    result <- import_session_from_zip(zip_path, session)

    # Update status and handle result
    if (result$success) {
      # Success - show success message and close modal
      HTML(
        "import_status",
        glue(
          '<div class="alert alert-success"><i class="fa fa-check"></i> {result$message}</div>'
        )
      )

      # Small delay to show success message, then close modal and navigate
      Sys.sleep(0.1)
      removeModal()

      # Show notification
      showNotification(
        ui = HTML(result$message),
        type = "message",
        duration = 5
      )
    } else {
      # Error - show error message but keep modal open
      HTML(
        "import_status",
        glue(
          '<div class="alert alert-danger"><i class="fa fa-exclamation-triangle"></i> {result$message}</div>'
        )
      )

      # Also show notification
      showNotification(
        ui = result$message,
        type = "error",
        duration = 10
      )
    }
  }) |>
    bindEvent(input$import_confirm, ignoreInit = TRUE)

  # Observer: Handle modal cancel button ----
  # upstream: input$import_cancel (button click from modal)
  # downstream: modal closed
  observe({
    removeModal()
  }) |>
    bindEvent(input$import_cancel, ignoreInit = TRUE)
}
