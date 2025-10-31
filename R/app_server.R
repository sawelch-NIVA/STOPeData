#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib toggle_dark_mode
#' @importFrom tibble tibble
#' @importFrom shinyjs enable disable hide show
#' @importFrom htmltools HTML
#' @importFrom purrr map_chr
#' @noRd
options(shiny.maxRequestSize = 20 * 1024^2) # TODO: Move this to the run call.
`%notin%` <- negate(`%in%`) # my belovéd.

app_server <- function(input, output, session) {
  # 1. Setup ----

  ## ReactiveValues: initialise reactiveValues in session$userData to store data ----
  # upstream: session start
  # downstream: session$userData$reactiveValues with initialized data structures
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

      # LLM extracted data and metadata
      schemaLLM = "",
      promptLLM = "",
      rawLLM = "",
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

      # Import data from save status flags
      saveExtractionComplete = FALSE,
      saveExtractionSuccessful = FALSE,

      bookmarkedSessions = NULL
    )
  }

  ## ReactiveValues: moduleState ----
  # upstream: session start
  # downstream: module state tracking for export functionality
  # todo: again, copied from mod_export. should be removed in time.
  moduleState <- reactiveValues(
    export_ready = FALSE,
    available_datasets = character(0),
    campaign_name = "Unknown_Campaign",
    dataset_dimensions = list()
  )

  ## reactiveTimer: Autosave periodicity (10 seconds)
  autoInvalidate <<- reactiveTimer(10000)

  ## Module servers ----
  # upstream: session start
  # downstream: module server instances
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

  ## Navigation setup ----
  # upstream: session start
  # downstream: module_order vector for navigation logic
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

  ## Session: enable reconnect on crashes ----
  # upstream: session start
  # downstream: session reconnection capability
  session$allowReconnect(TRUE)

  # 2. Helper Functions ----

  ## Reactive: DB status (DISABLED) ----
  # upstream: session start (with optional invalidateLater)
  # downstream: db_status value for UI display
  db_status <- reactive({
    # Add invalidateLater if you want periodic checks
    # invalidateLater(5000)
    FALSE
  })

  ## Reactive: Track current module position ----
  # upstream: input$`main-page` (navbar tab selection)
  # downstream: current position index for navigation logic
  current_position <- reactive({
    current_tab <- input$`main-page`

    stopifnot(current_tab %in% module_order)

    if (is.null(current_tab)) {
      return(1)
    } # Default to LLM extract (position 1)
    match(current_tab, module_order)
  })

  # 3. Observers ----

  ## Observer: Update nav button states based on current position ----
  # upstream: input$`main-page` (navbar tab selection), current_position()
  # downstream: enable/disable state of previous_section and next_section buttons
  observe({
    pos <- current_position()

    # Disable previous button on first module (LLM extract)
    if (pos <= 1) {
      hide("previous_section")
    } else {
      show("previous_section")
    }

    # Disable next button on last module (review)
    if (pos >= length(module_order)) {
      hide("next_section")
    } else {
      show("next_section")
    }
  }) |>
    bindEvent(input$`main-page`, ignoreInit = FALSE)

  ## Observer: Previous section navigation ----
  # upstream: input$previous_section (button click)
  # downstream: updateNavbarPage() call to navigate to previous module
  observe({
    pos <- current_position()
    if (pos > 1) {
      # Don't go before LLM extract
      new_tab <- module_order[pos - 1]
      updateNavbarPage(session, "main-page", selected = new_tab)
    }
  }) |>
    bindEvent(input$previous_section)

  ## Observer: Next section navigation ----
  # upstream: input$next_section (button click)
  # downstream: updateNavbarPage() call to navigate to next module
  observe({
    pos <- current_position()
    if (pos < length(module_order)) {
      # Don't go past review
      new_tab <- module_order[pos + 1]
      updateNavbarPage(session, "main-page", selected = new_tab)
    }
  }) |>
    bindEvent(input$next_section)

  ## Observer: Show import modal when upload_zip button clicked ----
  # upstream: input$`landing-upload_zip` (button click from landing module)
  # downstream: modal dialog with file input
  observe({
    showModal(
      modalDialog(
        title = div(
          icon("upload"),
          "Upload Session Data"
        ),
        size = "l",
        easyClose = TRUE,
        footer = div(
          actionButton(
            inputId = "import_cancel",
            label = "Close",
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
              "Select a ZIP file that was previously exported from this application. The ZIP should contain CSV files and metadata text files. 
              The importer recognises data types by file name, so please don't modify the 'Campaign__Date_Module' formatting."
            ),

            fileInput(
              inputId = "import_zip_file",
              label = "Choose ZIP File",
              accept = c(".zip"),
              multiple = FALSE,
              width = "100%"
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

  ## Observer: Check data availability ----
  # upstream: input$download_all_modal (action button)
  # downstream: moduleState$export_ready, moduleState$available_datasets, moduleState$dataset_dimensions
  observe({
    rv <- session$userData$reactiveValues

    # Define all possible datasets
    dataset_names <- c(
      "sitesData",
      "parametersData",
      "compartmentsData",
      "referenceData",
      "campaignData",
      "methodsData",
      "samplesData",
      "biotaData",
      "measurementsData",
      "schemaLLM",
      "promptLLM",
      "rawLLM"
    )

    # Define which datasets are text/object files vs tabular
    text_datasets <- c("schemaLLM", "promptLLM", "rawLLM")

    # Check which datasets have data and store dimensions
    available <- character(0)
    dimensions <- list()

    for (dataset in dataset_names) {
      data <- rv[[dataset]]

      if (dataset %in% text_datasets) {
        # Handle text/object data - check if it exists and has content
        has_content <- FALSE
        char_count <- 0

        if (!is.null(data)) {
          if (is.character(data) && length(data) > 0 && nchar(data[1]) > 0) {
            has_content <- TRUE
            char_count <- nchar(data[1])
          } else if (is.list(data) && length(data) > 0) {
            # For lists (like rawLLM), check if it has any content
            has_content <- TRUE
            char_count <- nchar(paste(
              capture.output(str(data)),
              collapse = "\n"
            ))
          } else if (inherits(data, "ellmer_schema") || is.object(data)) {
            # For schema objects or other objects
            has_content <- TRUE
            char_count <- nchar(paste(
              capture.output(print(data)),
              collapse = "\n"
            ))
          }
        }

        if (has_content) {
          available <- c(available, dataset)
          dimensions[[dataset]] <- list(
            type = "text",
            chars = char_count
          )
        }
      } else {
        # Handle tabular data
        if (!is.null(data) && nrow(data) > 0) {
          available <- c(available, dataset)
          dimensions[[dataset]] <- list(
            type = "tabular",
            rows = nrow(data),
            cols = ncol(data)
          )
        }
      }
    }

    moduleState$available_datasets <- available
    moduleState$dataset_dimensions <- dimensions
    moduleState$export_ready <- length(available) > 0

    # Get campaign name for filename
    if (!is.null(rv$campaignData) && nrow(rv$campaignData) > 0) {
      if ("CAMPAIGN_NAME" %in% names(rv$campaignData)) {
        campaign_name <- rv$campaignData$CAMPAIGN_NAME[1]
        if (!is.na(campaign_name) && campaign_name != "") {
          moduleState$campaign_name <- campaign_name
        }
      }
    }

    print_dev(glue(
      "mod_export: {length(available)} datasets available: {paste(available, collapse = ', ')}"
    ))
  }) |>
    bindEvent(input$download_all_modal)

  ## Observer: Handle ZIP file import when confirm button clicked ----
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
      session$userData$reactiveValues$saveExtractionComplete <- TRUE
      session$userData$reactiveValues$saveExtractionSuccessful <- TRUE
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
      session$userData$reactiveValues$saveExtractionComplete <- FALSE
      session$userData$reactiveValues$saveExtractionSuccessful <- FALSE
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

  ## Observer: Handle modal cancel button ----
  # upstream: input$import_cancel (button click from modal)
  # downstream: modal closed
  observe({
    removeModal()
  }) |>
    bindEvent(input$import_cancel, ignoreInit = TRUE)

  # todo: this is a hacky copy-paste of an observer from mod_export. a more integrated solution should be (eventually) added.
  ## Observer: Show download modal when download_all_modal button clicked ----
  # upstream: input$download_all_modal (button click)
  # downstream: modal dialog with dataset summary and download button
  observe({
    # Use the same moduleState data that was already populated
    available_datasets <- moduleState$available_datasets
    dataset_dimensions <- moduleState$dataset_dimensions
    export_ready <- moduleState$export_ready

    # Create summary content using the same format as workbook_summary
    if (export_ready && length(available_datasets) > 0) {
      summary_content <- map_chr(
        available_datasets,
        ~ {
          dims <- dataset_dimensions[[.x]]
          if (dims$type == "text") {
            glue("• {.x}: {dims$chars} characters")
          } else {
            glue("• {.x}: {dims$rows} rows × {dims$cols} columns")
          }
        }
      ) %>%
        paste(collapse = "\n")

      footer_content <- div(
        actionButton(
          inputId = "download_modal_cancel",
          label = "Cancel",
          class = "btn-secondary"
        ),
        downloadButton(
          outputId = "download_all_csv",
          label = "Download Data Files", # Changed from "Download CSV Files"
          class = "btn-primary",
          icon = icon("download")
        )
      )
    } else {
      summary_content <- "No data available for download."
      footer_content <- div(
        actionButton(
          inputId = "download_modal_cancel",
          label = "Close",
          class = "btn-secondary"
        )
      )
    }

    showModal(
      modalDialog(
        title = div(
          icon("download"),
          "Download Session Data"
        ),
        size = "l",
        easyClose = TRUE,
        footer = footer_content,

        # Modal content ----
        card(
          card_body(
            h4("Available Datasets"),
            p("The following datasets are available for download:"),
            pre(
              summary_content,
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px;"
            )
          )
        )
      )
    )
  }) |>
    bindEvent(input$download_all_modal, ignoreInit = TRUE)

  ## Observer: Handle download modal cancel button ----
  # upstream: input$download_modal_cancel (button click from modal)
  # downstream: modal closed
  observe({
    removeModal()
  }) |>
    bindEvent(input$download_modal_cancel, ignoreInit = TRUE)

  # 4. Outputs ----

  ## Output: db connection status (disabled) ----
  # upstream: db_status() reactive
  # downstream: UI element showing database connection status
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

  ## Output: download handler for all CSV files ----
  # upstream: moduleState reactive values
  # downstream: ZIP file download containing all data as CSV
  output$download_all_csv <- download_all_csv(
    session = session,
    moduleState = moduleState
  )
}
