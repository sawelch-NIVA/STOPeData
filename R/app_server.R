#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib toggle_dark_mode
#' @importFrom tibble tibble
#' @importFrom shinyjs enable disable
#' @import googledrive
#' @noRd
options(shiny.maxRequestSize = 20 * 1024^2) # TODO: Move this to the run call.

app_server <- function(input, output, session) {
  # reactiveValues: initialise reactiveValues in session$userData to store data ----
  if (!is.reactivevalues(session$userData$reactiveValues)) {
    session$userData$reactiveValues <- reactiveValues(
      ENTERED_BY = character(0),

      # Standard validated data
      sitesData = tibble(NULL),
      parametersData = tibble(NULL),
      compartmentsData = tibble(NULL),
      referenceData = tibble(NULL),
      campaignData = tibble(NULL),
      methodsData = tibble(NULL),
      samplesData = tibble(NULL),
      biotaData = tibble(NULL),
      samplesDataWithBiota = tibble(NULL),
      dataData = tibble(NULL),
      creedData = list(
        purpose_statement = tibble(NULL),
        dataset_details = tibble(NULL),
        gateway_criteria = tibble(NULL),
        reliability_criteria = tibble(NULL),
        relevance_criteria = tibble(NULL),
        CREED_output = tibble(NULL)
      ),

      # LLM extracted data
      campaignDataLLM = NULL,
      referenceDataLLM = NULL,
      sitesDataLLM = NULL,
      parametersDataLLM = NULL,
      compartmentsDataLLM = NULL,
      methodsDataLLM = NULL,
      samplesDataLLM = NULL,
      biotaDataLLM = NULL,
      samplesDataLLM = NULL,

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

  # Bookmark name for the current session ---
  bookmark_name <- reactiveVal(NULL)

  # onBookmark: save server reactiveValues and input values as bookmark
  onBookmark(function(state) {
    # Save reactiveValues
    state$values$reactiveValuesData <- reactiveValuesToList(
      session$userData$reactiveValues
    )

    # Save input
    state$values$input <- reactiveValuesToList(
      input
    )

    # Save bookmark metadata from session userData (set by bookmark manager)
    state$values$bookmarkName <- session$userData$bookmarkName %||%
      paste("Session", format(Sys.time(), "%Y-%m-%d %H:%M"))

    state$values$bookmarkUsername <- session$userData$bookmarkUsername %||%
      session$userData$reactiveValues$ENTERED_BY %||%
      "Unknown"

    state$values$bookmarkTimestamp <- session$userData$bookmarkTimestamp %||%
      Sys.time()

    state$values$bookmarkCampaign <- session$userData$reactiveValues$campaignData$CAMPAIGN_NAME_SHORT %||%
      "Unknown Campaign"

    state$values$bookmarkReference <- session$userData$reactiveValues$referenceData$REFERENCE_ID %||%
      Sys.time()
  })

  onRestore(function(state) {
    if (!is.null(state$values$reactiveValuesData)) {
      for (name in names(state$values$reactiveValuesData)) {
        session$userData$reactiveValues[[
          name
        ]] <- state$values$reactiveValuesData[[name]]
      }
    }
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
    "info",
    "save"
  )

  ## Gdrive setup
  # Authenticate
  # drive_auth_configure(api_key = Sys.getenv("GOOGLE_DRIVE_API_KEY"))
  # drive_auth(email = "sawelch1994@gmail.com")

  # # Locate bookmark folder
  # bookmarks_folder_name <- "Saved Sessions"
  # bookmarks_folder <- drive_get(bookmarks_folder_name)

  observe({
    if (nrow(bookmarks_folder) == 0) {
      showNotification(
        ui = "Connection to Google Drive Failed. Sessions cannot be saved.",
        type = "warning"
      )
    } else {
      showNotification(ui = "Connected to Google Drive", type = "message")
    }
  }) |>
    bindEvent(bookmarks_folder)

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

  ## observe: save session from navigation ba
  # downstream: session$userData$reactiveValues$bookmarkedSessions
  observe({
    # Get bookmark name or create default
    name <- paste("Session", format(Sys.time(), "%Y-%m-%d %H:%M"))

    # Get username from session
    username <- session$userData$reactiveValues$ENTERED_BY %||% "Unknown User"

    # Store bookmark metadata for onBookmark callback
    session$userData$bookmarkName <- name
    session$userData$bookmarkUsername <- username
    session$userData$bookmarkTimestamp <- Sys.time()

    input <- input
    userData <- session$userData$reactiveValues

    saveRDS(userData, file = "test.rds")

    # Show success notification
    showNotification(
      glue("Session '{name}' saved successfully."),
      type = "message"
    )
  }) |>
    bindEvent(input$save_bookmark, ignoreInit = TRUE)
}
