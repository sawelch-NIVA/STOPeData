# References Import Module ----
# A Shiny module for reference data entry with conditional validation based on reference type

#' References UI Function ----
#'
#' @description A shiny Module for reference data entry and validation with conditional fields.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput dateInput selectInput textAreaInput actionButton numericInput
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs enable disable
mod_references_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main input card ----
    card(
      card_header("Reference Data Entry"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Reference Data Information",
            icon = bs_icon("info-circle"),
            "This form collects bibliographic reference data. Fields marked with (*) are always required. Additional fields become required based on the selected reference type. Use the DOI lookup or BibTeX import features to auto-populate fields when available."
          )
        ),

        ## Import tools section ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### DOI lookup placeholder ----
          div(
            textInput(
              inputId = ns("doi_lookup"),
              label = "DOI Lookup (Disabled)",
              placeholder = "Enter DOI to auto-populate fields",
              width = "100%"
            ),
            actionButton(
              inputId = ns("lookup_doi"),
              label = "Lookup DOI (Disabled)",
              class = "btn-info",
              width = "100%"
            )
          ),

          ### BibTeX import placeholder ----
          div(
            textAreaInput(
              inputId = ns("bibtex_import"),
              label = "BibTeX Import (Disabled)",
              placeholder = "Paste BibTeX entry here",
              rows = 1,
              width = "100%"
            ),
            actionButton(
              inputId = ns("import_bibtex"),
              label = "Import BibTeX (Disabled)",
              class = "btn-info",
              width = "100%"
            )
          )
        ),

        ## Reference type selector (always required) ----
        selectInput(
          inputId = ns("REFERENCE_TYPE"),
          label = tooltip(
            list("Reference Type *", bs_icon("info-circle-fill")),
            "Select the type of reference to add"
          ),
          choices = c(
            "Journal Article" = "journal",
            "Book" = "book",
            "Report" = "report",
            "Dataset/Database" = "dataset"
          ),
          selected = "journal",
          width = "300px"
        ),

        ## Always required fields ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### AUTHOR - Always required, 1000 char ----
          textAreaInput(
            inputId = ns("AUTHOR"),
            label = tooltip(
              list("Author(s) *", bs_icon("info-circle-fill")),
              "Authors in format: Last1, First1; Last2, First2"
            ),
            placeholder = "Last1, First1; Last2, First2",
            rows = 1,
            width = "100%"
          ),

          ### TITLE - Always required, 1000 char ----
          textAreaInput(
            inputId = ns("TITLE"),
            label = tooltip(
              list("Title *", bs_icon("info-circle-fill")),
              "Full title of the publication"
            ),
            placeholder = "Full publication title",
            rows = 1,
            width = "100%"
          ),

          ### YEAR - Always required, int ----
          numericInput(
            inputId = ns("YEAR"),
            label = tooltip(
              list("Year *", bs_icon("info-circle-fill")),
              "Year of publication, last update, etc. as appropriate"
            ),
            value = as.numeric(format(Sys.Date(), "%Y")),
            min = 1800,
            max = as.numeric(format(Sys.Date(), "%Y")) + 5,
            step = 1,
            width = "100%"
          )
        ),

        ## Conditional fields layout ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### ACCESS_DATE - Required for all  ----
          dateInput(
            inputId = ns("ACCESS_DATE"),
            label = "Date Accessed *",
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(),

          ### ENTERED_BY - From mod_campaign if available ----
          textInput(
            inputId = ns("ENTERED_BY"),
            label = tooltip(
              list("Entered by", bs_icon("info-circle-fill")),
              "Your name or initials. Autofilled from campaign module if availible."
            ),
            placeholder = "Your name or initials...",
            width = "100%"
          ),

          ### Journal-specific fields ----
          textInput(
            inputId = ns("PERIODICAL_JOURNAL"),
            label = tooltip(
              list("Journal Name", bs_icon("info-circle-fill")),
              "Name of journal or publication series"
            ),
            placeholder = "Journal name",
            width = "100%"
          ),

          numericInput(
            inputId = ns("VOLUME"),
            label = "Volume",
            value = NA,
            min = 1,
            step = 1,
            width = "100%"
          ),

          numericInput(
            inputId = ns("ISSUE"),
            label = "Issue",
            value = NA,
            min = 1,
            step = 1,
            width = "100%"
          ),

          ### Book-specific fields ----
          textInput(
            inputId = ns("PUBLISHER"),
            label = "Publisher",
            placeholder = "Publishing organization",
            width = "100%"
          ),

          ### Report-specific fields ----
          textInput(
            inputId = ns("INSTITUTION"),
            label = "Institution",
            placeholder = "Institution responsible for the data",
            width = "100%"
          ),

          ### Dataset-specific fields ----
          textInput(
            inputId = ns("DB_NAME"),
            label = "Database Name",
            placeholder = "Name of the database/dataset",
            width = "100%"
          ),

          textInput(
            inputId = ns("DB_PROVIDER"),
            label = "Database Provider",
            placeholder = "Provider of the database/dataset",
            width = "100%"
          ),

          ### Optional fields for all types ----
          textInput(
            inputId = ns("DOI"),
            label = "DOI",
            placeholder = "Digital Object Identifier",
            width = "100%"
          ),

          textInput(
            inputId = ns("URL"),
            label = "URL",
            placeholder = "Web address",
            width = "100%"
          ),

          textInput(
            inputId = ns("PAGES"),
            label = "Pages",
            placeholder = "e.g., 123-145",
            width = "100%"
          ),

          textInput(
            inputId = ns("ISBN_ISSN"),
            label = "ISBN/ISSN",
            placeholder = "ISBN for books or ISSN for journals",
            width = "100%"
          ),

          textInput(
            inputId = ns("EDITION"),
            label = "Edition",
            placeholder = "Edition number or description",
            width = "100%"
          ),

          textInput(
            inputId = ns("PUBLISHED_PLACE"),
            label = "Published Place",
            placeholder = "City or location of publication",
            width = "100%"
          ),

          textInput(
            inputId = ns("DOCUMENT_NUMBER"),
            label = "Document Number",
            placeholder = "Document identification number",
            width = "100%"
          ),

          textInput(
            inputId = ns("ACCESSION_NUMBER"),
            label = "Accession Number",
            placeholder = "Database accession number",
            width = "100%"
          ),

          textInput(
            inputId = ns("PMCID"),
            label = "PubMed CID",
            placeholder = "PubMed Central ID",
            width = "100%"
          ),

          textInput(
            inputId = ns("SERIES_TITLE"),
            label = "Series Title",
            placeholder = "Title of the series if part of one",
            width = "100%"
          ),

          textInput(
            inputId = ns("SERIES_EDITOR"),
            label = "Series Editor",
            placeholder = "Editor of the series if applicable",
            width = "100%"
          ),

          numericInput(
            inputId = ns("SERIES_VOLUME"),
            label = "Series Volume",
            value = NA,
            min = 1,
            step = 1,
            width = "100%"
          ),

          textInput(
            inputId = ns("NUMBER_OF_PAGES"),
            label = "Number of Pages",
            placeholder = "Total pages",
            width = "100%"
          ),

          textInput(
            inputId = ns("NUMBER_OF_VOLUMES"),
            label = "Number of Volumes",
            placeholder = "Number of volumes",
            width = "100%"
          )
        ),

        ### REF_COMMENT - Full width text area ----
        textAreaInput(
          inputId = ns("REF_COMMENT"),
          label = "Reference Comment",
          placeholder = "Additional notes about the reference (optional)",
          width = "100%",
          rows = 3
        ),

        ## Validation status and raw data ----
        uiOutput(ns("validation_reporter")),
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Click to view raw validated data",
            icon = bs_icon("code"),
            verbatimTextOutput(ns("validated_data_display"))
          )
        ),

        ## Action buttons ----
        actionButton(
          inputId = ns("clear"),
          label = "Clear All Fields",
          class = "btn-danger",
          width = "300px"
        )
      )
    )
  )
}

#' References Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText updateTextInput updateDateInput updateNumericInput updateTextAreaInput updateSelectInput bindEvent
#' @importFrom shinyjs enable disable
mod_references_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## reactiveValues: moduleState ----
    moduleState <- reactiveValues(
      validated_data = NULL,
      is_valid = FALSE
    )

    ## InputValidator$new: iv ----
    iv <- InputValidator$new()

    ### Always required fields ----
    iv$add_rule("REFERENCE_TYPE", sv_required())

    iv$add_rule("AUTHOR", sv_required())
    iv$add_rule("AUTHOR", function(value) {
      if (isTruthy(value) && nchar(value) > 1000) {
        "Author(s) must be 1000 characters or less"
      }
    })

    iv$add_rule("TITLE", sv_required())
    iv$add_rule("TITLE", function(value) {
      if (isTruthy(value) && nchar(value) > 1000) {
        "Title must be 1000 characters or less"
      }
    })

    iv$add_rule("YEAR", sv_required())
    iv$add_rule("YEAR", function(value) {
      if (
        isTruthy(value) &&
          (value < 1800 || value > as.numeric(format(Sys.Date(), "%Y")) + 5)
      ) {
        "Year must be between 1800 and 5 years in the future"
      }
    })

    ### Conditionally required fields ----
    # ACCESS_DATE - required for all reference types
    iv$add_rule("ACCESS_DATE", function(value) {
      if (!isTruthy(value)) {
        "Date Accessed is required for all reference types"
      }
    })

    # Journal-specific required fields
    iv$add_rule("PERIODICAL_JOURNAL", function(value) {
      if (input$REFERENCE_TYPE == "journal" && !isTruthy(value)) {
        "Journal Name is required for journal articles"
      }
    })

    iv$add_rule("VOLUME", function(value) {
      if (input$REFERENCE_TYPE == "journal" && !isTruthy(value)) {
        "Volume is required for journal articles"
      }
    })

    iv$add_rule("ISSUE", function(value) {
      if (input$REFERENCE_TYPE == "journal" && !isTruthy(value)) {
        "Issue is required for journal articles"
      }
    })

    # Book-specific required fields
    iv$add_rule("PUBLISHER", function(value) {
      if (input$REFERENCE_TYPE %in% c("book", "report") && !isTruthy(value)) {
        "Publisher is required for books and reports"
      }
    })

    # Report-specific required fields
    iv$add_rule("INSTITUTION", function(value) {
      if (input$REFERENCE_TYPE == "report" && !isTruthy(value)) {
        "Institution is required for reports"
      }
    })

    # Dataset-specific required fields
    iv$add_rule("DB_NAME", function(value) {
      if (input$REFERENCE_TYPE == "dataset" && !isTruthy(value)) {
        "Database Name is required for datasets"
      }
    })

    iv$add_rule("DB_PROVIDER", function(value) {
      if (input$REFERENCE_TYPE == "dataset" && !isTruthy(value)) {
        "Database Provider is required for datasets"
      }
    })

    ### Character limit validations for optional fields ----
    char_limit_fields <- list(
      ACCESSION_NUMBER = 200,
      DB_NAME = 200,
      DB_PROVIDER = 200,
      DOCUMENT_NUMBER = 200,
      DOI = 200,
      EDITION = 200,
      INSTITUTION = 200,
      ISBN_ISSN = 200,
      NUMBER_OF_PAGES = 50,
      NUMBER_OF_VOLUMES = 100,
      PAGES = 200,
      PERIODICAL_JOURNAL = 200,
      PMCID = 200,
      PUBLISHED_PLACE = 200,
      PUBLISHER = 200,
      REF_COMMENT = 1000,
      SERIES_EDITOR = 200,
      SERIES_TITLE = 200,
      URL = 200
    )

    for (field_name in names(char_limit_fields)) {
      limit <- char_limit_fields[[field_name]]
      iv$add_rule(field_name, function(value) {
        if (isTruthy(value) && nchar(value) > limit) {
          paste(field_name, "must be", limit, "characters or less")
        }
      })
    }

    ## InputValidator$enable() ----
    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: update ENTERED_BY field with user_id ----
    # upstream: session$userData$reactiveValues$ENTERED_BY
    # downstream: input$ENTERED_BY
    observe({
        updateTextInput(session, "ENTERED_BY", value = session$userData$reactiveValues$ENTERED_BY)
    }) |> bindEvent(session$userData$reactiveValues$ENTERED_BY)

    ## observe: conditional field enable/disable based on reference type ----
    # upstream: input$REFERENCE_TYPE
    # downstream: field enable/disable states
    observe({
      ref_type <- input$REFERENCE_TYPE

      # Clear non-relevant fields when reference type changes
      if (ref_type != "journal") {
        updateTextInput(session, "PERIODICAL_JOURNAL", value = "")
        updateNumericInput(session, "VOLUME", value = NA)
        updateNumericInput(session, "ISSUE", value = NA)
      }

      if (ref_type != "book") {
        if (ref_type != "report") {
          # PUBLISHER is shared between book and report
          updateTextInput(session, "PUBLISHER", value = "")
        }
      }

      if (ref_type != "report") {
        updateTextInput(session, "INSTITUTION", value = "")
        if (ref_type != "book") {
          # PUBLISHER is shared between book and report
          updateTextInput(session, "PUBLISHER", value = "")
        }
      }

      if (ref_type != "dataset") {
        updateTextInput(session, "DB_NAME", value = "")
        updateTextInput(session, "DB_PROVIDER", value = "")
      }

      # Enable/disable fields based on reference type
      # Journal fields
      if (ref_type == "journal") {
        enable("PERIODICAL_JOURNAL")
        enable("VOLUME")
        enable("ISSUE")
      } else {
        disable("PERIODICAL_JOURNAL")
        disable("VOLUME")
        disable("ISSUE")
      }

      # Book fields
      if (ref_type == "book") {
        enable("PUBLISHER")
      } else if (ref_type != "report") {
        # PUBLISHER shared with report
        disable("PUBLISHER")
      }

      # Report fields
      if (ref_type == "report") {
        enable("INSTITUTION")
        enable("PUBLISHER")
      } else {
        disable("INSTITUTION")
        if (ref_type != "book") {
          # PUBLISHER shared with book
          disable("PUBLISHER")
        }
      }

      # Dataset fields
      if (ref_type == "dataset") {
        enable("DB_NAME")
        enable("DB_PROVIDER")
      } else {
        disable("DB_NAME")
        disable("DB_PROVIDER")
      }
    })

    ## observe: DOI lookup placeholder ----
    # upstream: input$lookup_doi
    # downstream: field updates (placeholder)
    observe({
      # TODO: Implement DOI lookup functionality
      # This would query crossref.org or similar service
      # and populate fields automatically
      showNotification(
        "DOI lookup functionality to be implemented",
        type = "message"
      )
    }) |>
      bindEvent(input$lookup_doi)

    ## observe: BibTeX import placeholder ----
    # upstream: input$import_bibtex
    # downstream: field updates (placeholder)
    observe({
      # TODO: Implement BibTeX parsing functionality
      # This would parse the BibTeX entry and populate fields
      showNotification(
        "BibTeX import functionality to be implemented",
        type = "message"
      )
    }) |>
      bindEvent(input$import_bibtex)

    ## observe: check validation status and send to session$userData ----
    # upstream: iv
    # downstream: moduleState$validated_data, moduleState$is_valid
    observe({
      if (iv$is_valid()) {
        # Collect validated data
        validated_data <- list(
          REFERENCE_TYPE = input$REFERENCE_TYPE,
          AUTHOR = input$AUTHOR,
          TITLE = input$TITLE,
          YEAR = input$YEAR,
          ACCESS_DATE = input$ACCESS_DATE,
          PERIODICAL_JOURNAL = input$PERIODICAL_JOURNAL %|truthy|% NA,
          VOLUME = input$VOLUME %|truthy|% NA,
          ISSUE = input$ISSUE %|truthy|% NA,
          PUBLISHER = input$PUBLISHER %|truthy|% NA,
          INSTITUTION = input$INSTITUTION %|truthy|% NA,
          DB_NAME = input$DB_NAME %|truthy|% NA,
          DB_PROVIDER = input$DB_PROVIDER %|truthy|% NA,
          DOI = input$DOI %|truthy|% NA,
          URL = input$URL %|truthy|% NA,
          PAGES = input$PAGES %|truthy|% NA,
          ISBN_ISSN = input$ISBN_ISSN %|truthy|% NA,
          EDITION = input$EDITION %|truthy|% NA,
          PUBLISHED_PLACE = input$PUBLISHED_PLACE %|truthy|% NA,
          DOCUMENT_NUMBER = input$DOCUMENT_NUMBER %|truthy|% NA,
          ACCESSION_NUMBER = input$ACCESSION_NUMBER %|truthy|% NA,
          PMCID = input$PMCID %|truthy|% NA,
          SERIES_TITLE = input$SERIES_TITLE %|truthy|% NA,
          SERIES_EDITOR = input$SERIES_EDITOR %|truthy|% NA,
          SERIES_VOLUME = input$SERIES_VOLUME %|truthy|% NA,
          NUMBER_OF_PAGES = input$NUMBER_OF_PAGES %|truthy|% NA,
          NUMBER_OF_VOLUMES = input$NUMBER_OF_VOLUMES %|truthy|% NA,
          REF_COMMENT = input$REF_COMMENT %|truthy|% NA
        )

        moduleState$validated_data <- validated_data
        moduleState$is_valid <- TRUE

        session$userData$reactiveValues$referencesData <- moduleState$validated_data
        print_dev(glue("mod_references is valid: {moduleState$is_valid},
                       session$userData$reactiveValues$referencesData: {session$userData$reactiveValues$referencesData}"))

      } else {
        moduleState$validated_data <- NULL
        moduleState$is_valid <- FALSE
      }
    })

    ## observe ~ bindEvent: Clear fields button ----
    # upstream: user clicks input$clear
    # downstream: all input fields
    observe(
      {
        # Reset all inputs to default values
        updateSelectInput(session, "REFERENCE_TYPE", selected = "journal")
        updateTextAreaInput(session, "AUTHOR", value = "")
        updateTextAreaInput(session, "TITLE", value = "")
        updateNumericInput(
          session,
          "YEAR",
          value = as.numeric(format(Sys.Date(), "%Y"))
        )
        updateDateInput(session, "ACCESS_DATE", value = as.Date(NA))
        updateTextInput(session, "PERIODICAL_JOURNAL", value = "")
        updateNumericInput(session, "VOLUME", value = NA)
        updateNumericInput(session, "ISSUE", value = NA)
        updateTextInput(session, "PUBLISHER", value = "")
        updateTextInput(session, "INSTITUTION", value = "")
        updateTextInput(session, "DB_NAME", value = "")
        updateTextInput(session, "DB_PROVIDER", value = "")
        updateTextInput(session, "DOI", value = "")
        updateTextInput(session, "URL", value = "")
        updateTextInput(session, "PAGES", value = "")
        updateTextInput(session, "ISBN_ISSN", value = "")
        updateTextInput(session, "EDITION", value = "")
        updateTextInput(session, "PUBLISHED_PLACE", value = "")
        updateTextInput(session, "DOCUMENT_NUMBER", value = "")
        updateTextInput(session, "ACCESSION_NUMBER", value = "")
        updateTextInput(session, "PMCID", value = "")
        updateTextInput(session, "SERIES_TITLE", value = "")
        updateTextInput(session, "SERIES_EDITOR", value = "")
        updateNumericInput(session, "SERIES_VOLUME", value = NA)
        updateTextInput(session, "NUMBER_OF_PAGES", value = "")
        updateTextInput(session, "NUMBER_OF_VOLUMES", value = "")
        updateTextAreaInput(session, "REF_COMMENT", value = "")
        updateTextInput(session, "doi_lookup", value = "")
        updateTextAreaInput(session, "bibtex_import", value = "")

        # Clear validation state
        moduleState$validated_data <- NULL
        moduleState$is_valid <- FALSE
      } |>
        suppressWarnings()
    ) |>
      bindEvent(input$clear)

    # 3. Outputs ----

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid
    # downstream: UI update
    output$validation_reporter <- renderUI({
      if (moduleState$is_valid) {
        div(
          bs_icon("clipboard2-check"),
          "All data validated successfully.",
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Please ensure all required fields are filled, and all entered data is properly formatted.",
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: validated_data_display ----
    # upstream: moduleState$validated_data
    # downstream: UI update
    output$validated_data_display <- renderText(
      {
        if (isTruthy(moduleState$validated_data)) {
          printreactiveValues(moduleState$validated_data)
        } else {
          "# Data object will be created when valid data is entered."
        }
      }
    )

    # 4. Return ----
    ## return: validated data for other modules ----
    # upstream: moduleState$validated_data
    # downstream: server.R
    return(
      reactive({
        moduleState$validated_data %|truthy|% NULL
      })
    )
  })
}

## To be copied in the UI ----
# mod_references_ui("references_1")

## To be copied in the server ----
# references_data <- mod_references_server("references_1")
#
# # Access validated data in other parts of your app:
# observe({
#   if (isTruthy(references_data())) {
#     # Do something with the validated data
#     print("References data validated!")
#     print(references_data())
#   }
# })
