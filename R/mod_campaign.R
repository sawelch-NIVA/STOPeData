# Campaign Import Module ----
# A Shiny module for campaign data entry with validation using shinyvalidate

#' Campaign UI Function ----
#'
#' @description A shiny Module for campaign data entry and validation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput dateInput selectInput textAreaInput actionButton
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel tooltip
#' @importFrom bsicons bs_icon
mod_campaign_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main input card ----
    card(
      card_header("Campaign Data Entry"),
      card_body(
        # Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Campaign Data Information",
            icon = bs_icon("info-circle"),
            "This form collects basic campaign metadata for environmental sampling data. Required fields are marked with an asterisk (*). All date fields should use ISO format (YYYY-MM-DD). Use the Campaign Comment field for any additional notes about the sampling campaign."
          )
        ),

        # Input fields layout ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          # CAMPAIGN_NAME - Required string, 100 char ----
          textInput(
            inputId = ns("CAMPAIGN_NAME"),
            label = "Campaign Name *",
            placeholder = "e.g., 'Vm_Tilt'",
            width = "100%"
          ) |>
            tooltip(
              bsicons::bs_icon("info-circle", title = "Campaign Name"),
              "Text string used to identify the sampling campaign or project. Ensure a consistent Campaign string is used."
            ),

          # CAMPAIGN_START_DATE - Required date ----
          dateInput(
            inputId = ns("CAMPAIGN_START_DATE"),
            label = "Campaign Start Date *",
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          # CAMPAIGN_END_DATE - Optional date ----
          dateInput(
            inputId = ns("CAMPAIGN_END_DATE"),
            label = "Campaign End Date",
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          # RELIABILITY_EVAL_SYS - Optional string, 50 char ----
          selectInput(
            inputId = ns("RELIABILITY_EVAL_SYS"),
            label = "Reliability Evaluation System",
            choices = c(
              "Not relevant",
              "Not reported",
              "CREED",
              "Other (add to comments)"
            ),
            width = "100%"
          ),

          # RELIABILITY_SCORE - Optional int, 22 char ----
          textInput(
            inputId = ns("RELIABILITY_SCORE"),
            label = "Reliability Score",
            value = NA,
            placeholder = "Numeric or categorical",
            width = "100%"
          ),

          # CONFIDENTIALITY_EXPIRY_DATE - Optional date ----
          dateInput(
            inputId = ns("CONFIDENTIALITY_EXPIRY_DATE"),
            label = "Confidentiality Expiry Date",
            value = NA,
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          # ORGANISATION - Required string, 50 char ----
          textInput(
            inputId = ns("ORGANISATION"),
            label = "Organisation *",
            placeholder = "Data collection organisation",
            width = "100%"
          ),

          # ENTERED_BY - Required string, 50 char ----
          textInput(
            inputId = ns("ENTERED_BY"),
            label = "Entered By *",
            placeholder = "Your initials or name",
            width = "100%"
          ),

          # ENTERED_DATE - Required date ----
          dateInput(
            inputId = ns("ENTERED_DATE"),
            label = "Entered Date *",
            value = Sys.Date(),
            format = "yyyy-mm-dd",
            width = "100%"
          )
        ),

        # CAMPAIGN_COMMENT - Full width text area ----
        textAreaInput(
          inputId = ns("CAMPAIGN_COMMENT"),
          label = "Campaign Comment",
          placeholder = "Campaign-level notes (optional)",
          width = "100%",
          rows = 3
        ),

        # Validation status and raw data ----
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

        # Action buttons ----
        actionButton(
          inputId = ns("clear"),
          label = "Clear All Fields",
          class = "btn-danger",
          width = "300px"
        ),
        br(),
        div(
          # TODO: Unified nav buttons from v0.3
          # TODO: Fix observer.
          class = "navigation-buttons-container",
          style = "display: flex; flex-direction: row-reverse;",
          actionButton(
            inputId = ns("next_section"),
            label = "Next Section",
            class = "btn-success",
            width = "300px",
          )
        )
      )
    )
  )
}

#' Campaign Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText updateTextInput updateDateInput updateNumericInput updateTextAreaInput bindEvent
mod_campaign_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      validated_data = NULL,
      is_valid = FALSE
    )

    # Input validator setup ----
    iv <- InputValidator$new()

    # Required field validations ----
    iv$add_rule("CAMPAIGN_NAME", sv_required())
    iv$add_rule("CAMPAIGN_NAME", function(value) {
      if (isTruthy(value) && nchar(value) > 100) {
        "Campaign Name must be 100 characters or less"
      }
    })

    iv$add_rule("CAMPAIGN_START_DATE", sv_required())

    iv$add_rule("ORGANISATION", sv_required())
    iv$add_rule("ORGANISATION", function(value) {
      if (isTruthy(value) && nchar(value) > 50) {
        "Organisation must be 50 characters or less"
      }
    })

    iv$add_rule("ENTERED_BY", sv_required())
    iv$add_rule("ENTERED_BY", function(value) {
      if (isTruthy(value) && nchar(value) > 50) {
        "Entered By must be 50 characters or less"
      }
    })

    iv$add_rule("ENTERED_DATE", sv_required())

    # Conditional: if RELIABILITY_EVAL_SYS isTruthy, then require a score ----
    iv$add_rule("RELIABILITY_SCORE", function(value) {
      if (isTruthy(value) && nchar(as.character(value)) > 22) {
        "Reliability Score must be 22 characters or less"
      }
    })

    iv$add_rule("RELIABILITY_SCORE", function(value) {
      if (
        isTruthy(input$RELIABILITY_EVAL_SYS) &&
          isRelevant(input$RELIABILITY_EVAL_SYS) &&
          !isTruthy(value)
      ) {
        "If an evaluation system is selected a score must also be entered."
      }
    })

    iv$add_rule("CAMPAIGN_COMMENT", function(value) {
      if (isTruthy(value) && nchar(value) > 1000) {
        "Campaign Comment must be 1000 characters or less"
      }
    })

    # Date validation for end date ----
    iv$add_rule("CAMPAIGN_END_DATE", function(value) {
      if (isTruthy(value) && isTruthy(input$CAMPAIGN_START_DATE)) {
        if (value < input$CAMPAIGN_START_DATE) {
          "Campaign End Date must be after Start Date"
        }
      }
    })

    # Enable validation ----
    iv$enable()

    # Validation observer ----
    observe({
      if (iv$is_valid()) {
        # Collect validated data ----
        validated_data <- list(
          CAMPAIGN_NAME = input$CAMPAIGN_NAME,
          CAMPAIGN_START_DATE = input$CAMPAIGN_START_DATE,
          CAMPAIGN_END_DATE = input$CAMPAIGN_END_DATE %|truthy|% as.Date(NA),
          RELIABILITY_SCORE = input$RELIABILITY_SCORE %|truthy|% NA,
          RELIABILITY_EVAL_SYS = input$RELIABILITY_EVAL_SYS %|truthy|% NA,
          CONFIDENTIALITY_EXPIRY_DATE = input$CONFIDENTIALITY_EXPIRY_DATE %|truthy|%
            as.Date(NA),
          ORGANISATION = input$ORGANISATION,
          ENTERED_BY = input$ENTERED_BY,
          ENTERED_DATE = input$ENTERED_DATE,
          CAMPAIGN_COMMENT = input$CAMPAIGN_COMMENT %|truthy|% NA
        )

        moduleState$validated_data <- validated_data
        moduleState$is_valid <- TRUE
      } else {
        moduleState$validated_data <- NULL
        moduleState$is_valid <- FALSE
      }
    })

    # Clear button observer ----
    observe(
      {
        # Reset all inputs to default values
        updateTextInput(session, "CAMPAIGN_NAME", value = "")
        updateDateInput(session, "CAMPAIGN_START_DATE", value = as.Date(NA))
        updateDateInput(session, "CAMPAIGN_END_DATE", value = as.Date(NA))
        updateNumericInput(session, "RELIABILITY_SCORE", value = NA)
        updateTextInput(session, "RELIABILITY_EVAL_SYS", value = "")
        updateDateInput(
          session,
          "CONFIDENTIALITY_EXPIRY_DATE",
          value = as.Date(NA)
        )
        updateTextInput(session, "ORGANISATION", value = "")
        updateTextInput(session, "ENTERED_BY", value = "")
        updateDateInput(session, "ENTERED_DATE", value = Sys.Date())
        updateTextAreaInput(session, "CAMPAIGN_COMMENT", value = "")

        # Clear validation state
        moduleState$validated_data <- NULL
        moduleState$is_valid <- FALSE
      } |>
        suppressWarnings()
    ) |>
      bindEvent(input$clear)

    # Validation reporter output ----
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

    # Validated data display ----
    output$validated_data_display <- renderText({
      if (isTruthy(moduleState$validated_data)) {
        # Format as R code
        data_lines <- sapply(names(moduleState$validated_data), function(name) {
          value <- moduleState$validated_data[[name]]
          if (is.na(value) || is.null(value)) {
            paste0("  ", name, " = NA")
          } else if (inherits(value, "Date")) {
            paste0("  ", name, " = as.Date('", as.character(value), "')")
          } else if (is.character(value)) {
            paste0("  ", name, " = '", value, "'")
          } else {
            paste0("  ", name, " = ", as.character(value))
          }
        })

        paste(c(data_lines, collapse = ",\n"))
      } else {
        "# Data object will be created when valid data is entered."
      }
    })

    # Return validated data for other modules ----
    return(
      reactive({
        moduleState$validated_data %|truthy|% NULL
      })
    )
  })
}

## To be copied in the UI ----
# mod_campaign_ui("campaign_1")

## To be copied in the server ----
# campaign_data <- mod_campaign_server("campaign_1")
#
# # Access validated data in other parts of your app:
# observe({
#   if (isTruthy(campaign_data())) {
#     # Do something with the validated data
#     print("Campaign data validated!")
#     print(campaign_data())
#   }
# })
