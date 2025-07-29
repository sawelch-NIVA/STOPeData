# Campaign Import Module ----
# A Shiny module for campaign data entry with validation using shinyvalidate

# Custom truthy operator ----
`%|truthy|%` <- function(lhs, rhs) {
  if (shiny::isTruthy(lhs)) {
    lhs
  } else {
    rhs
  }
}

#' Campaign UI Function ----
#'
#' @description A shiny Module for campaign data entry and validation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput dateInput numericInput textAreaInput
#' @importFrom bslib card card_header card_body layout_column_wrap
mod_campaign_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main input card ----
    card(
      card_header("Campaign Data Entry"),
      card_body(
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

          # RELIABILITY_SCORE - Optional int, 22 char ----
          numericInput(
            inputId = ns("RELIABILITY_SCORE"),
            label = "Reliability Score",
            value = NA,
            min = NA,
            max = 9999999999999999999999, # 22 char limit
            width = "100%"
          ),

          # RELIABILITY_EVAL_SYS - Optional string, 50 char ----
          selectInput(
            inputId = ns("RELIABILITY_EVAL_SYS"),
            label = "Reliability Evaluation System",
            choices = c("CREED", "Other (add to comments)"),
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

        br(),

        # Validation status display ----
        textOutput(ns("validation_status")),

        # Action buttons ----
        actionButton(
          inputId = ns("clear"),
          label = "Clear All",
          class = "btn-secondary",
          width = "300px"
        )
      )
    ),

    verbatimTextOutput(ns("validated_data_display"))
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

    # Reactive values ----
    values <- reactiveValues(
      validated_data = NULL,
      is_valid = FALSE
    )

    # Input validator setup ----
    iv <- InputValidator$new()

    # Required field validations ----
    iv$add_rule("CAMPAIGN_NAME", sv_required())
    iv$add_rule("CAMPAIGN_NAME", function(value) {
      if (shiny::isTruthy(value) && nchar(value) > 100) {
        "Campaign Name must be 100 characters or less"
      }
    })

    iv$add_rule("CAMPAIGN_START_DATE", sv_required())

    iv$add_rule("ORGANISATION", sv_required())
    iv$add_rule("ORGANISATION", function(value) {
      if (shiny::isTruthy(value) && nchar(value) > 50) {
        "Organisation must be 50 characters or less"
      }
    })

    iv$add_rule("ENTERED_BY", sv_required())
    iv$add_rule("ENTERED_BY", function(value) {
      if (shiny::isTruthy(value) && nchar(value) > 50) {
        "Entered By must be 50 characters or less"
      }
    })

    iv$add_rule("ENTERED_DATE", sv_required())

    # Optional field character limit validations ----
    iv$add_rule("RELIABILITY_SCORE", function(value) {
      if (shiny::isTruthy(value) && nchar(as.character(value)) > 22) {
        "Reliability Score must be 22 digits or less"
      }
    })

    iv$add_rule("RELIABILITY_EVAL_SYS", function(value) {
      if (shiny::isTruthy(value) && nchar(value) > 50) {
        "Reliability Evaluation System must be 50 characters or less"
      }
    })

    iv$add_rule("CAMPAIGN_COMMENT", function(value) {
      if (shiny::isTruthy(value) && nchar(value) > 1000) {
        "Campaign Comment must be 1000 characters or less"
      }
    })

    # Date validation for end date ----
    iv$add_rule("CAMPAIGN_END_DATE", function(value) {
      if (
        shiny::isTruthy(value) && shiny::isTruthy(input$CAMPAIGN_START_DATE)
      ) {
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

        values$validated_data <- validated_data
        values$is_valid <- TRUE
      } else {
        values$validated_data <- NULL
        values$is_valid <- FALSE
      }
    })

    # Clear button observer ----
    observe({
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
      values$validated_data <- NULL
      values$is_valid <- FALSE
    }) |>
      bindEvent(input$clear)

    # Validation status display ----
    output$validation_status <- renderText({
      if (values$is_valid && shiny::isTruthy(values$validated_data)) {
        # Format the data for display
        data_str <- paste(
          sapply(names(values$validated_data), function(name) {
            value <- values$validated_data[[name]]
            formatted_value <- value %|truthy|% "[NULL]"
            paste0(name, ": ", as.character(formatted_value))
          }),
          collapse = "\n"
        )
        paste("✓ Data validated successfully!\n\n", data_str)
      } else {
        "No validated data available. Please fill required fields."
      }
    })

    # Return validated data for other modules ----
    return(
      reactive({
        values$validated_data %|truthy|% NULL
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
#   if (shiny::isTruthy(campaign_data())) {
#     # Do something with the validated data
#     print("Campaign data validated!")
#     print(campaign_data())
#   }
# })
