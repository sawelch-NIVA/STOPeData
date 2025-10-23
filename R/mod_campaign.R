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
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel tooltip
#' @importFrom bsicons bs_icon
#' @importFrom tibble tibble
#' @export
mod_campaign_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main input card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_campaign.md"),
        ## Input fields layout ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### CAMPAIGN_NAME - Required string, 100 char ----
          textInput(
            inputId = ns("CAMPAIGN_NAME"),
            label = tooltip(
              list("Campaign Name", bs_icon("info-circle-fill")),
              "Text string used to identify the sampling campaign or project. Ensure a consistent Campaign string is used."
            ),
            placeholder = "e.g., 'Vannmiljø Mitigation Monitoring 2025'",
            width = "100%"
          ),

          ### CAMPAIGN_NAME_SHORT - Required string, 10 char ----
          textInput(
            inputId = ns("CAMPAIGN_NAME_SHORT"),
            label = tooltip(
              list("Campaign Name Short", bs_icon("info-circle-fill")),
              "Abbreviated campaign identifier (max 10 characters). Use for compact displays and references."
            ),
            placeholder = "e.g., 'Vm_Tilt_2025'",
            width = "100%"
          ),

          ### CAMPAIGN_START_DATE - Required date ----
          dateInput(
            inputId = ns("CAMPAIGN_START_DATE"),
            label = tooltip(
              list("Campaign Start Date", bs_icon("info-circle-fill")),
              "The official or actual data of first sampling."
            ),
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          ### CAMPAIGN_END_DATE - Optional date ----
          dateInput(
            inputId = ns("CAMPAIGN_END_DATE"),
            label = tooltip(
              list("Campaign End Date", bs_icon("info-circle-fill")),
              "The latest date of sampling or analysis."
            ),
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          ### RELIABILITY_EVAL_SYS - Optional string, 50 char ----
          selectInput(
            inputId = ns("RELIABILITY_EVAL_SYS"),
            label = tooltip(
              list(
                "Reliability Evaluation System",
                bs_icon("info-circle-fill")
              ),
              "The system used to evaluate data quality."
            ),
            choices = c(
              "Not relevant",
              "Not reported",
              "CREED",
              "Other (add to comments)"
            ),
            width = "100%"
          ),

          ### RELIABILITY_SCORE - Optional int, 22 char ----
          textInput(
            inputId = ns("RELIABILITY_SCORE"),
            label = tooltip(
              list("Reliability Score", bs_icon("info-circle-fill")),
              "The score given (numeric or categorical) under the Reliability Evaluation System, if relevant."
            ),
            value = NA,
            placeholder = "Numeric or categorical",
            width = "100%"
          ),

          ### CONFIDENTIALITY_EXPIRY_DATE - Optional date ----
          dateInput(
            inputId = ns("CONFIDENTIALITY_EXPIRY_DATE"),
            label = tooltip(
              list("Confidentiality Expiry Date", bs_icon("info-circle-fill")),
              "The date at which the data leaves embargo or ceases to be confidential."
            ),
            value = NA,
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          ### ORGANISATION - Required string, 50 char ----
          textInput(
            inputId = ns("ORGANISATION"),
            label = tooltip(
              list("Organisation", bs_icon("info-circle-fill")),
              "The principal organisation(s) responsible for collecting or creating the original data: authors' institution, report publishing institution, etc."
            ),
            placeholder = "Data collection organisation",
            width = "100%"
          ),

          ### ENTERED_BY - Required string, 50 char ----
          textInput(
            inputId = ns("ENTERED_BY"),
            label = tooltip(
              list("Entered By", bs_icon("info-circle-fill")),
              "Your name."
            ),
            placeholder = "Your name",
            width = "100%"
          ),

          ### ENTERED_DATE - Required date ----
          dateInput(
            inputId = ns("ENTERED_DATE"),
            label = tooltip(
              list("Entered Date", bs_icon("info-circle-fill")),
              "The date you are entering this data into the app."
            ),
            value = Sys.Date(),
            format = "yyyy-mm-dd",
            width = "100%"
          )
        ),

        ### CAMPAIGN_COMMENT - Full width text area ----
        textAreaInput(
          inputId = ns("CAMPAIGN_COMMENT"),
          label = tooltip(
            list("Campaign Comments", bs_icon("info-circle-fill")),
            "Use this space to enter any potentially relevant or noteworthy comments or remarks about the overall campaign."
          ),
          placeholder = "Campaign-level notes (optional)",
          width = "100%",
          rows = 3
        ),

        ## Validation status and raw data ----
        span(
          # prevent flex-grow validation element from growing vertically
          uiOutput(ns("validation_reporter"))
        ),
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

#' Campaign Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText updateTextInput updateDateInput updateNumericInput updateTextAreaInput bindEvent
#' @importFrom glue glue
#' @importFrom tibble add_row
#' @export
mod_campaign_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      validated_data = initialise_campaign_tibble(),
      is_valid = FALSE
    )

    ## InputValidator$new: iv ----
    iv <- InputValidator$new()
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

    ### Conditional field validations
    # if RELIABILITY_EVAL_SYS isTruthy, then require a score
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

    # Date validation for end date
    iv$add_rule("CAMPAIGN_END_DATE", function(value) {
      if (isTruthy(value) && isTruthy(input$CAMPAIGN_START_DATE)) {
        if (value < input$CAMPAIGN_START_DATE) {
          "Campaign End Date must be after Start Date"
        }
      }
    })

    # CAMPAIGN_NAME_SHORT is short
    iv$add_rule("CAMPAIGN_NAME_SHORT", sv_required())
    iv$add_rule("CAMPAIGN_NAME_SHORT", function(value) {
      if (isTruthy(value) && nchar(value) > 20) {
        "Campaign Name Short must be 20 characters or less"
      }
    })

    ## InputValidator$enable() ----
    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: check validation status and send to session$userData ----
    # imports: fct_formats::initialise_campaign_tibble()
    # upstream: iv
    # downstream: moduleState$validated_data, moduleState$is_valid, session$userData$reactiveValues
    observe({
      if (iv$is_valid()) {
        # Collect validated data
        validated_data <- tryCatch(
          {
            # uses standardised format from fct_formats, will fail if format/data types not respected
            initialise_campaign_tibble() |>
              add_row(
                CAMPAIGN_NAME_SHORT = input$CAMPAIGN_NAME_SHORT,
                CAMPAIGN_NAME = input$CAMPAIGN_NAME,
                CAMPAIGN_START_DATE = input$CAMPAIGN_START_DATE,
                CAMPAIGN_END_DATE = input$CAMPAIGN_END_DATE %|truthy|%
                  as.Date(NA),
                RELIABILITY_SCORE = input$RELIABILITY_SCORE %|truthy|% NA,
                RELIABILITY_EVAL_SYS = input$RELIABILITY_EVAL_SYS %|truthy|% NA,
                CONFIDENTIALITY_EXPIRY_DATE = input$CONFIDENTIALITY_EXPIRY_DATE %|truthy|%
                  as.Date(NA),
                ORGANISATION = input$ORGANISATION,
                ENTERED_BY = input$ENTERED_BY,
                ENTERED_DATE = input$ENTERED_DATE,
                CAMPAIGN_COMMENT = input$CAMPAIGN_COMMENT %|truthy|% NA
              )
          },
          error = function(e) {
            stop(
              "Column mismatch in campaign data collection: ",
              e$message,
              call. = FALSE
            )
          }
        )

        moduleState$validated_data <- validated_data
        moduleState$is_valid <- TRUE

        session$userData$reactiveValues$campaignData <- moduleState$validated_data
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
        updateTextInput(session, "CAMPAIGN_NAME_SHORT", value = "")
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
        moduleState$validated_data <- initialise_campaign_tibble()
        moduleState$is_valid <- FALSE
      } |>
        suppressWarnings()
    ) |>
      bindEvent(input$clear)

    ## observe ~ bindEvent: Set session username from ENTERED_BY ----
    observe({
      req(input$ENTERED_BY)

      # only trigger if a username doesn't already exist in the session
      if (!isTruthy(session$userData$reactiveValues$ENTERED_BY)) {
        # Set the reactive value
        session$userData$reactiveValues$ENTERED_BY <- input$ENTERED_BY

        showNotification(
          glue("Saved your username {input$ENTERED_BY} to session data."),
          type = "message"
        )
      }
    }) |>
      bindEvent(input$ENTERED_BY, ignoreInit = TRUE)

    ## observe: update ENTERED_BY field with user_id ----
    # upstream: session$userData$reactiveValues$ENTERED_BY
    # downstream: input$ENTERED_BY
    observe({
      updateTextInput(
        session,
        "ENTERED_BY",
        value = session$userData$reactiveValues$ENTERED_BY
      )
    }) |>
      bindEvent(session$userData$reactiveValues$ENTERED_BY)

    ## observe: Populate from LLM data when available ----
    # upstream: session$userData$reactiveValues$campaignDataLLM
    # downstream: input fields
    observe({
      llm_data <- session$userData$reactiveValues$campaignDataLLM
      if (
        !is.null(llm_data) &&
          session$userData$reactiveValues$llmExtractionComplete
      ) {
        populate_campaign_from_llm(session, llm_data)

        showNotification(
          "Campaign form populated.",
          type = "message"
        )
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$campaignDataLLM,
        session$userData$reactiveValues$llmExtractionComplete,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

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
    output$validated_data_display <- renderText({
      if (isTruthy(moduleState$validated_data)) {
        printreactiveValues(moduleState$validated_data)
      } else {
        "# Data object will be created when valid data is entered."
      }
    })
  })
}

## To be copied in the UI ----
# mod_campaign_ui("campaign_1")

## To be copied in the server ----
# campaign_data <- mod_campaign_server("campaign_1")
