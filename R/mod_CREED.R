# CREED Quality Assessment Module ----
# A Shiny module for CREED-based dataset quality assessment

#' CREED UI Function ----
#'
#' @description A shiny Module for CREED dataset quality assessment.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput textAreaInput actionButton checkboxInput renderText markdown
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs disabled
#' @export
mod_CREED_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main CREED card ----
    card(
      fill = TRUE,
      full_screen = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(
          content_file = "inst/app/www/md/intro_CREED.md"
        ),

        div(
          div(
            style = "margin: 10px 10px 0 10px; display: flex; align-items: center; gap: 15px;",
            actionButton(
              inputId = ns("get_data"),
              label = "Get Data from Modules",
              icon = icon("refresh"),
              class = "btn-primary"
            ),
            uiOutput(ns("validation_reporter")),
          ),
          br(),
          div(
            class = "alert alert-primary",
            p(
              bs_icon("arrow-down-circle-fill", class = "text-primary"),
              strong(" Auto-population: "),
              "Fields marked with this icon are auto-populated from data entered in 
          earlier modules. This data can be overwritten as needed, but note that
          if you populate fields from data again, your changes will not be 
          saved."
            )
          )
        ),

        ## Purpose Statement ---
        h5("1. Purpose Statement"),
        markdown(
          "- The CREED grading process begins with laying out the purpose of 
            your overall chemical/ecological impact or risk assessment, which 
            shapes the criteria used. 
            - As it is usual to include multiple datasets
            in such an assessment, we recommend reusing a single Purpose Statement
            per assessment.
            "
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Purpose Statement",
            mod_CREED_purpose_ui(NS(id, "CREED_purpose"))
          )
        ),

        ## Dataset Details  ----
        h5("2. Dataset Details - Key Attributes"),
        markdown(
          "- This section provides a summary of basic details for the dataset. 
          - Review the auto-populated fields below and add any missing information."
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Dataset Details",
            mod_CREED_details_ui(NS(id, "CREED_details"))
          )
        ),

        ## Gateway Criteria ----
        h5("3. Gateway Criteria"),
        markdown(
          "- CREED's gateway criteria are designed to allow for the easy 
          rejection of a study without requiring methodical examination.
          - Most studies processed using this tool can be expected 
          to pass these criteria without issue. 
          
          - Nevertheless they are included for the sake of completeness.
          - Each criterion is auto-evaluated based on your entered data, but can be 
          manually overridden."
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Gateway Criteria",
            mod_CREED_gateway_ui(NS(id, "CREED_gateway"))
          )
        ),

        ## Reliability Criteria ---
        h5("4. Reliability Criteria"),
        markdown(
          "Assess how reliable the dataset is for answering your assessment 
          questions."
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Reliability Criteria",
            mod_CREED_reliability_ui(NS(id, "CREED_reliability"))
          )
        ),

        ## Relevance Criteria ---

        h5("5. Relevance Criteria"),
        markdown(
          "Assess how relevant the dataset is to the purpose as described in your
          Purpose Statement."
        ),

        accordion(
          open = FALSE,
          accordion_panel(
            title = "Relevance Criteria",
            mod_CREED_relevance_ui(NS(id, "CREED_relevance"))
          )
        ),

        ## Status display ----
        div(
          style = "margin-top: 15px;",
          uiOutput(ns("status_reporter"))
        ),

        ## Final Report ----
        div(
          style = "margin: 20px 0;",
          h5("6. Generate Final Report"),
          p(
            "Work in progress.",
            class = "text-muted"
          )
        )
      )
    )
  )
}

#' CREED Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI showNotification updateTextAreaInput updateCheckboxInput bindEvent
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom shinyjs enable disable

#' @export
mod_CREED_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      ready_for_assessment = FALSE,
      assessment_saved = FALSE
    )

    # Call sub-module servers ----
    mod_CREED_purpose_server("CREED_purpose")
    mod_CREED_details_server("CREED_details")
    mod_CREED_gateway_server("CREED_gateway")
    mod_CREED_reliability_server("CREED_reliability")
    mod_CREED_relevance_server("CREED_relevance")

    # 2. Helper functions ----

    # 3. Observers and Reactives ----
    ## observe: enable CREED assessment only when all previous modules have been filled out ----
    # upstream: nrow(dataData) > 0
    # downstream: pretty much everything in the module
    observe({
      if (nrow(session$userData$reactiveValues$dataData) > 0) {
        moduleState$ready_for_assessment <- TRUE
        enable(id = "input$get_data")
      } else {
        moduleState$ready_for_assessment <- FALSE
        disable(id = "input$get_data")
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$dataData
      )

    ## observe ~bindEvent(save_assessment): Save CREED assessment ----
    # upstream: user clicks input$save_assessment
    # downstream: moduleState$dataset_details, session$userData$reactiveValues$creedData
    observe(
      {
        print_dev("autopop all triggered")
        # individual submodule functions go here.
      }
    ) |>
      bindEvent(input$get_data)

    # 4. Outputs ----

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      validation_status <- if (moduleState$ready_for_assessment) {
        div(
          bs_icon("clipboard2-check"),
          "Data module validated successfully.",
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Please complete the data module before starting CREED assessment..",
          class = "validation-status validation-warning"
        )
      }

      div(validation_status, class = "validation-container")
    })

    ## output: status_reporter ----
    # upstream: moduleState$assessment_saved
    # downstream: UI status display
    output$status_reporter <- renderUI({
      if (moduleState$assessment_saved) {
        div(
          bs_icon("check-circle"),
          "CREED assessment saved successfully. Dataset details are ready for quality evaluation.",
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("info-circle"),
          "Review dataset details above and click 'Save Assessment' when ready.",
          class = "validation-status validation-info"
        )
      }
    })
  })
}

## To be copied in the UI ----
# mod_creed_ui("creed_1")

## To be copied in the server ----
# mod_creed_server("creed_1")
