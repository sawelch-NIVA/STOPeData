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
          class = "alert alert-primary",
          p(
            bs_icon("arrow-down-circle-fill", class = "text-primary"),
            strong(" Auto-population: "),
            "Fields marked with this icon are auto-populated from data entered in 
          earlier modules. This data can be overwritten as needed, but note that
          if you populate fields from data again, your changes will not be 
          saved."
          )
        ),

        ## Purpose Statement ---
        h5("Purpose Statement"),
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
            mod_CREED_purpose_ui("CREED_purpose"),
          )
        ),

        ## Dataset Details  ----
        h5("Dataset Details - Key Attributes"),
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
        h5("Gateway Criteria"),
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
            mod_CREED_gateway_ui("CREED_gateway")
          )
        ),

        ## Reliability Criteria ---
        h5("Reliability Criteria"),
        markdown(
          "Assess how reliable the dataset is for answering your assessment 
          questions."
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Reliability Criteria",
            mod_CREED_reliability_ui("CREED_reliability")
          )
        ),

        ## Relevance Criteria ---

        h5("Relevance Criteria"),
        markdown(
          "Assess how relevant the dataset is to the purpose as described in your
          Purpose Statement."
        ),

        accordion(
          open = FALSE,
          accordion_panel(
            title = "Relevance Criteria",
            mod_CREED_relevance_ui("CREED_relevance")
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
          h5("Generate Final Report"),
          p(
            "This part is complicated...",
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
#' @export
mod_CREED_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
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

    ## observe ~bindEvent(update_details): Update auto-populated fields ----
    # upstream: user clicks input$update_details
    # downstream: auto-populated input fields
    observe({
      auto_populate_details()
      showNotification(
        "Dataset details updated from current data",
        type = "message"
      )
    }) |>
      bindEvent(input$update_details)

    ## observe ~bindEvent(save_assessment): Save CREED assessment ----
    # upstream: user clicks input$save_assessment
    # downstream: moduleState$dataset_details, session$userData$reactiveValues$creedData
    observe({
      print_dev("asssessement saved (not really ;)")
    }) |>
      bindEvent(input$save_assessment)

    # 4. Outputs ----

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

    ## Gateway Criteria Summary Outputs ----

    # Gateway 1: Medium/Matrix summary
    output$gateway_medium_summary <- renderText({
      gateway_summaries()$medium
    })

    # Gateway 2: Analyte summary
    output$gateway_analyte_summary <- renderText({
      gateway_summaries()$analyte
    })

    # Gateway 3: Location summary
    output$gateway_location_summary <- renderText({
      gateway_summaries()$location
    })

    # Gateway 4: Year summary
    output$gateway_year_summary <- renderText({
      gateway_summaries()$year
    })

    # Gateway 5: Units summary
    output$gateway_units_summary <- renderText({
      gateway_summaries()$units
    })

    # Gateway 6: Citation summary
    output$gateway_citation_summary <- renderText({
      gateway_summaries()$citation
    })
  })
}

## To be copied in the UI ----
# mod_creed_ui("creed_1")

## To be copied in the server ----
# mod_creed_server("creed_1")
