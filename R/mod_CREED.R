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
#' @importFrom shiny NS tagList textInput textAreaInput actionButton checkboxInput renderText
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel input_task_button
#' @importFrom bsicons bs_icon
#' @export
mod_CREED_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main CREED card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_CREED.md"),
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
        div(
          style = "margin: 20px 0;",
          h5("Dataset Purpose Statement"),
          p(
            "Give the purpose of the parent chemical/ecological assessment here. (WIP)",
            class = "text-muted"
          )
        ),
        accordion(accordion_panel(
          title = "Purpose Statement",
          mod_CREED_purpose_ui("CREED_purpose"),
        )),
        ## Dataset Details section ----
        div(
          style = "margin: 20px 0;",
          h5("Dataset Details - Key Attributes"),
          p(
            "This section provides basic details on the dataset. 
            Review the auto-populated fields below and add any missing information.",
            class = "text-muted"
          )
        ),
        accordion(accordion_panel(
          title = "Dataset Details",
          mod_CREED_details_ui("CREED_details")
        )),

        ## Gateway Criteria ----
        div(
          style = "margin: 20px 0;",
          h5("Gateway Criteria"),
          p(
            "CREED's gateway criteria are designed to allow for the easy 
    rejection of a study without requiring methodical examination.
    Consequently, most studies processed using this tool can be expected 
    to pass these criteria without issue. Nevertheless they are included
    for the sake of completeness.
    Each criterion is auto-evaluated based on your data, but can be 
    manually overridden.",
            class = "text-muted"
          )
        ),
        accordion(accordion_panel(
          title = "Gateway Criteria",
          mod_CREED_gateway_ui("CREED_gateway")
        )),

        ## Reliability Criteria ---
        div(
          style = "margin: 20px 0;",
          h5("Reliability Criteria"),
          p(
            "Is the study reliable?",
            class = "text-muted"
          )
        ),
        accordion(accordion_panel(
          title = "Reliability Criteria",
          mod_CREED_reliability_ui("CREED_reliability")
        )),

        ## Relevance Criteria ---
        div(
          style = "margin: 20px 0;",
          h5("Relevance Criteria"),
          p(
            "Is the study relevant to your goals? (again, probably yes)",
            class = "text-muted"
          )
        ),

        accordion(accordion_panel(
          title = "Relevance Criteria",
          mod_CREED_relevance_ui("CREED_relevance")
        )),

        ## Status display ----
        div(
          style = "margin-top: 15px;",
          uiOutput(ns("status_reporter"))
        ),

        ## Future CREED sections placeholder ----
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
      dataset_details = NULL,
      assessment_saved = FALSE
    )

    ## Reactive: moduleData ----
    # upstream: session$userData$reactiveValues
    # downstream: all helper functions
    moduleData <- reactive({
      list(
        campaign = session$userData$reactiveValues$campaignData,
        references = session$userData$reactiveValues$referencesData,
        sites = session$userData$reactiveValues$sitesData,
        parameters = session$userData$reactiveValues$parametersData,
        compartments = session$userData$reactiveValues$compartmentsData,
        samples = session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
          session$userData$reactiveValues$sampleData,
        methods = session$userData$reactiveValues$methodsData,
        measurements = session$userData$reactiveValues$dataData
      )
    })

    ## Reactive: gateway_summaries ----
    # upstream: moduleData()
    # downstream: gateway summary outputs
    gateway_summaries <- reactive({
      get_gateway_summaries(moduleData())
    })

    mod_CREED_purpose_server("CREED_purpose")
    mod_CREED_details_server("CREED_details")
    mod_CREED_gateway_server("CREED_gateway")
    mod_CREED_reliability_server("CREED_reliability")
    mod_CREED_relevance_server("CREED_relevance")

    # 2. Helper functions ----

    ## Auto-populate gateway criteria ----
    auto_populate_gateway_criteria <- function() {
      # Get gateway availability
      availability <- check_gateway_availability(moduleData())

      # Update checkboxes
      updateCheckboxInput(
        session,
        "gateway_medium_answer",
        value = availability$medium
      )
      updateCheckboxInput(
        session,
        "gateway_analyte_answer",
        value = availability$analyte
      )
      updateCheckboxInput(
        session,
        "gateway_location_answer",
        value = availability$location
      )
      updateCheckboxInput(
        session,
        "gateway_year_answer",
        value = availability$year
      )
      updateCheckboxInput(
        session,
        "gateway_units_answer",
        value = availability$units
      )
      updateCheckboxInput(
        session,
        "gateway_citation_answer",
        value = availability$citation
      )
    }

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
    # downstream: moduleState$dataset_details, session$userData
    observe({
      # Collect all dataset details
      dataset_details <- list(
        # Auto-populated fields
        source = input$source_auto,
        reported_analyte = input$analyte_auto,
        sample_medium = input$medium_auto,
        study_area = input$study_area_auto,
        number_of_sites = input$num_sites_auto,
        number_of_samples = input$num_samples_auto,
        sampling_period = input$sampling_period_auto,
        analytical_methods = input$analytical_methods_auto,
        limit_of_quantification = input$loq_auto,

        # User input fields
        sampling_conditions = input$sampling_conditions %|truthy|% "",
        site_density = input$site_density %|truthy|% "",
        site_types = input$site_types %|truthy|% "",
        sampling_frequency = input$sampling_frequency %|truthy|% "",
        sampling_methods = input$sampling_methods %|truthy|% "",
        other_details = input$other_details %|truthy|% "",

        # Gateway Criteria - simplified structure
        gateway_criteria = list(
          medium_matrix = input$gateway_medium_answer %|truthy|% FALSE,
          analyte = input$gateway_analyte_answer %|truthy|% FALSE,
          spatial_location = input$gateway_location_answer %|truthy|% FALSE,
          year = input$gateway_year_answer %|truthy|% FALSE,
          units = input$gateway_units_answer %|truthy|% FALSE,
          citation = input$gateway_citation_answer %|truthy|% FALSE
        ),

        # Metadata
        created_date = Sys.time(),
        created_by = session$userData$reactiveValues$ENTERED_BY %|truthy|%
          "Unknown"
      )

      moduleState$dataset_details <- dataset_details
      moduleState$assessment_saved <- TRUE

      # Save to session userData
      session$userData$reactiveValues$creedAssessment <- dataset_details

      showNotification(
        "CREED assessment saved successfully",
        type = "message"
      )

      print_dev("CREED assessment saved to session data")
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
