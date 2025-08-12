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
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel input_task_button
#' @importFrom bsicons bs_icon
#' @export
mod_CREED_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main CREED card ----
    card(
      fill = TRUE,
      card_header("Dataset Quality Assessment (CREED)"),
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_CREED.md"),

        ## Purpose Statement ---
        div(
          style = "margin: 20px 0;",
          h5("Dataset Purpose Statement"),
          p(
            "Give the purpose of the parent chemical/ecological assessment here. (WIP)",
            class = "text-muted"
          )
        ),
        ## Dataset Details section ----
        div(
          style = "margin: 20px 0;",
          h5("Dataset Details - Key Attributes"),
          p(
            "This section providse basic details on the dataset. 
            Review the auto-populated fields below and add any missing information.",
            class = "text-muted"
          )
        ),

        input_task_button(
          id = ns("update_details"),
          label = "Update Dataset Details",
          icon = icon("refresh"),
          class = "btn-primary",
          width = "200px"
        ),

        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### Auto-populated fields ----
          div(
            span(
              bs_icon("arrow-down-circle-fill", class = "text-primary"),
              h6(
                "Populated from entered data, can be overwritten as needed",
                style = "color: #0066cc; display: contents;"
              )
            ),

            textAreaInput(
              inputId = ns("source_auto"),
              label = tooltip(
                list(
                  "Source (reference)",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Reference citation or source of the dataset being processed."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("analyte_auto"),
              label = tooltip(
                list(
                  "Reported Analyte",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Chemical analyte(s) or substance(s) measured in the study."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("medium_auto"),
              label = tooltip(
                list(
                  "Sample Medium/Matrix",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Type of environmental medium sampled (e.g., water, sediment, biota)."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("study_area_auto"),
              label = tooltip(
                list(
                  "Study Area",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Geographic area or region where sampling was conducted."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("num_sites_auto"),
              label = tooltip(
                list(
                  "Number of Sites",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Total number of sampling locations in the study."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("num_samples_auto"),
              label = tooltip(
                list(
                  "Number of Samples",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Total number of samples collected and analyzed."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("sampling_period_auto"),
              label = tooltip(
                list(
                  "Sampling Period",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Time period when samples were collected (e.g., dates, seasons, years)."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("analytical_methods_auto"),
              label = tooltip(
                list(
                  "Analytical Method(s)",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Laboratory methods used for chemical analysis of samples."
              ),
              value = "",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("loq_auto"),
              label = tooltip(
                list(
                  "Limit of Quantification",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Lowest concentration of a stressor that can be reliably quantified by the analytical method."
              ),
              value = "",
              rows = 1,
              width = "100%"
            )
          ),
          div(
            textAreaInput(
              inputId = ns("site_types"),
              label = tooltip(
                list(
                  "Site Type(s)",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Description of sampling site characteristics (e.g., urban, rural, industrial, background)."
              ),
              placeholder = "Describe types of sampling sites",
              rows = 1,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("sampling_methods_auto"),
              label = tooltip(
                list(
                  "Sampling Method(s)",
                  bs_icon("arrow-down-circle-fill", class = "text-primary")
                ),
                "Field protocols and equipment used for sample collection."
              ),
              placeholder = "Describe sampling protocols used",
              rows = 1,
              width = "100%"
            ),

            ### User input fields ----

            h6("Additional Details (free text)"),

            textAreaInput(
              inputId = ns("sampling_conditions"),
              label = tooltip(
                list("Sampling Conditions", bs_icon("info-circle-fill")),
                "Describe environmental conditions during sampling (e.g., weather, season, flow conditions)."
              ),
              placeholder = "Describe sampling conditions, etc.",
              rows = 1,
              width = "100%"
            ),

            textInput(
              inputId = ns("site_density"),
              label = tooltip(
                list("Site Density", bs_icon("info-circle-fill")),
                "Spatial distribution of sampling sites (e.g., 1 site per 100 km², grid spacing)."
              ),
              placeholder = "e.g., 1 site per 100 km²",
              width = "100%"
            ),

            textInput(
              inputId = ns("sampling_frequency"),
              label = tooltip(
                list("Sampling Frequency", bs_icon("info-circle-fill")),
                "Temporal frequency of sample collection (e.g., monthly, quarterly, one-time)."
              ),
              placeholder = "e.g., monthly, quarterly",
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("other_details"),
              label = tooltip(
                list("Other Details", bs_icon("info-circle-fill")),
                "Any additional relevant information about the study design, methods, or data quality."
              ),
              placeholder = "Any additional relevant information",
              rows = 3,
              width = "100%"
            )
          )
        ),

        ## Action buttons ----
        input_task_button(
          id = ns("save_assessment"),
          label = "Save Assessment",
          icon = icon("save"),
          class = "btn-success",
          width = "200px",
          style = "margin-left: 10px;"
        ),

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

        # Gateway Criterion 1: Sampling Medium/Matrix
        div(
          style = "border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
          h6(
            "1. Sampling Medium/Matrix",
            style = "margin-bottom: 10px;"
          ),
          p(
            "Does the study specify which medium/matrix is sampled?",
            style = "margin-bottom: 8px;"
          ),
          checkboxInput(
            inputId = ns("gateway_medium_answer"),
            label = "Yes",
            value = FALSE
          ),
          div(
            style = "background-color: #f8f9fa; padding: 8px; margin: 8px 0; border-radius: 4px; font-size: 0.9em;",
            strong("Data found: "),
            textOutput(ns("gateway_medium_summary"), inline = TRUE)
          ),
        ),

        # Gateway Criterion 2: Analyte
        div(
          style = "border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
          h6("2. Analyte", style = "margin-bottom: 10px;"),
          p(
            "Does the study specify which unique analyte is measured?",
            style = "margin-bottom: 8px;"
          ),
          checkboxInput(
            inputId = ns("gateway_analyte_answer"),
            label = "Yes",
            value = FALSE
          ),
          div(
            style = "background-color: #f8f9fa; padding: 8px; margin: 8px 0; border-radius: 4px; font-size: 0.9em;",
            strong("Data found: "),
            textOutput(ns("gateway_analyte_summary"), inline = TRUE)
          )
        ),

        # Gateway Criterion 3: Spatial Location
        div(
          style = "border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
          h6(
            "3. Spatial Location",
            style = "margin-bottom: 10px;"
          ),
          p(
            "Does the study specify where samples were collected? At a minimum, there is enough information for the given purpose (e.g., country).",
            style = "margin-bottom: 8px;"
          ),
          checkboxInput(
            inputId = ns("gateway_location_answer"),
            label = "Yes",
            value = FALSE
          ),
          div(
            style = "background-color: #f8f9fa; padding: 8px; margin: 8px 0; border-radius: 4px; font-size: 0.9em;",
            strong("Data found: "),
            textOutput(ns("gateway_location_summary"), inline = TRUE)
          )
        ),

        # Gateway Criterion 4: Year
        div(
          style = "border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
          h6("4. Year", style = "margin-bottom: 10px;"),
          p(
            "Does the study indicate when samples were collected? At a minimum, there is enough information for the given purpose (e.g., sampling year).",
            style = "margin-bottom: 8px;"
          ),
          checkboxInput(
            inputId = ns("gateway_year_answer"),
            label = "Yes",
            value = FALSE
          ),
          div(
            style = "background-color: #f8f9fa; padding: 8px; margin: 8px 0; border-radius: 4px; font-size: 0.9em;",
            strong("Data found: "),
            textOutput(ns("gateway_year_summary"), inline = TRUE)
          )
        ),

        # Gateway Criterion 5: Units
        div(
          style = "border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
          h6("5. Units", style = "margin-bottom: 10px;"),
          p(
            "Does the study specify units of measurement?",
            style = "margin-bottom: 8px;"
          ),
          checkboxInput(
            inputId = ns("gateway_units_answer"),
            label = "Yes",
            value = FALSE
          ),
          div(
            style = "background-color: #f8f9fa; padding: 8px; margin: 8px 0; border-radius: 4px; font-size: 0.9em;",
            strong("Data found: "),
            textOutput(ns("gateway_units_summary"), inline = TRUE)
          )
        ),

        # Gateway Criterion 6: Data Source/Citation
        div(
          style = "border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
          h6(
            "6. Data Source/Citation",
            style = "margin-bottom: 10px;"
          ),
          p(
            "Does the study cite the source of data and/or is a suitable bibliographic reference available for the study?",
            style = "margin-bottom: 8px;"
          ),
          checkboxInput(
            inputId = ns("gateway_citation_answer"),
            label = "Yes",
            value = FALSE
          ),
          div(
            style = "background-color: #f8f9fa; padding: 8px; margin: 8px 0; border-radius: 4px; font-size: 0.9em;",
            strong("Data found: "),
            textOutput(ns("gateway_citation_summary"), inline = TRUE)
          )
        ),

        ## Reliability Criteria ---
        div(
          style = "margin: 20px 0;",
          h5("Reliability Criteria"),
          p(
            "Is the study reliable?",
            class = "text-muted"
          )
        ),
        mod_CREED_reliability_ui("CREED_reliability"),

        ## Relevance Criteria ---
        div(
          style = "margin: 20px 0;",
          h5("Relevance Criteria"),
          p(
            "Is the study relevant to your goals? (again, probably yes)",
            class = "text-muted"
          )
        ),

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

    mod_CREED_reliability_server("CREED_reliability")

    # 2. Helper functions ----

    ## Auto-populate dataset details ----
    auto_populate_details <- function() {
      # Get dataset summaries
      summaries <- get_dataset_summaries(moduleData())

      # Update UI fields
      updateTextAreaInput(session, "source_auto", value = summaries$source)
      updateTextAreaInput(session, "analyte_auto", value = summaries$analytes)
      updateTextAreaInput(session, "medium_auto", value = summaries$medium)
      updateTextAreaInput(
        session,
        "study_area_auto",
        value = summaries$study_area
      )
      updateTextAreaInput(
        session,
        "num_sites_auto",
        value = summaries$num_sites
      )
      updateTextAreaInput(
        session,
        "site_types_auto",
        value = summaries$site_types
      )
      updateTextAreaInput(
        session,
        "num_samples_auto",
        value = summaries$num_samples
      )
      updateTextAreaInput(
        session,
        "sampling_period_auto",
        value = summaries$sampling_period
      )
      updateTextAreaInput(
        session,
        "analytical_methods_auto",
        value = summaries$analytical_methods
      )
      updateTextAreaInput(
        session,
        "sampling_methods_auto",
        value = summaries$sampling_methods
      )
      updateTextAreaInput(session, "loq_auto", value = summaries$loq_info)

      # Auto-populate gateway criteria
      auto_populate_gateway_criteria()

      print_dev("CREED dataset details auto-populated")
    }

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
