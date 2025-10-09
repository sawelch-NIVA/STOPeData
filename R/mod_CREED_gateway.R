#' CREED_gateway UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_CREED_gateway_ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(
      id = ns("populate_from_data"),
      label = "Populate section from data",
      icon = bs_icon("arrow-down-circle")
    ),
    # Gateway Criterion 1: Sampling Medium/Matrix
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "flex-grow: 1; margin-right: 15px;",
        strong("1. Does the study specify which medium/matrix is sampled?"),
        div(
          style = "background-color: #f8f9fa; padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_medium_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_medium_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 2: Analyte
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "flex-grow: 1; margin-right: 15px;",
        strong("2. Does the study specify which unique analyte is measured?"),
        div(
          style = "background-color: #f8f9fa; padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_analyte_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_analyte_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 3: Spatial Location
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "flex-grow: 1; margin-right: 15px;",
        strong(
          "3. Does the study specify where samples were collected? At a minimum, there is enough information for the given purpose (e.g., country)."
        ),
        div(
          style = "background-color: #f8f9fa; padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_location_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_location_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 4: Year
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "flex-grow: 1; margin-right: 15px;",
        strong(
          "4. Does the study indicate when samples were collected? At a minimum, there is enough information for the given purpose (e.g., sampling year)."
        ),
        div(
          style = "background-color: #f8f9fa; padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_year_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_year_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 5: Units
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "flex-grow: 1; margin-right: 15px;",
        strong("5. Does the study specify units of measurement?"),
        div(
          style = "background-color: #f8f9fa; padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_units_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_units_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 6: Data Source/Citation
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "flex-grow: 1; margin-right: 15px;",
        strong(
          "6. Does the study cite the source of data and/or is a suitable bibliographic reference available for the study?"
        ),
        div(
          style = "background-color: #f8f9fa; padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_citation_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_citation_answer"),
        label = "Yes",
        value = FALSE
      )
    ),
    input_task_button(
      id = ns("save_assessment"),
      label = "Save Section",
      icon = icon("save"),
      class = "btn-success"
    )
  )
}

#' CREED_gateway Server Functions
#'
#' @noRd
mod_CREED_gateway_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Auto-populate gateway criteria ----
    auto_populate_gateway_criteria <- function() {
      # Build module_data list from session userData
      module_data <- list(
        campaign = session$userData$reactiveValues$campaignData,
        references = session$userData$reactiveValues$referenceData,
        sites = session$userData$reactiveValues$sitesData,
        parameters = session$userData$reactiveValues$parametersData,
        compartments = session$userData$reactiveValues$compartmentsData,
        samples = session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
          session$userData$reactiveValues$samplesData,
        methods = session$userData$reactiveValues$methodsData,
        measurements = session$userData$reactiveValues$dataData
      )

      # Get gateway availability
      availability <- check_gateway_availability(module_data)

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
  })
}

## To be copied in the UI
# mod_CREED_gateway_ui("CREED_gateway_1")

## To be copied in the server
# mod_CREED_gateway_server("CREED_gateway_1")
