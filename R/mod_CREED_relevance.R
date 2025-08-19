# CREED Relevance Assessment Module ----
# A Shiny module for CREED relevance criteria evaluation

#' CREED_relevance UI Function
#'
#' @description A shiny Module for evaluating CREED relevance criteria.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput textAreaInput actionButton checkboxInput renderText selectInput h4 p strong em div
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel input_task_button
#' @importFrom bsicons bs_icon
mod_CREED_relevance_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## Media criteria ----

    ### RV01: Sample Medium/Matrix ----
    create_criterion_section(
      ns,
      criterion_id = "RV01",
      title = "Sample Medium/Matrix",
      type = "required",
      description = "Was the sampling medium/matrix appropriate for the given purpose?"
    ),

    ### RV02: Collection Method/Sample Type ----
    create_criterion_section(
      ns,
      criterion_id = "RV02",
      title = "Collection Method/Sample Type",
      type = "recommended",
      description = "Was the sample collection method (e.g., grab, depth- and width-integrated, discrete, composite, or time-integrated samples, or continuous monitoring) adequate for the given purpose?"
    ),

    ## Spatial criteria ----

    ### RV03: Study Area ----
    create_criterion_section(
      ns,
      criterion_id = "RV03",
      title = "Study Area",
      type = "required",
      description = "Were the study area and number of locations sampled suitable for the given purpose?"
    ),

    ### RV04: Site Type ----
    create_criterion_section(
      ns,
      criterion_id = "RV04",
      title = "Site Type",
      type = "recommended",
      description = "Was the rationale for selection of sampling locations provided and was it suitable for the given purpose?"
    ),

    ## Temporal criteria ----

    ### RV05: Sampling Timespan ----
    create_criterion_section(
      ns,
      criterion_id = "RV05",
      title = "Sampling Timespan",
      type = "required",
      description = "Were the samples collected over a time scale that was appropriate for the given purpose?"
    ),

    ### RV06: Sampling Frequency ----
    create_criterion_section(
      ns,
      criterion_id = "RV06",
      title = "Sampling Frequency",
      type = "required",
      description = "Over the timespan, was the sampling frequency appropriate for the given purpose?"
    ),

    ### RV07: Temporal Conditions ----
    create_criterion_section(
      ns,
      criterion_id = "RV07",
      title = "Temporal Conditions",
      type = "recommended",
      description = "Were conditions during sampling events documented and relevant for the given purpose (e.g., baseflow, storm events, planned/unplanned discharges, etc.)?"
    ),

    ## Analytical criteria ----

    ### RV08: Analyte ----
    create_criterion_section(
      ns,
      criterion_id = "RV08",
      title = "Analyte",
      type = "required",
      description = "Was/were the reported analyte(s) appropriate for the given purpose?"
    ),

    ### RV09: Sensitivity/LOD/LOQ ----
    create_criterion_section(
      ns,
      criterion_id = "RV09",
      title = "Sensitivity/LOD/LOQ",
      type = "required",
      description = "Was the method sensitive enough for the given purpose (i.e., were the LOD and/or LOQ below the benchmarks or metrics to which concentrations in the dataset will be compared)?"
    ),

    ## Data Handling and Statistics criteria ----
    h5(
      "Data Handling and Statistics",
      class = "text-primary",
      style = "margin-top: 20px; margin-bottom: 15px;"
    ),

    ### RV10: Summary Statistics Type ----
    create_criterion_section(
      ns,
      criterion_id = "RV10",
      title = "Summary Statistics Type",
      type = "recommended",
      description = "If dataset contains summary statistics: Were the summary statistics provided (e.g., median, geometric mean, arithmetic mean, percentiles) appropriate for the given purpose?"
    ),

    ## Supporting Parameters criteria ----

    ### RV11: Supporting Parameters ----
    create_criterion_section(
      ns,
      criterion_id = "RV11",
      title = "Supporting Parameters",
      type = "required",
      description = "If supporting parameters are required for the purpose: Were
      all supporting parameters provided that were needed to achieve the given purpose?"
    ),

    ## Action buttons and status ----
    input_task_button(
      id = ns("calc_scores"),
      label = "Calculate Relevance Score"
    ),

    div(
      style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
      h6("Completion Status"),
      uiOutput(ns("completion_status"))
    )
  )
}

#' CREED_relevance Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer
mod_CREED_relevance_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Server functionality to be implemented ----
    # - Auto-population of relevant data fields
    # - Score calculation and validation
    # - Data storage in session$userData
    # - Completion status tracking
  })
}

## To be copied in the UI ----
# mod_CREED_relevance_ui("CREED_relevance_1")

## To be copied in the server ----
# mod_CREED_relevance_server("CREED_relevance_1")
