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
#' @importFrom shiny NS tagList textInput textAreaInput actionButton
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel
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
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "CREED Assessment Information",
            icon = bs_icon("info-circle"),
            "This module provides a Criteria for Reporting and Evaluating Exposure Datasets (CREED) assessment (https://academic.oup.com/ieam/article/20/4/1019/7821760). The Dataset Details section summarizes key characteristics of your dataset that are relevant for quality evaluation. Fields are auto-populated where possible from your imported data."
          )
        ),

        ## Dataset Details section ----
        div(
          style = "margin: 20px 0;",
          h5("Dataset Details - Key Attributes"),
          p(
            "Review the auto-populated fields below and add any missing information.",
            class = "text-muted"
          )
        ),

        ## Two-column layout for dataset details ----
        layout_column_wrap(
          width = "400px",
          fill = FALSE,
          fillable = FALSE,

          ### Auto-populated fields (left column) ----
          div(
            h6("Auto-populated from Dataset", style = "color: #0066cc;"),

            textInput(
              inputId = ns("source_auto"),
              label = "Source (reference):",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("analyte_auto"),
              label = "Reported Analyte:",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("medium_auto"),
              label = "Sample Medium/Matrix:",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("study_area_auto"),
              label = "Study Area:",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("num_sites_auto"),
              label = "Number of Sites:",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("num_samples_auto"),
              label = "Number of Samples:",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("sampling_period_auto"),
              label = "Sampling Period:",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("analytical_methods_auto"),
              label = "Analytical Method(s):",
              value = "",
              width = "100%"
            ),

            textInput(
              inputId = ns("loq_auto"),
              label = "Limit of Quantification:",
              value = "",
              width = "100%"
            )
          ),

          ### User input fields (right column) ----
          div(
            h6("Additional Details", style = "color: #0066cc;"),

            textAreaInput(
              inputId = ns("sampling_conditions"),
              label = "Sampling Conditions:",
              placeholder = "Describe sampling conditions, protocols, etc.",
              rows = 2,
              width = "100%"
            ),

            textInput(
              inputId = ns("site_density"),
              label = "Site Density:",
              placeholder = "e.g., 1 site per 100 km²",
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("site_types"),
              label = "Site Type(s):",
              placeholder = "Describe types of sampling sites",
              rows = 2,
              width = "100%"
            ),

            textInput(
              inputId = ns("sampling_frequency"),
              label = "Sampling Frequency:",
              placeholder = "e.g., monthly, quarterly",
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("sampling_methods"),
              label = "Sampling Method(s):",
              placeholder = "Describe sampling protocols used",
              rows = 2,
              width = "100%"
            ),

            textAreaInput(
              inputId = ns("other_details"),
              label = "Other Details:",
              placeholder = "Any additional relevant information",
              rows = 3,
              width = "100%"
            )
          )
        ),

        ## Action buttons ----
        div(
          style = "margin: 20px 0;",
          actionButton(
            inputId = ns("update_details"),
            label = "Update Dataset Details",
            icon = icon("refresh"),
            class = "btn-primary",
            width = "200px"
          ),
          actionButton(
            inputId = ns("save_assessment"),
            label = "Save Assessment",
            icon = icon("save"),
            class = "btn-success",
            width = "200px",
            style = "margin-left: 10px;"
          )
        ),

        ## Status display ----
        div(
          style = "margin-top: 15px;",
          uiOutput(ns("status_reporter"))
        ),

        ## Future CREED sections placeholder ----
        accordion(
          id = ns("future_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Full CREED Assessment (Coming Soon)",
            icon = bs_icon("gear"),
            "Future versions will include Gateway Criteria, Reliability Assessment, and Relevance Assessment tabs for complete CREED evaluation."
          )
        )
      )
    )
  )
}

#' CREED Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI showNotification updateTextInput updateTextAreaInput bindEvent
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

    # 2. Helper functions ----

    ## Create bibliographic reference from reference data ----
    create_bibliography_reference <- function(ref_data) {
      if (is.null(ref_data) || nrow(ref_data) == 0) {
        return("Relevant data not found")
      }

      # Basic format: Author (Year). Title. Journal/Source.
      ref_parts <- c()

      if (!is.na(ref_data$AUTHOR) && ref_data$AUTHOR != "") {
        ref_parts <- c(ref_parts, ref_data$AUTHOR)
      }

      if (!is.na(ref_data$YEAR)) {
        ref_parts <- c(ref_parts, paste0("(", ref_data$YEAR, ")"))
      }

      if (!is.na(ref_data$TITLE) && ref_data$TITLE != "") {
        ref_parts <- c(ref_parts, ref_data$TITLE)
      }

      if (
        !is.na(ref_data$PERIODICAL_JOURNAL) && ref_data$PERIODICAL_JOURNAL != ""
      ) {
        ref_parts <- c(ref_parts, ref_data$PERIODICAL_JOURNAL)
      } else if (!is.na(ref_data$PUBLISHER) && ref_data$PUBLISHER != "") {
        ref_parts <- c(ref_parts, ref_data$PUBLISHER)
      }

      if (length(ref_parts) > 0) {
        return(paste(ref_parts, collapse = ". "))
      } else {
        return("Reference data incomplete")
      }
    }

    ## Summarize multiple values ----
    summarize_multiple <- function(values, prefix = "", max_display = 3) {
      if (is.null(values) || length(values) == 0) {
        return("Relevant data not found")
      }

      unique_values <- unique(values[!is.na(values) & values != ""])

      if (length(unique_values) == 0) {
        return("Relevant data not found")
      }

      if (length(unique_values) <= max_display) {
        result <- paste(unique_values, collapse = ", ")
      } else {
        displayed <- paste(unique_values[1:max_display], collapse = ", ")
        result <- paste0(
          displayed,
          " (and ",
          length(unique_values) - max_display,
          " more)"
        )
      }

      if (prefix != "" && length(unique_values) > 1) {
        result <- paste0(prefix, " (", length(unique_values), "): ", result)
      } else if (prefix != "") {
        result <- paste0(prefix, ": ", result)
      }

      return(result)
    }

    ## Calculate date range ----
    calculate_date_range <- function(dates) {
      if (is.null(dates) || length(dates) == 0) {
        return("Relevant data not found")
      }

      valid_dates <- dates[!is.na(dates)]
      if (length(valid_dates) == 0) {
        return("Relevant data not found")
      }

      min_date <- min(valid_dates)
      max_date <- max(valid_dates)

      if (min_date == max_date) {
        return(as.character(min_date))
      } else {
        return(paste(min_date, "to", max_date))
      }
    }

    ## Auto-populate dataset details ----
    auto_populate_details <- function() {
      # Get data from session
      campaign_data <- session$userData$reactiveValues$campaignData
      reference_data <- session$userData$reactiveValues$referencesData
      sites_data <- session$userData$reactiveValues$sitesData
      parameters_data <- session$userData$reactiveValues$parametersData
      samples_data <- session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
        session$userData$reactiveValues$sampleData
      methods_data <- session$userData$reactiveValues$methodsData
      measurement_data <- session$userData$reactiveValues$dataData

      # Source reference
      source_ref <- if (!is.null(reference_data)) {
        create_bibliography_reference(reference_data)
      } else {
        "Relevant data not found"
      }

      # Reported analytes
      analytes <- if (!is.null(parameters_data)) {
        summarize_multiple(parameters_data$PARAMETER_NAME, "Parameters")
      } else {
        "Relevant data not found"
      }

      # Sample medium/matrix
      medium <- if (!is.null(samples_data)) {
        summarize_multiple(samples_data$ENVIRON_COMPARTMENT, "Compartments")
      } else {
        "Relevant data not found"
      }

      # Study area (countries and areas)
      study_area <- if (!is.null(sites_data)) {
        countries <- summarize_multiple(sites_data$COUNTRY, "Countries")
        areas <- summarize_multiple(sites_data$AREA, "Areas")
        paste(countries, areas, sep = "; ")
      } else {
        "Relevant data not found"
      }

      # Number of sites
      num_sites <- if (!is.null(sites_data)) {
        as.character(nrow(sites_data))
      } else {
        "Relevant data not found"
      }

      # Number of samples
      num_samples <- if (!is.null(samples_data)) {
        as.character(nrow(samples_data))
      } else {
        "Relevant data not found"
      }

      # Sampling period
      sampling_period <- if (!is.null(samples_data)) {
        calculate_date_range(samples_data$SAMPLING_DATE)
      } else {
        "Relevant data not found"
      }

      # Analytical methods
      analytical_methods <- if (!is.null(methods_data)) {
        analytical_only <- methods_data[
          methods_data$PROTOCOL_CATEGORY == "Analytical Protocol",
        ]
        if (nrow(analytical_only) > 0) {
          summarize_multiple(
            analytical_only$PROTOCOL_NAME,
            "Analytical Protocols"
          )
        } else {
          "Relevant data not found"
        }
      } else {
        "Relevant data not found"
      }

      # LOQ/LOD
      loq_info <- if (!is.null(measurement_data)) {
        loq_values <- measurement_data$LOQ_VALUE[
          !is.na(measurement_data$LOQ_VALUE)
        ]
        lod_values <- measurement_data$LOD_VALUE[
          !is.na(measurement_data$LOD_VALUE)
        ]

        info_parts <- c()
        if (length(loq_values) > 0) {
          loq_range <- paste(min(loq_values), "to", max(loq_values))
          loq_unit <- measurement_data$LOQ_UNIT[
            !is.na(measurement_data$LOQ_UNIT)
          ][1]
          info_parts <- c(info_parts, paste("LOQ:", loq_range, loq_unit))
        }
        if (length(lod_values) > 0) {
          lod_range <- paste(min(lod_values), "to", max(lod_values))
          lod_unit <- measurement_data$LOD_UNIT[
            !is.na(measurement_data$LOD_UNIT)
          ][1]
          info_parts <- c(info_parts, paste("LOD:", lod_range, lod_unit))
        }

        if (length(info_parts) > 0) {
          paste(info_parts, collapse = "; ")
        } else {
          "Relevant data not found"
        }
      } else {
        "Relevant data not found"
      }

      # Update UI fields
      updateTextInput(session, "source_auto", value = source_ref)
      updateTextInput(session, "analyte_auto", value = analytes)
      updateTextInput(session, "medium_auto", value = medium)
      updateTextInput(session, "study_area_auto", value = study_area)
      updateTextInput(session, "num_sites_auto", value = num_sites)
      updateTextInput(session, "num_samples_auto", value = num_samples)
      updateTextInput(session, "sampling_period_auto", value = sampling_period)
      updateTextInput(
        session,
        "analytical_methods_auto",
        value = analytical_methods
      )
      updateTextInput(session, "loq_auto", value = loq_info)

      print_dev("CREED dataset details auto-populated")
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
  })
}

## To be copied in the UI ----
# mod_creed_ui("creed_1")

## To be copied in the server ----
# mod_creed_server("creed_1")
