#' CREED_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList textAreaInput
#' @importFrom bslib input_task_button tooltip

mod_CREED_details_ui <- function(id) {
  ns <- NS(id)

  tagList(
    input_task_button(
      id = ns("populate_from_data"),
      label = "Populate section from data",
      icon = bs_icon("arrow-down-circle")
    ),
    br(),

    layout_column_wrap(
      width = "800px",
      fill = FALSE,
      fillable = FALSE,

      ### Auto-populated fields ----
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
      ),

      textAreaInput(
        inputId = ns("site_types"),
        label = tooltip(
          list(
            "Site Type(s)",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Description of sampling site characteristics (e.g., urban, rural, industrial, background)."
        ),
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
        rows = 1,
        width = "100%"
      ),

      ### User input fields ----

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
    ),

    ## Action buttons ----
    # input_task_button(
    #   id = ns("save_assessment"),
    #   label = "Save Section",
    #   icon = icon("save"),
    #   class = "btn-success"
    # ) |>
    #   shinyjs::disabled() # TODO: Necessary?
  )
}


#' CREED_details Server Functions
#' @import shiny
#' @importFrom golem print_dev
#' @importFrom shiny updateTextAreaInput bindEvent isTruthy
#' @importFrom tibble tibble
#' @export
mod_CREED_details_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe ~bindEvent(populate_from_data): Auto-populate fields ----
    # upstream: user clicks populate_from_data
    # downstream: all auto-populated textAreaInput fields
    observe({
      session$userData$reactiveValues$creedData$datasetDetails <- summarise_CREED_details(
        session$userData$reactiveValues
      )
      # print_dev(
      #   "writing dataset details to session data, auto-populating relevant inputs)"
      # )
      # print_dev(dput(session$userData$reactiveValues$creedData$datasetDetails))

      # Update UI fields
      # Avoid writing out the whole layered call every time
      dataset_details <- session$userData$reactiveValues$creedData$datasetDetails
      # convert to list for easier access
      dataset_details <- setNames(dataset_details$value, dataset_details$field)

      updateTextAreaInput(
        session,
        "source_auto",
        value = dataset_details[["source"]]
      )
      updateTextAreaInput(
        session,
        "analyte_auto",
        value = dataset_details[["analytes"]]
      )
      updateTextAreaInput(
        session,
        "medium_auto",
        value = dataset_details[["medium"]]
      )
      updateTextAreaInput(
        session,
        "study_area_auto",
        value = dataset_details[["study_area"]]
      )
      updateTextAreaInput(
        session,
        "num_sites_auto",
        value = dataset_details[["num_sites"]]
      )
      updateTextAreaInput(
        session,
        "site_types",
        value = dataset_details[["site_types"]]
      )
      updateTextAreaInput(
        session,
        "num_samples_auto",
        value = dataset_details[["num_samples"]]
      )
      updateTextAreaInput(
        session,
        "sampling_period_auto",
        value = dataset_details[["sampling_period"]]
      )
      updateTextAreaInput(
        session,
        "analytical_methods_auto",
        value = dataset_details[["analytical_methods"]]
      )
      updateTextAreaInput(
        session,
        "sampling_methods_auto",
        value = dataset_details[["sampling_methods"]]
      )
      updateTextAreaInput(
        session,
        "loq_auto",
        value = dataset_details[["loq_info"]]
      )

      print_dev("CREED Dataset Details auto-populated")
    }) |>
      bindEvent(
        input$populate_from_data,
        ignoreInit = TRUE
      )

    #' summarise session user data into pretty strings for CREED "Dataset Details" reporting
    #'
    #' @description summarise session user data into pretty strings for CREED "Dataset Details" reporting
    #' @param sessionData session$userData$reactiveValues object
    #' @return tibble of Dataset Details fields ("field") and pretty strings ("value")
    #' @export
    summarise_CREED_details <- function(sessionData) {
      # Helper function to check if a dataset exists and has content
      dataset_exists <- function(dataset) {
        isTruthy(dataset) && !all(is.na(dataset))
      }

      # Check existence of each dataset once
      has_reference <- dataset_exists(sessionData$referenceData)
      has_parameters <- dataset_exists(sessionData$parametersData)
      has_compartments <- dataset_exists(sessionData$compartmentsData)
      has_sites <- dataset_exists(sessionData$sitesData)
      has_samples <- dataset_exists(sessionData$samplesData)
      has_methods <- dataset_exists(sessionData$methodsData)
      has_measurements <- dataset_exists(sessionData$measurementsData)

      # Extract values into intermediate variables
      source_value <- if (has_reference) {
        create_bibliography_reference(sessionData$referenceData)
      } else {
        "Relevant data not found"
      }

      analytes_value <- if (has_parameters) {
        summarize_multiple(
          sessionData$parametersData$PARAMETER_NAME,
          "Parameters"
        )
      } else {
        "Relevant data not found"
      }

      medium_value <- if (has_compartments) {
        summarize_multiple(
          sessionData$compartmentsData$ENVIRON_COMPARTMENT,
          "Compartments"
        )
      } else {
        "Relevant data not found"
      }

      study_area_value <- if (has_sites) {
        countries <- summarize_multiple(
          sessionData$sitesData$COUNTRY,
          "Countries"
        )
        areas <- summarize_multiple(sessionData$sitesData$AREA, "Areas")
        paste(countries, areas, sep = "; ")
      } else {
        "Relevant data not found"
      }

      num_sites_value <- if (has_sites) {
        as.character(nrow(sessionData$sitesData))
      } else {
        "Relevant data not found"
      }

      site_types_value <- if (has_sites) {
        summarize_multiple(
          sessionData$sitesData$SITE_GEOGRAPHICAL_FEATURE,
          "Site Types"
        )
      } else {
        "Relevant data not found"
      }

      num_samples_value <- if (has_samples) {
        as.character(nrow(sessionData$samplesData))
      } else {
        "Relevant data not found"
      }

      sampling_period_value <- if (has_samples) {
        calculate_date_range(sessionData$samplesData$SAMPLING_DATE)
      } else {
        "Relevant data not found"
      }

      sampling_methods_value <- if (has_methods) {
        sampling_only <- sessionData$methodsData[
          sessionData$methodsData$PROTOCOL_CATEGORY == "Sampling Protocol",
        ]
        if (nrow(sampling_only) > 0) {
          summarize_multiple(sampling_only$PROTOCOL_NAME, "Sampling Protocols")
        } else {
          "Relevant data not found"
        }
      } else {
        "Relevant data not found"
      }

      analytical_methods_value <- if (has_methods) {
        analytical_only <- sessionData$methodsData[
          sessionData$methodsData$PROTOCOL_CATEGORY == "Analytical Protocol",
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

      loq_info_value <- if (has_measurements) {
        loq_values <- sessionData$measurementsData$LOQ_VALUE[
          !is.na(sessionData$measurementsData$LOQ_VALUE)
        ]
        lod_values <- sessionData$measurementsData$LOD_VALUE[
          !is.na(sessionData$measurementsData$LOD_VALUE)
        ]

        info_parts <- c()
        if (length(loq_values) > 0) {
          loq_range <- paste(min(loq_values), "to", max(loq_values))
          loq_unit <- sessionData$measurementsData$LOQ_UNIT[
            !is.na(sessionData$measurementsData$LOQ_UNIT)
          ][1]
          info_parts <- c(info_parts, paste("LOQ:", loq_range, loq_unit))
        }
        if (length(lod_values) > 0) {
          lod_range <- paste(min(lod_values), "to", max(lod_values))
          lod_unit <- sessionData$measurementsData$LOD_UNIT[
            !is.na(sessionData$measurementsData$LOD_UNIT)
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

      # Build tibble from extracted values
      tibble(
        field = c(
          "source",
          "analytes",
          "medium",
          "study_area",
          "num_sites",
          "site_types",
          "num_samples",
          "sampling_period",
          "sampling_methods",
          "analytical_methods",
          "loq_info"
        ),
        value = c(
          source_value,
          analytes_value,
          medium_value,
          study_area_value,
          num_sites_value,
          site_types_value,
          num_samples_value,
          sampling_period_value,
          sampling_methods_value,
          analytical_methods_value,
          loq_info_value
        )
      )
    }
  })
}

## To be copied in the UI
# mod_CREED_details_ui("CREED_details_1")

## To be copied in the server
# mod_CREED_details_server("CREED_details_1")
