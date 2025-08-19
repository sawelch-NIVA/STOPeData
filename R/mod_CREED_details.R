#' CREED_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_CREED_details_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
    )
  )
}

#' CREED_details Server Functions
#'
#' @noRd
mod_CREED_details_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
  })
}

## To be copied in the UI
# mod_CREED_details_ui("CREED_details_1")

## To be copied in the server
# mod_CREED_details_server("CREED_details_1")
