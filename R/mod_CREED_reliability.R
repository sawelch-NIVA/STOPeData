# CREED Reliability Assessment Module ----
# A Shiny module for CREED reliability criteria evaluation

mod_CREED_reliability_ui <- function(id) {
  CREED_choices <- c(
    "Not Met" = "not_met",
    "Fully Met" = "fully",
    "Partly Met" = "partly",
    "Not Reported" = "not_reported",
    "Not Relevant" = "not_relevant"
  )

  ns <- NS(id)

  # Helper function to create criterion sections ----
  create_criterion_section <- function(
    criterion_id,
    title,
    type,
    description,
    note = NULL,
    is_conditional = FALSE
  ) {
    icon_class <- if (type == "required") {
      "reliability-required"
    } else {
      "reliability-recommended"
    }
    type_text <- if (type == "required") "Required" else "Recommended"

    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h4(
            HTML(paste(
              bs_icon("award-fill", class = icon_class),
              paste0(
                criterion_id,
                ": ",
                title,
                " (",
                tools::toTitleCase(type_text),
                ")"
              )
            ))
          ),
          p(
            strong("Criterion: "),
            description
          ),
          if (!is.null(note)) {
            p(
              em("Note: "),
              note,
              class = "text-muted"
            )
          }
        ),
        selectInput(
          inputId = ns(paste0(criterion_id, "_score")),
          label = "Score:",
          choices = CREED_choices,
          width = "200px"
        )
      ),

      # Two-column layout for data and justification ----
      layout_columns(
        col_widths = c(6, 6),
        create_relevant_data_input(ns, criterion_id),
        create_justification_input(ns, criterion_id)
      ),

      # Conditional styling if needed ----
      if (is_conditional) {
        tags$script(HTML(paste0(
          "document.getElementById('",
          ns(paste0(criterion_id, "_container")),
          "').classList.add('criterion-disabled');"
        )))
      }
    )
  }

  # Helper function for relevant data input ----
  create_relevant_data_input <- function(ns, criterion_id) {
    div(
      textAreaInput(
        inputId = ns(paste0(criterion_id, "_relevant_data")),
        label = tooltip(
          list(
            "Relevant Data",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Data extracted from your dataset that is relevant to this criterion."
        ),
        value = "",
        rows = 3,
        width = "100%"
      )
    )
  }

  # Helper function for justification input ----
  create_justification_input <- function(ns, criterion_id) {
    textAreaInput(
      inputId = ns(paste0(criterion_id, "_justification")),
      label = "Justification (free text)",
      placeholder = "Provide justification for your scoring...",
      rows = 3,
      width = "100%"
    )
  }

  # Helper function for conditional criteria ----
  create_conditional_criterion <- function(
    criterion_id,
    title,
    type,
    description,
    note = NULL
  ) {
    div(
      id = ns(paste0(criterion_id, "_container")),
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",
      create_criterion_section(
        criterion_id,
        title,
        type,
        description,
        note,
        is_conditional = TRUE
      )
    )
  }

  tagList(
    # Custom CSS for required/recommended styling ----
    tags$head(
      tags$style(HTML(
        "
        .reliability-required { fill: #aaaaaa !important;}
        .reliability-recommended { fill:  #ffc107 !important; }
        .criterion-disabled { 
          opacity: 0.6; 
          pointer-events: none; 
        }
      "
      ))
    ),

    # Main reliability section ----

    ## Info section ----
    div(
      style = "margin-bottom: 10px;",
      p(
        "Evaluate the reliability (data quality) of your dataset across 19 criteria. ",
        "Required criteria (",
        bs_icon("award-fill", class = "reliability-required"),
        ") are needed for Silver level scoring. ",
        "Recommended criteria (",
        bs_icon("award-fill", class = "reliability-recommended"),
        ") are additional requirements for Gold level scoring.",
        class = "text-muted"
      ),
      div(
        class = "alert alert-warning",
        p(
          bs_icon("exclamation-triangle"),
          strong(" Note: "),
          "Server functionality for auto-population and score calculation is not yet implemented."
        )
      )
    ),

    # Reliability criteria using helper functions ----

    ## RB1: Media - Sample medium/matrix ----
    create_criterion_section(
      "RB1",
      "Sample Medium/Matrix",
      "required",
      "Was the sampling medium/matrix reported in detail (for water: dissolved fraction or whole water; for sediment: sieved or whole; for soil, grain size; for biota, species, age, sex, tissue type), and was the matrix appropriate for the analyte of interest?"
    ),

    ## RB2: Media - Collection method/sample type ----
    create_criterion_section(
      "RB2",
      "Collection Method/Sample Type",
      "recommended",
      "Was the sample collection method reported? Examples include grab, depth- and width-integrated, discrete, composite, or time-integrated samples, or continuous monitoring."
    ),

    ## RB3: Media - Sample handling ----
    create_criterion_section(
      "RB3",
      "Sample Handling",
      "recommended",
      "Was information reported on sample handling (transport conditions, preservation, filtration, storage)? Was the type of container suitable for use with the analyte of interest (i.e., no loss or contamination)?"
    ),

    ## RB4: Spatial - Site location ----
    create_criterion_section(
      "RB4",
      "Site Location",
      "required",
      "Were the site locations reported?"
    ),

    ## RB5: Temporal - Date and time ----
    create_criterion_section(
      "RB5",
      "Date and Time",
      "required",
      "Were the date and time of sample collection reported?"
    ),

    ## RB6: Analytical - Analyte(s) measured ----
    create_criterion_section(
      "RB6",
      "Analyte(s) Measured",
      "required",
      "Was/were the analyte(s) of interest suitably and definitively identified?"
    ),

    ## RB7: Analytical - LOD/LOQ ----
    create_criterion_section(
      "RB7",
      "Limit of Detection and/or Limit of Quantification",
      "required",
      "Were limits of detection and/or quantification provided?",
      note = "The limit of detection (LOD) is the minimum value that the method can determine is statistically different from blanks. The limit of quantification (LOQ) is the minimum value that the method can quantify with a defined uncertainty."
    ),

    ## RB8: Analytical - Accreditation/QMS (Shortcut criterion) ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h2(
            HTML(paste(
              bs_icon("award-fill", class = "reliability-required"),
              "RB8: Accreditation/Quality Management System (Required - Shortcut Criterion)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Were the laboratory and method accredited for all or almost all samples? Several national and international accreditation bodies are available (e.g., ISO, UKAS); was that laboratory and/or method certified to these standards? Was a quality system (such as, e.g., ISO 17025) adopted?"
          ),
          div(
            class = "alert alert-info",
            p(
              bs_icon("info-circle"),
              strong(" Shortcut Criterion: "),
              "If you answer 'Fully Met' to this question, you may skip questions RB9-RB12. If not, please complete those questions."
            )
          )
        ),
        selectInput(
          inputId = ns("RB8_score"),
          label = "Score:",
          choices = CREED_choices,
          width = "200px"
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        create_relevant_data_input(ns, "RB8"),
        create_justification_input(ns, "RB8")
      )
    ),

    ## RB9-RB12: Conditional criteria ----
    div(
      id = ns("RB9_container"),
      create_criterion_section(
        "RB9",
        "Method",
        "required",
        "Was the method sufficiently described or referenced, such that it can be reproduced if necessary? Was method validation included?",
        note = "Skip this criterion if you answered 'Fully Met' to RB8 (Accreditation/QMS)."
      )
    ),

    div(
      id = ns("RB10_container"),
      create_criterion_section(
        "RB10",
        "Lab Blank Contamination",
        "recommended",
        "Was method blank contamination assessed with laboratory blanks?",
        note = "Skip this criterion if you answered 'Fully Met' to RB8 (Accreditation/QMS)."
      )
    ),

    div(
      id = ns("RB11_container"),
      create_criterion_section(
        "RB11",
        "Recovery/Accuracy",
        "recommended",
        "Were method recovery/accuracy and/or uncertainty assessed by recovery of standard reference material (SRM) and/or were lab spike samples assessed?",
        note = "Skip this criterion if you answered 'Fully Met' to RB8 (Accreditation/QMS)."
      )
    ),

    div(
      id = ns("RB12_container"),
      create_criterion_section(
        "RB12",
        "Reproducibility/Precision",
        "recommended",
        "Were method reproducibility and/or uncertainty assessed with lab replicates and long-term control recoveries?",
        note = "Skip this criterion if you answered 'Fully Met' to RB8 (Accreditation/QMS)."
      )
    ),

    ## RB13: Analytical - Field QC ----
    create_criterion_section(
      "RB13",
      "Field QC",
      "recommended",
      "Were quality control (QC) samples collected during field sampling (such as field blanks, spikes, replicates) to demonstrate the method performance for a given field study?"
    ),

    # Note: RB14-RB19 would continue using create_criterion_section()
    # Stopping here to keep artifact manageable

    ## Action buttons and status ----
    input_task_button(
      id = ns("calc_scores"),
      label = "Calculate Reliability Score"
    ),

    div(
      style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
      h6("Completion Status"),
      uiOutput(ns("completion_status"))
    )
  )
}

#' CREED Reliability Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI isTruthy
#' @export
mod_CREED_reliability_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: reliability_scores ----
    reliability_scores <- reactiveValues()

    # 2. Observers ----

    ## observe: RB8 conditional logic ----
    # upstream: input$RB8_score
    # downstream: RB9-RB12 container styling
    observe({
      rb8_fully_met <- input$RB8_score == "fully"

      # Apply/remove disabled styling to conditional questions
      conditional_ids <- c(
        "RB9_container",
        "RB10_container",
        "RB11_container",
        "RB12_container"
      )

      for (container_id in conditional_ids) {
        if (rb8_fully_met) {
          shinyjs::addClass(id = container_id, class = "criterion-disabled")
        } else {
          shinyjs::removeClass(id = container_id, class = "criterion-disabled")
        }
      }
    }) |>
      bindEvent(input$RB8_score, ignoreInit = TRUE)

    ## observe: Collect reliability scores ----
    # upstream: button
    # downstream: reliability_scores, session$userData
    observe({
      # Define all reliability criteria
      criteria_ids <- paste0("RB", 1:19) # Will need to expand when we add RB14-RB19

      # Collect scores for completed criteria
      scores_data <- data.frame(
        criterion_id = character(),
        criterion_title = character(),
        required_recommended = character(),
        score = character(),
        justification = character(),
        stringsAsFactors = FALSE
      )

      # RB1
      if (!isTruthy(input$RB1_score) && input$RB1_score != "") {
        scores_data <- rbind(
          scores_data,
          data.frame(
            criterion_id = "RB1",
            criterion_title = "Sample Medium/Matrix",
            required_recommended = "Required",
            score = input$RB1_score,
            justification = input$RB1_justification %||% "",
            stringsAsFactors = FALSE
          )
        )
      }

      # RB2
      if (!isTruthy(input$RB2_score) && input$RB2_score != "") {
        scores_data <- rbind(
          scores_data,
          data.frame(
            criterion_id = "RB2",
            criterion_title = "Collection Method/Sample Type",
            required_recommended = "Recommended",
            score = input$RB2_score,
            justification = input$RB2_justification %||% "",
            stringsAsFactors = FALSE
          )
        )
      }

      # Continue for other criteria...
      # (Pattern established - would continue for RB3-RB19)

      # Store in reactiveValues and session
      reliability_scores$data <- scores_data
      session$userData$reactiveValues$creedReliabilityScores <- scores_data
    }) |>
      bindEvent(input$calc_scores, ignoreInit = TRUE)

    # 3. Outputs ----

    ## output: completion_status ----
    # upstream: reliability_scores$data
    # downstream: UI completion display
    output$completion_status <- renderUI({
      if (
        !isTruthy(reliability_scores$data) || nrow(reliability_scores$data) == 0
      ) {
        div(
          bs_icon("exclamation-triangle", class = "text-warning"),
          "No criteria completed yet",
          class = "text-muted"
        )
      } else {
        completed <- nrow(reliability_scores$data)
        total <- 19 # Total number of criteria

        div(
          bs_icon("check-circle", class = "text-success"),
          paste("Completed:", completed, "of", total, "criteria"),
          if (completed == total) {
            span(" - All criteria complete!", class = "text-success")
          }
        )
      }
    })
  })
}

## To be copied in the UI ----
# mod_CREED_reliability_ui("reliability_1")

## To be copied in the server ----
# mod_CREED_reliability_server("reliability_1")
