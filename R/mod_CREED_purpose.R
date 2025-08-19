# CREED Purpose Statement Module ----
# A Shiny module for defining CREED assessment purpose and thresholds

#' CREED_purpose UI Function
#'
#' @description A shiny Module for defining CREED assessment purpose and criterion thresholds.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textAreaInput h5 p strong div actionButton fileInput downloadButton hr h6
#' @importFrom bslib layout_columns input_task_button
#' @importFrom bsicons bs_icon
mod_CREED_purpose_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Info section ----
    # Purpose statement section ----
    p(
      "Describe the objective for which the usability of the dataset is assessed, including any required dataset thresholds."
    ),
    textAreaInput(
      inputId = ns("purpose_statement"),
      label = "Purpose Statement:",
      placeholder = "Enter the assessment purpose and any specific requirements...",
      rows = 6,
      width = "100%"
    ),

    hr(),

    # Import/Export section ----
    layout_columns(
      col_widths = c(6, 6),
      div(
        fileInput(
          inputId = ns("import_file"),
          label = "Import Purpose & Thresholds (YAML)",
          accept = c(".yaml", ".yml")
        ),
        input_task_button(
          id = ns("import_btn"),
          label = "Load Imported Data",
          icon = bs_icon("upload")
        )
      ),
      div(
        downloadButton(
          outputId = ns("export_btn"),
          label = "Export Purpose & Thresholds",
          icon = icon("download")
        ),
        br(),
        br(),
        input_task_button(
          id = ns("clear_all"),
          label = "Clear All Fields",
          icon = bs_icon("trash"),
          class = "btn-outline-danger"
        )
      )
    ),

    hr(),

    # Purpose summary table section ----
    p(
      "Enter thresholds for 'Partly Met' (minimum requirements) and 'Fully Met' (optimal requirements). ",
      "Leave fields blank if no specific threshold is needed.",
      class = "text-muted"
    ),

    # RV1 ----
    h6(
      "RV1: The sampling medium/matrix was appropriate for the given purpose."
    ),
    layout_column_wrap(
      width = "400px",
      textAreaInput(
        inputId = ns("RV1_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV1_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV2 ----
    h6("RV2: The sample collection method was adequate for the given purpose."),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV2_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV2_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV3 ----
    h6(
      "RV3: The study area and number of locations sampled was suitable for the given purpose."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV3_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV3_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV4 ----
    h6(
      "RV4: The rationale for selection of sampling locations was provided and it is suitable for the given purpose."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV4_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV4_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV5 ----
    h6(
      "RV5: The samples were collected over a time scale that was appropriate for the given purpose."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV5_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV5_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV6 ----
    h6(
      "RV6: Over the timespan, the sampling frequency was appropriate for the given purpose."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV6_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV6_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV7 ----
    h6(
      "RV7: Conditions during sampling events were documented and relevant for the given purpose."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV7_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV7_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV8 ----
    h6(
      "RV8: The analyte(s) reported was/were appropriate for the given purpose."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV8_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV8_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV9 ----
    h6("RV9: The method was sensitive enough for the given purpose."),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV9_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV9_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV10 ----
    h6(
      "RV10: The summary statistics provided were appropriate for the given purpose."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV10_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV10_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    # RV11 ----
    h6(
      "RV11: All supporting parameters that were needed to achieve the given purpose were provided."
    ),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV11_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%"
      ),
      textAreaInput(
        inputId = ns("RV11_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%"
      )
    ),

    hr(),

    # Status section ----
    h5("Status"),
    uiOutput(ns("purpose_status"))
  )
}

#' CREED_purpose Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI req updateTextAreaInput showNotification
#' @importFrom yaml read_yaml write_yaml
mod_CREED_purpose_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: purpose_data ----
    purpose_data <- reactiveValues()

    # Define criterion IDs ----
    criterion_ids <- paste0("RV", 1:11)

    # 2. Reactive expressions ----

    ## reactive: collect_purpose_data ----
    # upstream: all input fields
    # downstream: purpose_data, session$userData
    collect_purpose_data <- reactive({
      # Collect purpose statement
      purpose_statement <- input$purpose_statement %||% ""

      # Collect thresholds for each criterion
      thresholds <- list()
      for (criterion_id in criterion_ids) {
        partly_met <- input[[paste0(criterion_id, "_partly_met")]] %||% ""
        fully_met <- input[[paste0(criterion_id, "_fully_met")]] %||% ""

        thresholds[[criterion_id]] <- list(
          partly_met = partly_met,
          fully_met = fully_met
        )
      }

      list(
        purpose_statement = purpose_statement,
        thresholds = thresholds,
        created_date = Sys.Date(),
        module_version = "1.0"
      )
    })

    # 3. Observers ----

    ## observe: Store data automatically ----
    # upstream: collect_purpose_data()
    # downstream: purpose_data, session$userData
    observe({
      data <- collect_purpose_data()
      purpose_data$current <- data

      # Store in session for other modules to access
      if (is.null(session$userData$reactiveValues)) {
        session$userData$reactiveValues <- list()
      }
      session$userData$reactiveValues$creedPurpose <- data
    }) |>
      bindEvent(
        input$purpose_statement,
        c(lapply(criterion_ids, function(x) input[[paste0(x, "_partly_met")]])),
        c(lapply(criterion_ids, function(x) input[[paste0(x, "_fully_met")]])),
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    ## observe: Import data ----
    # upstream: input$import_btn, input$import_file
    # downstream: UI field updates
    observe({
      req(input$import_file)

      tryCatch(
        {
          imported_data <- yaml::read_yaml(input$import_file$datapath)

          # Update purpose statement
          if (!is.null(imported_data$purpose_statement)) {
            updateTextAreaInput(
              session,
              "purpose_statement",
              value = imported_data$purpose_statement
            )
          }

          # Update thresholds
          if (!is.null(imported_data$thresholds)) {
            for (criterion_id in criterion_ids) {
              if (!is.null(imported_data$thresholds[[criterion_id]])) {
                criterion_data <- imported_data$thresholds[[criterion_id]]

                if (!is.null(criterion_data$partly_met)) {
                  updateTextAreaInput(
                    session,
                    paste0(criterion_id, "_partly_met"),
                    value = criterion_data$partly_met
                  )
                }

                if (!is.null(criterion_data$fully_met)) {
                  updateTextAreaInput(
                    session,
                    paste0(criterion_id, "_fully_met"),
                    value = criterion_data$fully_met
                  )
                }
              }
            }
          }

          showNotification(
            "Purpose data imported successfully",
            type = "success"
          )
        },
        error = function(e) {
          showNotification(
            paste("Import failed:", e$message),
            type = "error"
          )
        }
      )
    }) |>
      bindEvent(input$import_btn)

    ## observe: Clear all fields ----
    # upstream: input$clear_all
    # downstream: UI field updates
    observe({
      # Clear purpose statement
      updateTextAreaInput(session, "purpose_statement", value = "")

      # Clear all thresholds
      for (criterion_id in criterion_ids) {
        updateTextAreaInput(
          session,
          paste0(criterion_id, "_partly_met"),
          value = ""
        )
        updateTextAreaInput(
          session,
          paste0(criterion_id, "_fully_met"),
          value = ""
        )
      }

      showNotification(
        "All fields cleared",
        type = "message"
      )
    }) |>
      bindEvent(input$clear_all)

    # 4. Outputs ----

    ## output: Export handler ----
    # upstream: collect_purpose_data()
    # downstream: YAML download
    output$export_btn <- downloadHandler(
      filename = function() {
        paste0("CREED_purpose_", Sys.Date(), ".yaml")
      },
      content = function(file) {
        data <- collect_purpose_data()
        yaml::write_yaml(data, file)
      },
      contentType = "application/x-yaml"
    )

    ## output: purpose_status ----
    # upstream: purpose_data$current
    # downstream: UI status display
    output$purpose_status <- renderUI({
      if (is.null(purpose_data$current)) {
        div(
          bs_icon("exclamation-triangle", class = "text-warning"),
          "No purpose data entered yet",
          class = "text-muted"
        )
      } else {
        data <- purpose_data$current

        # Count completed fields
        purpose_complete <- nchar(data$purpose_statement %||% "") > 0

        thresholds_with_data <- sum(sapply(data$thresholds, function(x) {
          nchar(x$partly_met %||% "") > 0 || nchar(x$fully_met %||% "") > 0
        }))

        div(
          if (purpose_complete) {
            span(
              bs_icon("check-circle", class = "text-success"),
              "Purpose statement complete",
              br()
            )
          } else {
            span(
              bs_icon("exclamation-circle", class = "text-warning"),
              "Purpose statement needed",
              br()
            )
          },
          span(
            bs_icon("list-task"),
            paste(
              "Thresholds defined for",
              thresholds_with_data,
              "of",
              length(criterion_ids),
              "criteria"
            )
          )
        )
      }
    })
  })
}

## To be copied in the UI ----
# mod_CREED_purpose_ui("CREED_purpose_1")

## To be copied in the server ----
# mod_CREED_purpose_server("CREED_purpose_1")
