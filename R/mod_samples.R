# Samples Import Module ----
# A Shiny module for sample combinations of sites, parameters, and compartments

#' Samples UI Function ----
#'
#' @description A shiny Module for sample combinations data entry with selectize inputs.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput dateInput actionButton
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom golem get_golem_wd
mod_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_samples.md"),

        ## Sample combination form ----
        div(
          style = "padding: 15px; border-radius: 8px; margin: 15px 0;",
          h5("Create Sample Combinations"),

          layout_column_wrap(
            width = "300px",
            fill = FALSE,
            fillable = FALSE,

            ## Sites selection ----
            div(
              selectizeInput(
                inputId = ns("sites_select"),
                label = tooltip(
                  list("Sites", bs_icon("info-circle-fill")),
                  "Select one or more sampling sites"
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "No sites available - add sites first"
                ),
                width = "100%"
              ),
              div(
                style = "margin-top: 5px;",
                actionButton(
                  ns("add_all_sites"),
                  "Add All",
                  class = "btn-sm btn-primary",
                  style = "margin-right: 5px;"
                ) |>
                  disabled(), # until sites are available,
                actionButton(
                  ns("remove_all_sites"),
                  "Remove All",
                  class = "btn-sm btn-danger"
                )
              )
            ),

            ## Parameters selection ----
            div(
              selectizeInput(
                inputId = ns("parameters_select"),
                label = tooltip(
                  list("Parameters", bs_icon("info-circle-fill")),
                  "Select one or more parameters to measure"
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "No parameters available - add parameters first"
                ),
                width = "100%"
              ),
              div(
                style = "margin-top: 5px;",
                actionButton(
                  ns("add_all_parameters"),
                  "Add All",
                  class = "btn-sm btn-primary",
                  style = "margin-right: 5px;"
                ) |>
                  disabled(), # until parameters are available,
                actionButton(
                  ns("remove_all_parameters"),
                  "Remove All",
                  class = "btn-sm btn-danger"
                )
              )
            ),

            ## Compartments selection ----
            div(
              selectizeInput(
                inputId = ns("compartments_select"),
                label = tooltip(
                  list("Compartments", bs_icon("info-circle-fill")),
                  "Select one or more environmental compartment combinations"
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "No compartments available - add compartments first"
                ),
                width = "100%"
              ),
              div(
                style = "margin-top: 5px;",
                actionButton(
                  ns("add_all_compartments"),
                  "Add All",
                  class = "btn-sm btn-primary",
                  style = "margin-right: 5px;"
                ) |>
                  disabled(), # until compartments are available,
                actionButton(
                  ns("remove_all_compartments"),
                  "Remove All",
                  class = "btn-sm btn-danger"
                )
              )
            ),

            ## Sampling dates ----
            div(
              airDatepickerInput(
                inputId = ns("sampling_date"),
                label = tooltip(
                  list("Sampling Dates", bs_icon("info-circle-fill")),
                  "Dates when samples were collected"
                ),
                dateFormat = "yyyy-MM-dd",
                width = "100%",
                multiple = TRUE,
                todayButton = TRUE,
                update_on = "change",
                addon = "none"
              ),
              div(
                style = "margin-top: 5px;",
                actionButton(
                  ns("remove_all_dates"),
                  "Remove All",
                  class = "btn-sm btn-danger"
                ) |>
                  disabled() # start disabled until dates are selected
              )
            ),

            ## Sample replicates ----
            numericInput(
              inputId = ns("sample_replicates"),
              label = tooltip(
                list("Sample Replicates", bs_icon("info-circle-fill")),
                "Number of replicate samples for each combination"
              ),
              value = 1,
              min = 1,
              max = 20,
              step = 1,
              width = "100%"
            )
          ),

          ## Generate combinations button ----
          div(
            style = "margin-top: 15px;",
            input_task_button(
              id = ns("generate_combinations"),
              label = "Generate Sample Combinations",
              icon = icon("magic"),
              class = "btn-success",
              width = "250px"
            )
          ) |>
            disabled(),

          ## Preview info ----
          div(
            style = "margin-top: 10px; padding: 10px; border-radius: 5px;",
            uiOutput(ns("combination_preview"))
          )
        ),

        ## Samples table ----
        rHandsontableOutput(
          ns("samples_table"),
          width = "100%",
          height = "100%"
        ),

        ## Validation status ----
        div(
          style = "margin-top: 15px;",
          uiOutput(ns("validation_reporter"))
        ),

        ## Raw data accordion ----
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Click to view raw validated data",
            icon = bs_icon("code"),
            verbatimTextOutput(ns("validated_data_display"))
          )
        )
      )
    )
  )
}

#' Samples Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectizeInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_context_menu hot_col
#' @importFrom shinyjs enable disable disabled
#' @importFrom shinyWidgets updateAirDateInput

mod_samples_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      samples_data = data.frame(),
      validated_data = NULL,
      is_valid = FALSE,
      available_sites = NULL,
      available_parameters = NULL,
      available_compartments = NULL
    )

    ## Set initial empty data frame ----
    moduleState$samples_data <- init_samples_df()

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    iv$add_rule("samples_table_validation", function(value) {
      if (nrow(moduleState$samples_data) == 0) {
        "At least one sample combination must be generated"
      } else {
        # Check required fields - updated for new column structure
        required_fields <- c(
          "SITE_CODE",
          "PARAMETER_NAME",
          "ENVIRON_COMPARTMENT", # Changed from COMPARTMENT
          "ENVIRON_COMPARTMENT_SUB",
          "MEASURED_CATEGORY", # Added this
          "SAMPLING_DATE",
          "REPLICATE",
          "SAMPLE_ID"
        )

        for (i in 1:nrow(moduleState$samples_data)) {
          for (field in required_fields) {
            value <- moduleState$samples_data[i, field]
            if (is.na(value) || value == "") {
              return(paste("Row", i, "is missing required field:", field))
            }
          }
        }
        NULL # All validations passed
      }
    })
    iv$enable()

    # 3. Observers and Reactives ----

    ## observe: Update selectize choices from other modules or dummy data ----
    # upstream: sites_data(), parameters_data(), compartments_data()
    # downstream: selectize input choices and moduleState
    observe({
      # Update sites if data is available
      if (!is.null(session$userData$reactiveValues$sitesData)) {
        update_sites_selectize(
          session,
          session$userData$reactiveValues$sitesData
        )
        moduleState$available_sites <- session$userData$reactiveValues$sitesData
      } else {
        update_sites_selectize(session, dummy_sites)
        moduleState$available_sites <- dummy_sites
      }

      # Update parameters if data is available
      if (!is.null(session$userData$reactiveValues$parametersData)) {
        update_parameters_selectize(
          session,
          session$userData$reactiveValues$parametersData
        )
        moduleState$available_parameters <- session$userData$reactiveValues$parametersData
      } else {
        update_parameters_selectize(session, dummy_parameters)
        moduleState$available_parameters <- dummy_parameters
      }

      # Update compartments if data is available
      if (!is.null(session$userData$reactiveValues$compartmentsData)) {
        update_compartments_selectize(
          session,
          session$userData$reactiveValues$compartmentsData
        )
        moduleState$available_compartments <- session$userData$reactiveValues$compartmentsData
      } else {
        update_compartments_selectize(session, dummy_compartments)
        moduleState$available_compartments <- dummy_compartments
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$sitesData,
        session$userData$reactiveValues$parametersData,
        session$userData$reactiveValues$compartmentsData,
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )

    ## observe ~bindEvent(add_all_sites): Add all sites ----
    # upstream: user clicks input$add_all_sites
    # downstream: input$sites_select
    observe({
      if (
        isTruthy(moduleState$available_sites) &&
          nrow(moduleState$available_sites) > 0
      ) {
        updateSelectizeInput(
          session,
          "sites_select",
          selected = moduleState$available_sites$SITE_CODE
        )
        enable(id = "add_all_sites")
        enable(id = "sites_select")
      } else {
        disable(id = "add_all_sites")
        disable(id = "sites_select")
      }
    }) |>
      bindEvent(input$add_all_sites, moduleState$available_sites)

    ## observe ~bindEvent(remove_all_sites): Remove all sites ----
    # upstream: user clicks input$remove_all_sites
    # downstream: input$sites_select
    observe({
      updateSelectizeInput(session, "sites_select", selected = character(0))
    }) |>
      bindEvent(input$remove_all_sites)

    ## observe ~bindEvent(add_all_parameters): Add all parameters ----
    # upstream: user clicks input$add_all_parameters
    # downstream: input$parameters_select
    observe({
      if (
        isTruthy(moduleState$available_parameters) &&
          nrow(moduleState$available_parameters) > 0
      ) {
        updateSelectizeInput(
          session,
          "parameters_select",
          selected = session$userData$reactiveValues$parametersData$PARAMETER_NAME
        )
        enable(id = "add_all_parameters")
        enable(id = "parameters_select")
      } else {
        showNotification("No parameters found to add.")
        disable(id = "add_all_parameters")
        disable(id = "parameters_select")
      }
    }) |>
      bindEvent(
        input$add_all_parameters,
        moduleState$available_parameters
      )

    ## observe ~bindEvent(remove_all_parameters): Remove all parameters ----
    # upstream: user clicks input$remove_all_parameters
    # downstream: input$parameters_select
    observe({
      updateSelectizeInput(
        session,
        "parameters_select",
        selected = character(0)
      )
    }) |>
      bindEvent(input$remove_all_parameters)

    ## observe ~bindEvent(add_all_compartments): Add all compartments ----
    # upstream: user clicks input$add_all_compartments
    # downstream: input$compartments_select
    observe({
      if (
        isTruthy(moduleState$available_compartments) &&
          nrow(moduleState$available_compartments) > 0
      ) {
        compartment_values <- paste(
          moduleState$available_compartments$ENVIRON_COMPARTMENT,
          moduleState$available_compartments$ENVIRON_COMPARTMENT_SUB,
          sep = " | "
        )
        updateSelectizeInput(
          session,
          "compartments_select",
          selected = compartment_values
        )
        enable(id = "add_all_compartments")
        enable(id = "compartments_select")
      } else {
        disable(id = "add_all_compartments")
        disable(id = "compartments_select")
      }
    }) |>
      bindEvent(input$add_all_compartments, moduleState$available_compartments)

    ## observe ~bindEvent(remove_all_compartments): Remove all compartments ----
    # upstream: user clicks input$remove_all_compartments
    # downstream: input$compartments_select
    observe({
      updateSelectizeInput(
        session,
        "compartments_select",
        selected = character(0)
      )
    }) |>
      bindEvent(input$remove_all_compartments)

    ## observe: Enable/disable remove all sites button ----
    # upstream: input$sites_select
    # downstream: remove_all_sites button state
    observe({
      if (isTruthy(input$sites_select) && length(input$sites_select) > 0) {
        enable("remove_all_sites")
      } else {
        disable("remove_all_sites")
      }
    }) |>
      bindEvent(input$sites_select, ignoreNULL = FALSE)

    ## observe: Enable/disable remove all parameters button ----
    # upstream: input$parameters_select
    # downstream: remove_all_parameters button state
    observe({
      if (
        isTruthy(input$parameters_select) && length(input$parameters_select) > 0
      ) {
        enable("remove_all_parameters")
      } else {
        disable("remove_all_parameters")
      }
    }) |>
      bindEvent(input$parameters_select, ignoreNULL = FALSE)

    ## observe: Enable/disable remove all compartments button ----
    # upstream: input$compartments_select
    # downstream: remove_all_compartments button state
    observe({
      if (
        isTruthy(input$compartments_select) &&
          length(input$compartments_select) > 0
      ) {
        enable("remove_all_compartments")
      } else {
        disable("remove_all_compartments")
      }
    }) |>
      bindEvent(input$compartments_select, ignoreNULL = FALSE)

    ## observe: Enable/disable remove all dates button ----
    # upstream: input$sampling_date
    # downstream: remove_all_dates button state
    observe({
      if (isTruthy(input$sampling_date) && length(input$sampling_date) > 0) {
        enable("remove_all_dates")
      } else {
        disable("remove_all_dates")
      }
    }) |>
      bindEvent(input$sampling_date, ignoreNULL = FALSE)

    ## observe ~bindEvent(remove_all_dates): Remove all dates ----
    # upstream: user clicks input$remove_all_dates
    # downstream: input$sampling_date
    observe({
      updateAirDateInput(
        session,
        "sampling_date",
        clear = TRUE
      )
    }) |>
      bindEvent(input$remove_all_dates)

    ## observe ~bindEvent(generate_combinations): Enable generate button when options valid ----
    observe({
      if (
        all(
          isTruthy(c(
            input$sites_select,
            input$compartments_select,
            input$parameters_select,
            input$sampling_date,
            input$sample_replicates
          ))
        )
      ) {
        enable("generate_combinations")
      } else {
        disable("generate_combinations")
      }
    }) |>
      bindEvent(
        input$sites_select,
        input$compartments_select,
        input$parameters_select,
        input$sampling_date,
        input$sample_replicates
      )

    ## observe ~bindEvent(generate_combinations): Generate sample combinations ----
    # upstream: user clicks input$generate_combinations
    # downstream: moduleState$samples_data
    observe({
      sites <- input$sites_select
      parameters <- input$parameters_select
      compartments <- input$compartments_select
      dates <- input$sampling_date
      replicates <- input$sample_replicates %||% 1

      if (
        length(sites) == 0 ||
          length(parameters) == 0 ||
          length(compartments) == 0 ||
          length(dates) == 0
      ) {
        showNotification(
          "Please select at least one site, parameter, compartment, and date",
          type = "warning"
        )
        return()
      }

      # Create combinations with duplicate checking - now passing available_sites and available_parameters
      result <- create_sample_combinations(
        sites,
        parameters,
        compartments, # These are still merged format like "Aquatic | Freshwater"
        dates,
        replicates,
        moduleState$samples_data,
        moduleState$available_compartments, # Pass this for parsing
        moduleState$available_sites, # Pass this for SITE_NAME lookup
        moduleState$available_parameters # Pass this for PARAMETER_TYPE lookup
      )
      new_combinations <- result$combinations
      skipped_count <- result$skipped

      # Add to existing data
      if (nrow(new_combinations) > 0) {
        moduleState$samples_data <- rbind(
          moduleState$samples_data,
          new_combinations
        )
      }

      # Show notification with details
      message <- if (skipped_count > 0) {
        paste(
          "Generated",
          nrow(new_combinations),
          "new sample combinations,",
          skipped_count,
          "duplicates skipped"
        )
      } else {
        paste("Generated", nrow(new_combinations), "sample combinations")
      }
      showNotification(message, type = "message")
    }) |>
      bindEvent(input$generate_combinations)

    ## observe: Handle table changes ----
    # upstream: input$samples_table changes
    # downstream: moduleState$samples_data
    observe({
      if (!is.null(input$samples_table)) {
        updated_data <- hot_to_r(input$samples_table)
        moduleState$samples_data <- updated_data
      }
    })

    ## observe: Check overall validation, send data to session$userData ----
    # upstream: moduleState$samples_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$samples_data) > 0) {
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$samples_data

        session$userData$reactiveValues$sampleData <- moduleState$validated_data
        print_dev(glue(
          "mod_samples is valid: {moduleState$is_valid},
                       session$userData$reactiveValues$sampleData: {nrow(session$userData$reactiveValues$sampleData)} rows"
        ))
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    })

    # 4. Outputs ----

    ## output: Update combination preview ----
    # upstream: input selections
    # downstream: combination_preview output
    output$combination_preview <- renderUI({
      sites_count <- length(input$sites_select %||% character(0))
      params_count <- length(input$parameters_select %||% character(0))
      comps_count <- length(input$compartments_select %||% character(0))
      dates_count <- length(input$sampling_date %||% character(0))
      replicates_count <- input$sample_replicates %||% 1

      update_combination_preview(
        sites_count,
        params_count,
        comps_count,
        dates_count,
        replicates_count
      )
    })

    ## output: samples_table ----
    # upstream: moduleState$samples_data
    # downstream: UI table display
    output$samples_table <- renderRHandsontable({
      if (nrow(moduleState$samples_data) == 0) {
        # Show empty table structure
        rhandsontable(
          init_samples_df(),
          stretchH = "all",
          height = "inherit",
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_col("SAMPLE_ID", readOnly = TRUE) |> # Make sample ID read-only
          hot_col("REPLICATE_ID", readOnly = TRUE) |> # Make replicate ID read-only
          hot_context_menu(
            allowRowEdit = TRUE, # Enable row operations
            allowColEdit = FALSE, # Disable column operations
            customOpts = list(
              # Only include remove_row in the menu
              "row_above" = NULL,
              "row_below" = NULL,
              "remove_row" = list(
                name = "Remove selected rows"
              )
            )
          )
      } else {
        rhandsontable(
          moduleState$samples_data,
          stretchH = "all",
          height = "inherit",
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_col("SITE_CODE", readOnly = TRUE) |>
          hot_col("SITE_NAME", readOnly = TRUE) |>
          hot_col("SAMPLE_ID", readOnly = TRUE) |> # Make sample ID read-only
          hot_col("REPLICATE_ID", readOnly = TRUE) |> # Make replicate ID read-only
          hot_col(
            "SAMPLING_DATE",
            readOnly = TRUE,
            format = "date"
          ) |>
          hot_col(
            "ENVIRON_COMPARTMENT",
            readOnly = TRUE
          ) |>
          hot_col(
            "ENVIRON_COMPARTMENT_SUB",
            readOnly = TRUE
          ) |>
          hot_col(
            "MEASURED_CATEGORY",
            readOnly = TRUE
          ) |>
          hot_context_menu(
            allowRowEdit = TRUE, # Enable row operations
            allowColEdit = FALSE, # Disable column operations
            customOpts = list(
              # Only include remove_row in the menu
              "row_above" = NULL,
              "row_below" = NULL,
              "remove_row" = list(
                name = "Remove selected rows"
              )
            )
          )
      }
    })

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      if (moduleState$is_valid) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All sample data validated successfully.",
            nrow(moduleState$samples_data),
            "sample(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Generate at least one valid sample combination to proceed.",
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: validated_data_display ----
    # upstream: moduleState$validated_data
    # downstream: UI data display
    output$validated_data_display <- renderText({
      if (isTruthy(moduleState$validated_data)) {
        # Format first few samples as examples
        sample_count <- nrow(moduleState$validated_data)
        display_count <- min(5, sample_count)

        sample_entries <- lapply(1:display_count, function(i) {
          sample <- moduleState$validated_data[i, ]
          sample_lines <- sapply(names(sample), function(name) {
            value <- sample[[name]]
            if (is.na(value) || is.null(value) || value == "") {
              paste0("  ", name, " = NA")
            } else if (is.character(value)) {
              paste0("  ", name, " = '", value, "'")
            } else {
              paste0("  ", name, " = ", as.character(value))
            }
          })
          paste0("Sample ", i, ":\n", paste(sample_lines, collapse = "\n"))
        })

        result <- paste(sample_entries, collapse = "\n\n")
        if (sample_count > display_count) {
          result <- paste0(
            result,
            "\n\n# ... and ",
            sample_count - display_count,
            " more samples"
          )
        }

        return(result)
      } else {
        "# Sample combinations will appear here when generated"
      }
    })

    # 5. Return ----
    ## return: validated data for other modules ----
    # upstream: moduleState$validated_data
    # downstream: app_server.R
    return(
      reactive({
        moduleState$validated_data %|truthy|% NULL
      })
    )
  })
}

## To be copied in the UI ----
# mod_samples_ui("samples_1")

## To be copied in the server ----
# samples_data <- mod_samples_server("samples_1")
