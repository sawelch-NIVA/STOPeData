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
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets airDatepickerInput
mod_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      card_header("Sample Combinations Data Management"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Sample Combinations Information",
            icon = bs_icon("info-circle"),
            "This module creates sample combinations by selecting sites, parameters, compartments, and sampling dates. Each combination represents a specific sample that will be collected. Select multiple values from each category to generate all possible combinations."
          )
        ),

        ## Sample combination form ----
        div(
          style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px; margin: 15px 0;",
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
                  class = "btn-sm btn-outline-primary",
                  style = "margin-right: 5px;"
                ),
                actionButton(
                  ns("remove_all_sites"),
                  "Remove All",
                  class = "btn-sm btn-outline-danger"
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
                  class = "btn-sm btn-outline-primary",
                  style = "margin-right: 5px;"
                ),
                actionButton(
                  ns("remove_all_parameters"),
                  "Remove All",
                  class = "btn-sm btn-outline-danger"
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
                  class = "btn-sm btn-outline-primary",
                  style = "margin-right: 5px;"
                ),
                actionButton(
                  ns("remove_all_compartments"),
                  "Remove All",
                  class = "btn-sm btn-outline-danger"
                )
              )
            ),

            ## Sampling dates ----
            airDatepickerInput(
              inputId = ns("sampling_date"),
              label = tooltip(
                list("Sampling Dates", bs_icon("info-circle-fill")),
                "Dates when samples were collected"
              ),
              dateFormat = "yyyy-MM-dd",
              language = "en",
              width = "100%",
              multiple = TRUE,
              todayButton = TRUE,
              addon = "none"
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
          ),

          ## Preview info ----
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #e9ecef; border-radius: 5px;",
            uiOutput(ns("combination_preview"))
          )
        ),

        ## Action buttons for table management ----
        div(
          style = "margin: 15px 0;",
          input_task_button(
            id = ns("remove_selected"),
            label = "Remove Selected",
            icon = icon("trash"),
            class = "btn-danger",
            width = "200px"
          ),
          input_task_button(
            id = ns("clear_all"),
            label = "Clear All Samples",
            icon = icon("broom"),
            class = "btn-warning",
            width = "200px"
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
    ),

    ## Navigation buttons ----
    div(
      class = "navigation-buttons-container",
      style = "display: flex; justify-content: space-between; margin-top: 20px;",

      actionButton(
        inputId = ns("previous_section"),
        label = "Previous Section",
        class = "btn-secondary",
        width = "200px"
      ),

      actionButton(
        inputId = ns("next_section"),
        label = "Next Section",
        class = "btn-success",
        width = "200px"
      )
    )
  )
}

#' Samples Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectizeInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_context_menu
#' @importFrom shinyjs enable disable
mod_samples_server <- function(
  id
) {
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

    ## Dummy data for standalone testing ----
    dummy_sites <- data.frame(
      SITE_CODE = c("SITE_001", "SITE_002", "SITE_003"),
      SITE_NAME = c("River Site A", "Lake Site B", "Coastal Site C"),
      stringsAsFactors = FALSE
    )

    dummy_parameters <- data.frame(
      STRESSOR_NAME = c("Copper", "Lead", "pH", "Dissolved oxygen"),
      STRESSOR_TYPE = c(
        "Stressor",
        "Stressor",
        "Quality parameter",
        "Quality parameter"
      ),
      stringsAsFactors = FALSE
    )

    dummy_compartments <- data.frame(
      ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Terrestrial"),
      ENVIRON_COMPARTMENT_SUB = c(
        "Freshwater",
        "Marine/Salt Water",
        "Soil A Horizon (Topsoil)"
      ),
      MEASURED_CATEGORY = c("External", "External", "External"),
      stringsAsFactors = FALSE
    )

    ## Initialize empty samples data frame ----
    init_samples_df <- function() {
      data.frame(
        SITE_CODE = character(0),
        SITE_NAME = character(0),
        PARAMETER_NAME = character(0),
        PARAMETER_TYPE = character(0),
        COMPARTMENT = character(0),
        COMPARTMENT_SUB = character(0),
        SAMPLING_DATE = character(0),
        SAMPLE_ID = character(0),
        stringsAsFactors = FALSE
      )
    }

    ## Set initial empty data frame ----
    moduleState$samples_data <- init_samples_df()

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    iv$add_rule("samples_table_validation", function(value) {
      if (nrow(moduleState$samples_data) == 0) {
        "At least one sample combination must be generated"
      } else {
        # Check required fields
        required_fields <- c(
          "SITE_CODE",
          "PARAMETER_NAME",
          "COMPARTMENT",
          "SAMPLING_DATE",
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

    # 2. Helper functions ----

    ## Generate unique sample ID ----
    generate_sample_id <- function(
      site_code,
      parameter_name,
      compartment,
      date
    ) {
      # Create a unique sample ID combining key identifiers
      paste(
        site_code,
        substr(gsub("[^A-Za-z0-9]", "", parameter_name), 1, 10),
        substr(gsub("[^A-Za-z0-9]", "", compartment), 1, 10),
        gsub("-", "", date),
        sep = "_"
      )
    }

    ## Create sample combinations ----
    create_sample_combinations <- function(
      sites,
      parameters,
      compartments,
      date
    ) {
      combinations <- expand.grid(
        SITE_CODE = sites,
        PARAMETER_NAME = parameters,
        COMPARTMENT = compartments,
        SAMPLING_DATE = as.character(date),
        stringsAsFactors = FALSE
      )

      # Add additional columns
      combinations$SITE_NAME <- ""
      combinations$PARAMETER_TYPE <- ""
      combinations$COMPARTMENT_SUB <- ""
      combinations$SAMPLE_ID <- mapply(
        generate_sample_id,
        combinations$SITE_CODE,
        combinations$PARAMETER_NAME,
        combinations$COMPARTMENT,
        combinations$SAMPLING_DATE
      )

      # Reorder columns
      combinations <- combinations[, c(
        "SITE_CODE",
        "SITE_NAME",
        "PARAMETER_NAME",
        "PARAMETER_TYPE",
        "COMPARTMENT",
        "COMPARTMENT_SUB",
        "SAMPLING_DATE",
        "SAMPLE_ID"
      )]

      return(combinations)
    }

    combination_exists <- function(
      existing_data,
      site,
      parameter,
      compartment,
      date
    ) {
      if (nrow(existing_data) == 0) return(FALSE)

      any(
        existing_data$SITE_CODE == site &
          existing_data$PARAMETER_NAME == parameter &
          existing_data$COMPARTMENT == compartment &
          existing_data$SAMPLING_DATE == as.character(date)
      )
    }

    ## Update selectize choices helper ----
    update_selectize_choices <- function(
      session,
      sites,
      parameters,
      compartments
    ) {
      # Update sites choices
      site_choices <- setNames(
        sites$SITE_CODE,
        paste(sites$SITE_CODE, "-", sites$SITE_NAME)
      )
      updateSelectizeInput(
        session,
        "sites_select",
        choices = site_choices,
        options = list(placeholder = "Select sites...")
      )

      # Update parameters choices
      param_choices <- setNames(
        parameters$STRESSOR_NAME,
        paste(
          parameters$STRESSOR_NAME,
          paste0("(", parameters$STRESSOR_TYPE, ")")
        )
      )
      updateSelectizeInput(
        session,
        "parameters_select",
        choices = param_choices,
        options = list(placeholder = "Select parameters...")
      )

      # Update compartments choices
      comp_choices <- setNames(
        paste(
          compartments$ENVIRON_COMPARTMENT,
          compartments$ENVIRON_COMPARTMENT_SUB,
          sep = " | "
        ),
        paste(
          compartments$ENVIRON_COMPARTMENT,
          "→",
          compartments$ENVIRON_COMPARTMENT_SUB,
          "→",
          compartments$MEASURED_CATEGORY
        )
      )
      updateSelectizeInput(
        session,
        "compartments_select",
        choices = comp_choices,
        options = list(placeholder = "Select compartments...")
      )
    }

    ## Create sample combinations with duplicate checking ----
    create_sample_combinations <- function(
      sites,
      parameters,
      compartments,
      dates,
      existing_data
    ) {
      all_combinations <- data.frame()
      skipped_count <- 0

      for (date in dates) {
        date_combinations <- expand.grid(
          SITE_CODE = sites,
          PARAMETER_NAME = parameters,
          COMPARTMENT = compartments,
          SAMPLING_DATE = as.character(date),
          stringsAsFactors = FALSE
        )

        # Filter out existing combinations
        new_combinations <- data.frame()
        for (i in 1:nrow(date_combinations)) {
          if (
            !combination_exists(
              existing_data,
              date_combinations$SITE_CODE[i],
              date_combinations$PARAMETER_NAME[i],
              date_combinations$COMPARTMENT[i],
              date_combinations$SAMPLING_DATE[i]
            )
          ) {
            new_combinations <- rbind(new_combinations, date_combinations[i, ])
          } else {
            skipped_count <- skipped_count + 1
          }
        }

        all_combinations <- rbind(all_combinations, new_combinations)
      }

      if (nrow(all_combinations) > 0) {
        # Add additional columns
        all_combinations$SITE_NAME <- ""
        all_combinations$PARAMETER_TYPE <- ""
        all_combinations$COMPARTMENT_SUB <- ""
        all_combinations$SAMPLE_ID <- mapply(
          generate_sample_id,
          all_combinations$SITE_CODE,
          all_combinations$PARAMETER_NAME,
          all_combinations$COMPARTMENT,
          all_combinations$SAMPLING_DATE
        )

        # Reorder columns
        all_combinations <- all_combinations[, c(
          "SITE_CODE",
          "SITE_NAME",
          "PARAMETER_NAME",
          "PARAMETER_TYPE",
          "COMPARTMENT",
          "COMPARTMENT_SUB",
          "SAMPLING_DATE",
          "SAMPLE_ID"
        )]
      }

      return(list(combinations = all_combinations, skipped = skipped_count))
    }

    ## Update combination preview (now supports multiple dates) ----
    update_combination_preview <- function(
      sites_count,
      params_count,
      comps_count,
      dates_count
    ) {
      total_combinations <- sites_count *
        params_count *
        comps_count *
        dates_count

      div(
        strong("Preview: "),
        sprintf(
          "%d sites × %d parameters × %d compartments × %d dates = %d total combinations",
          sites_count,
          params_count,
          comps_count,
          dates_count,
          total_combinations
        )
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Update selectize choices from other modules or dummy data ----
    # upstream: sites_data(), parameters_data(), compartments_data()
    # downstream: selectize input choices and moduleState
    observe({
      # Use provided data or fallback to dummy data
      sites <- dummy_sites
      parameters <- dummy_parameters
      compartments <- dummy_compartments

      # Store available options
      moduleState$available_sites <- sites
      moduleState$available_parameters <- parameters
      moduleState$available_compartments <- compartments

      # Update choices using helper function
      update_selectize_choices(session, sites, parameters, compartments)
    })

    ## observe: Update combination preview ----
    # upstream: input selections
    # downstream: combination_preview output
    output$combination_preview <- renderUI({
      sites_count <- length(input$sites_select %||% character(0))
      params_count <- length(input$parameters_select %||% character(0))
      comps_count <- length(input$compartments_select %||% character(0))
      dates_count <- length(input$sampling_date %||% character(0)) # Now supports multiple dates

      update_combination_preview(
        sites_count,
        params_count,
        comps_count,
        dates_count
      )
    })

    ## observe ~bindEvent(add_all_sites): Add all sites ----
    # upstream: user clicks input$add_all_sites
    # downstream: input$sites_select
    observe({
      if (!is.null(moduleState$available_sites)) {
        updateSelectizeInput(
          session,
          "sites_select",
          selected = moduleState$available_sites$SITE_CODE
        )
      }
    }) |>
      bindEvent(input$add_all_sites)

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
      if (!is.null(moduleState$available_parameters)) {
        updateSelectizeInput(
          session,
          "parameters_select",
          selected = moduleState$available_parameters$STRESSOR_NAME
        )
      }
    }) |>
      bindEvent(input$add_all_parameters)

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
      if (!is.null(moduleState$available_compartments)) {
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
      }
    }) |>
      bindEvent(input$add_all_compartments)

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

    ## observe ~bindEvent(generate_combinations): Generate sample combinations ----
    # upstream: user clicks input$generate_combinations
    # downstream: moduleState$samples_data
    observe({
      sites <- input$sites_select
      parameters <- input$parameters_select
      compartments <- input$compartments_select
      dates <- input$sampling_date

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

      # Create combinations with duplicate checking
      result <- create_sample_combinations(
        sites,
        parameters,
        compartments,
        dates,
        moduleState$samples_data
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
          "new combinations,",
          skipped_count,
          "duplicates skipped"
        )
      } else {
        paste("Generated", nrow(new_combinations), "sample combinations")
      }

      showNotification(message, type = "message")
    }) |>
      bindEvent(input$generate_combinations)

    ## observe ~bindEvent(remove_selected): Remove selected rows ----
    # upstream: user clicks input$remove_selected
    # downstream: moduleState$samples_data
    observe({
      if (!is.null(input$samples_table)) {
        current_data <- hot_to_r(input$samples_table)

        # Remove last row (simplified - proper row selection is complex in rhandsontable)
        if (nrow(current_data) > 0) {
          moduleState$samples_data <- current_data[
            -nrow(current_data),
            ,
            drop = FALSE
          ]
          showNotification("Removed last row", type = "message")
        } else {
          showNotification("No rows to remove", type = "warning")
        }
      } else {
        showNotification("No data to remove", type = "warning")
      }
    }) |>
      bindEvent(input$remove_selected)

    ## observe ~bindEvent(clear_all): Clear all samples ----
    # upstream: user clicks input$clear_all
    # downstream: moduleState$samples_data
    observe({
      moduleState$samples_data <- init_samples_df()
      showNotification("All samples cleared", type = "message")
    }) |>
      bindEvent(input$clear_all)

    ## observe: Handle table changes ----
    # upstream: input$samples_table changes
    # downstream: moduleState$samples_data
    observe({
      if (!is.null(input$samples_table)) {
        updated_data <- hot_to_r(input$samples_table)
        moduleState$samples_data <- updated_data
      }
    })

    ## observe: Check overall validation status ----
    # upstream: moduleState$samples_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$samples_data) > 0) {
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$samples_data
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    })

    # 4. Outputs ----

    ## output: samples_table ----
    # upstream: moduleState$samples_data
    # downstream: UI table display
    output$samples_table <- renderRHandsontable({
      if (nrow(moduleState$samples_data) == 0) {
        # Show empty table structure
        rhandsontable(init_samples_df(), width = "100%", height = "100%") |>
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      } else {
        rhandsontable(moduleState$samples_data, width = "100%") |>
          hot_col("SAMPLE_ID", readOnly = TRUE) |> # Make sample ID read-only
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
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
# samples_data <- mod_samples_server("samples_1", sites_data, parameters_data, compartments_data)
