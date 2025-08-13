# Parameters Import Module ----
# A Shiny module for parameter data entry with filtered dropdowns and chemical validation

#' Parameters UI Function ----
#'
#' @description A shiny Module for parameter data entry with type-filtered parameter selection.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectInput updateSelectizeInput
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom tibble tibble
#' @export
mod_parameters_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_parameters.md"),

        ## Parameter selection controls ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          selectizeInput(
            inputId = ns("parameter_type_select"),
            label = "Parameter Type (dummy data)",
            choices = c(
              "Stressor" = "Stressor",
              "Quality parameter" = "Quality parameter",
              "Normalization" = "Normalization",
              "Background" = "Background"
            ),
            selected = "Stressor",
            width = "100%",
            multiple = FALSE
          ),

          selectizeInput(
            inputId = ns("parameter_subtype_select"),
            label = "Parameter Subtype",
            choices = c("Show all" = "Show all"),
            selected = "Show all",
            width = "100%",
            multiple = FALSE
          ),

          selectizeInput(
            inputId = ns("parameter_name_select"),
            label = "Parameter Name",
            choices = c("Select parameter type first..."),
            width = "100%",
            selected = "Formaldehyde",
            multiple = FALSE
          )
        ),

        ## Action buttons and validation status ----
        div(
          style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",

          input_task_button(
            id = ns("add_existing"),
            label = "Add Existing Parameter",
            icon = icon("plus-circle"),
            class = "btn-success",
            width = "200px"
          ),

          input_task_button(
            id = ns("add_new"),
            label = "Add New Parameter",
            icon = icon("plus"),
            class = "btn-info",
            width = "200px"
          ),

          ### Validation status ----
          uiOutput(ns("validation_reporter"))
        ),

        ## Parameter validation results (in case of LLM data only) ----
        conditionalPanel(
          condition = "output.show_validation",
          ns = ns,
          div(
            h6("Parameter Database Lookup", style = "color: #0066cc;"),
            div(
              style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #0066cc;",
              verbatimTextOutput(ns("parameter_validation_results"))
            )
          )
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

    ## Parameters table card ----
    card(
      div(
        rHandsontableOutput(ns("parameters_table")),
        style = "margin-bottom: 10px;"
      )
    )
  )
}

#' Parameters Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
#' @importFrom golem print_dev
#' @importFrom glue glue
#' @importFrom dplyr mutate bind_rows pull filter arrange select distinct
#' @importFrom arrow read_parquet
#' @importFrom purrr negate
#' @importFrom tibble tibble
#' @export
mod_parameters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%notin%` <- negate(`%in%`)

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      parameters_data = tibble(),
      validated_data = NULL,
      is_valid = FALSE,
      next_param_id = 1,
      session_parameters = list(),
      validation_results = NULL,
      show_validation = FALSE
    )

    ## Dummy parameter data ----
    # Read dummy_parameters ----
    dummy_quality_params <- read_parquet(
      file = "inst/data/clean/dummy_quality_parameters.parquet"
    ) |>
      mutate(ENTERED_BY = "saw@niva.no")

    # Read and prepare chemical_parameters ----
    chemical_parameters <- read_parquet(
      file = "inst/data/clean/ClassyFire_Taxonomy_2025_02.parquet"
    ) |>
      mutate(
        MEASURED_TYPE = "Concentration",
        ENTERED_BY = "saw@niva.no"
      ) |>
      arrange(PARAMETER_NAME) |>
      mutate(
        PARAMETER_TYPE_SUB = case_when(
          PARAMETER_NAME == "Carbon" ~ "Carbon",
          TRUE ~ PARAMETER_TYPE_SUB
        ),
        PARAMETER_NAME_SUB = ""
      )

    # Merge datasets ----
    dummy_parameters <- bind_rows(dummy_quality_params, chemical_parameters)

    ## Controlled vocabulary options ----
    parameter_types <- c(
      "Not relevant",
      "Stressor",
      "Quality parameter",
      "Normalization",
      "Background",
      "Other"
    )

    parameter_types_sub <- dummy_parameters |>
      select(PARAMETER_TYPE_SUB) |>
      distinct() |>
      arrange(PARAMETER_TYPE_SUB) |>
      pull(PARAMETER_TYPE_SUB)

    measured_types <- c(
      "Not relevant",
      "Concentration",
      "Dose rate",
      "Dose",
      "Physical parameter",
      "Amount",
      "Volume",
      "Fraction of total",
      "Percent",
      "Irradiance",
      "Response",
      "Other"
    )

    ## Initialize empty parameters data frame ----
    init_parameters_df <- function() {
      tibble(
        PARAMETER_TYPE = character(0),
        PARAMETER_TYPE_SUB = character(0),
        MEASURED_TYPE = character(0),
        PARAMETER_NAME = character(0),
        PARAMETER_NAME_SUB = character(0),
        INCHIKEY_SD = character(0),
        PUBCHEM_CID = character(0),
        CAS_RN = NA,
        ENTERED_BY = character(0)
      )
    }

    ## Set initial empty data frame ----
    moduleState$parameters_data <- init_parameters_df()

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    iv$add_rule("parameters_table_validation", function(value) {
      if (nrow(moduleState$parameters_data) == 0) {
        "At least one parameter must be added"
      } else {
        # Check required fields
        required_fields <- c(
          "PARAMETER_TYPE",
          "MEASURED_TYPE",
          "PARAMETER_NAME",
          "ENTERED_BY"
        )

        for (i in 1:nrow(moduleState$parameters_data)) {
          for (field in required_fields) {
            value <- moduleState$parameters_data[i, field]
            if (is.na(value) || value == "") {
              return(paste("Row", i, "is missing required field:", field))
            }
          }

          # Conditional validation for PARAMETER_TYPE_SUB
          parameter_type <- moduleState$parameters_data[i, "PARAMETER_TYPE"]
          PARAMETER_TYPE_SUB <- moduleState$parameters_data[
            i,
            "PARAMETER_TYPE_SUB"
          ]

          if (
            parameter_type %in%
              c("Stressor", "Quality parameter", "Normalization", "Background")
          ) {
            if (is.na(PARAMETER_TYPE_SUB) || PARAMETER_TYPE_SUB == "") {
              return(paste(
                "Row",
                i,
                "requires PARAMETER_TYPE_SUB when PARAMETER_TYPE is",
                parameter_type
              ))
            }
          }

          # Pattern validation for chemical identifiers
          cas_rn <- moduleState$parameters_data[i, "CAS_RN"]
          inchikey <- moduleState$parameters_data[i, "INCHIKEY_SD"]
          pubchem <- moduleState$parameters_data[i, "PUBCHEM_CID"]

          if (
            !is.na(cas_rn) &&
              cas_rn != "" &&
              !grepl("^[0-9]{1,7}-[0-9]{2}-[0-9]$", cas_rn)
          ) {
            return(paste(
              "Row",
              i,
              "has invalid CAS_RN format (should be XXXXXXX-XX-X)"
            ))
          }

          if (
            !is.na(inchikey) &&
              inchikey != "" &&
              !grepl("^[A-Z]{14}-[A-Z]{10}-[A-Z]$", inchikey)
          ) {
            return(paste("Row", i, "has invalid INCHIKEY_SD format"))
          }

          if (
            !is.na(pubchem) &&
              pubchem != "" &&
              !grepl("^[0-9]\\d{0,8}$", pubchem)
          ) {
            return(paste("Row", i, "has invalid PUBCHEM_CID format"))
          }
        }
        NULL # All validations passed
      }
    })

    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: Update subtype dropdown when parameter type changes ----
    # upstream: input$parameter_type_select
    # downstream: input$parameter_subtype_select choices
    observe({
      param_type <- input$parameter_type_select

      if (isTruthy(param_type)) {
        # Get available subtypes for this parameter type
        type_filtered_data <- dummy_parameters |>
          filter(PARAMETER_TYPE == param_type)

        available_subtypes <- if (nrow(type_filtered_data) > 0) {
          type_filtered_data |>
            pull(PARAMETER_TYPE_SUB) |>
            unique() |>
            sort()
        } else {
          character(0)
        }

        # Add "Show all" option at the beginning
        subtype_choices <- c(
          "Show all" = "Show all",
          setNames(available_subtypes, available_subtypes)
        )

        updateSelectizeInput(
          session,
          "parameter_subtype_select",
          choices = subtype_choices,
          selected = "Show all",
          server = TRUE
        )
      }
    }) |>
      bindEvent(input$parameter_type_select, ignoreInit = FALSE)

    ## observe: Update parameter name dropdown when type or subtype changes ----
    # upstream: input$parameter_type_select, input$parameter_subtype_select
    # downstream: input$parameter_name_select choices
    observe({
      param_type <- input$parameter_type_select
      param_subtype <- input$parameter_subtype_select

      if (isTruthy(param_type) && isTruthy(param_subtype)) {
        available_names <- get_parameters_filtered(
          param_type = param_type,
          param_subtype = param_subtype,
          dummy_parameters = dummy_parameters,
          session_parameters = moduleState$session_parameters
        )

        updateSelectizeInput(
          session,
          "parameter_name_select",
          choices = available_names,
          selected = if (length(available_names) > 1) {
            available_names[1]
          } else {
            ""
          },
          server = TRUE # server-side selectize for better performance
        )
      }
    }) |>
      bindEvent(
        input$parameter_type_select,
        input$parameter_subtype_select,
        ignoreInit = FALSE
      )

    ## observe ~bindEvent(LLM data validates or updates): Load and validate parameters ----
    # upstream: session$userData$reactiveValues$llmExtractionComplete
    # downstream: moduleState$parameters_data, moduleState$show_validation, moduleState$validation_results
    observe({
      llm_parameters <- session$userData$reactiveValues$parametersDataLLM
      if (
        !is.null(llm_parameters) &&
          nrow(llm_parameters) > 0 &&
          session$userData$reactiveValues$llmExtractionComplete
      ) {
        # Load and validate parameters
        moduleState$parameters_data <- llm_parameters

        # Run validation if chemical_parameters is available
        if (exists("chemical_parameters")) {
          validation_result <- validate_parameters_against_database(
            moduleState$parameters_data,
            chemical_parameters
          )
          moduleState$validation_results <- validation_result
          moduleState$show_validation <- TRUE

          # Show notification based on validation
          if (validation_result$has_warnings) {
            showNotification(
              paste(
                "Loaded",
                nrow(llm_parameters),
                "parameters with validation warnings. Check results below."
              ),
              type = "warning"
            )
          } else {
            showNotification(
              paste(
                "Loaded",
                nrow(llm_parameters),
                "parameters - all validated successfully!"
              ),
              type = "message"
            )
          }
        } else {
          showNotification(
            paste(
              "Loaded",
              nrow(llm_parameters),
              "parameters. Chemical database not available for validation."
            ),
            type = "message"
          )
          moduleState$show_validation <- FALSE
        }
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$llmExtractionComplete,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    ## observe: Add existing parameter ----
    # upstream: user clicks input$add_existing
    # downstream: moduleState$parameters_data
    observe({
      param_type <- input$parameter_type_select
      param_name <- input$parameter_name_select

      if (
        isTruthy(param_type) &&
          isTruthy(param_name) &&
          param_name %notin% moduleState$parameters_data$PARAMETER_NAME
      ) {
        new_param <- create_existing_parameter(
          param_type,
          param_name,
          dummy_parameters = dummy_parameters
        )

        if (!is.null(new_param)) {
          moduleState$parameters_data <- rbind(
            moduleState$parameters_data,
            new_param
          )
          showNotification(
            paste("Added parameter:", param_name),
            type = "message"
          )
          # TODO: This currently doesn't trigger
        } else if (param_name %in% moduleState$parameters_data$PARAMETER_NAME) {
          showNotification("Parameter already present in table", type = "error")
        } else {
          showNotification("Parameter not found", type = "error")
        }
      } else {
        showNotification(
          "Please select a valid parameter type and name",
          type = "warning"
        )
      }
    }) |>
      bindEvent(input$add_existing)

    ## observe: Add new parameter ----
    # upstream: user clicks input$add_new
    # downstream: moduleState$parameters_data
    observe({
      param_type <- input$parameter_type_select

      if (isTruthy(param_type)) {
        new_param <- create_new_parameter(
          param_type,
          session$userData$reactiveValues$ENTERED_BY %|truthy|% ""
        )
        moduleState$parameters_data <- rbind(
          moduleState$parameters_data,
          new_param
        )

        showNotification(
          "Added blank parameter row. Fill in details in the table.",
          type = "message"
        )
      } else {
        showNotification(
          "Please select a parameter type first",
          type = "warning"
        )
      }
    }) |>
      bindEvent(input$add_new)

    ## observe: Handle table changes ----
    # upstream: input$parameters_table changes
    # downstream: moduleState$parameters_data
    observe({
      if (!is.null(input$parameters_table)) {
        updated_data <- hot_to_r(input$parameters_table)

        # Store any new parameters that were created in the session
        for (i in 1:nrow(updated_data)) {
          param_type <- updated_data[i, "PARAMETER_TYPE"]
          param_name <- updated_data[i, "PARAMETER_NAME"]

          if (isTruthy(param_type) && isTruthy(param_name)) {
            # Check if this is a new parameter (not in dummy_parameters)
            if (is.null(dummy_parameters[[param_type]][[param_name]])) {
              # Store in session parameters
              if (is.null(moduleState$session_parameters[[param_type]])) {
                moduleState$session_parameters[[param_type]] <- list()
              }

              moduleState$session_parameters[[param_type]][[
                param_name
              ]] <- list(
                PARAMETER_NAME = param_name,
                PARAMETER_TYPE_SUB = updated_data[i, "PARAMETER_TYPE_SUB"],
                MEASURED_TYPE = updated_data[i, "MEASURED_TYPE"],
                PARAMETER_NAME_SUB = updated_data[i, "PARAMETER_NAME_SUB"],
                INCHIKEY_SD = updated_data[i, "INCHIKEY_SD"],
                PUBCHEM_CID = updated_data[i, "PUBCHEM_CID"],
                CAS_RN = updated_data[i, "CAS_RN"],
                ENTERED_BY = updated_data[i, "ENTERED_BY"]
              )
            }
          }
        }

        moduleState$parameters_data <- updated_data
      }
    })

    ## observe: Check overall validation status and update reactiveValues ----
    # upstream: moduleState$parameters_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$parameters_data) > 0) {
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$parameters_data

        session$userData$reactiveValues$parametersData <- moduleState$validated_data
        print_dev(glue(
          "moduleState$is_valid: {moduleState$is_valid},
                       session$userData$reactiveValues$parametersData: {session$userData$reactiveValues$parametersData}"
        ))
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    })

    ## observe: Load from LLM data when available ----
    # upstream: session$userData$reactiveValues$parametersDataLLM
    # downstream: moduleState$parameters_data
    observe({
      llm_parameters <- session$userData$reactiveValues$parametersDataLLM
      if (
        !is.null(llm_parameters) &&
          nrow(llm_parameters) > 0 &&
          session$userData$reactiveValues$llmExtractionComplete
      ) {
        # Replace current parameters data with LLM data
        moduleState$parameters_data <- create_parameters_from_llm(
          llm_parameters,
          if (exists("chemical_parameters")) chemical_parameters else NULL
        )

        showNotification(
          paste(
            "Loaded",
            nrow(llm_parameters),
            "parameters from LLM extraction. Review and add missing chemical identifiers."
          ),
          type = "message"
        )
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$parametersDataLLM,
        session$userData$reactiveValues$llmExtractionComplete,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    # 3. Outputs ----

    ## output: parameters_table ----
    # upstream: moduleState$parameters_data
    # downstream: UI table display
    output$parameters_table <- renderRHandsontable({
      if (nrow(moduleState$parameters_data) == 0) {
        # Show empty table structure
        rhandsontable(
          init_parameters_df(),
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
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
          moduleState$parameters_data,
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "right") |>
          hot_col(
            "PARAMETER_NAME",
            type = "text",
            renderer = mandatory_highlight_text()
          ) |>
          hot_col(
            "PARAMETER_NAME_SUB",
            type = "text"
          ) |>
          hot_col(
            "PARAMETER_TYPE",
            type = "dropdown",
            source = parameter_types,
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(
            "PARAMETER_TYPE_SUB",
            type = "dropdown",
            source = parameter_types_sub,
            strict = TRUE
          ) |>
          hot_col(
            "MEASURED_TYPE",
            type = "dropdown",
            source = measured_types,
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(c("INCHIKEY_SD", "PUBCHEM_CID", "CAS_RN"), type = "text") |>
          hot_col("ENTERED_BY", renderer = mandatory_highlight_text()) |>
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
    # upstream: moduleState$is_valid, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      llm_indicator <- if (
        session$userData$reactiveValues$llmExtractionComplete
      ) {
        div(
          bs_icon("cpu"),
          "Some data populated from LLM extraction - please review for accuracy",
          class = "validation-status validation-info",
          style = "margin-bottom: 10px;"
        )
      } else {
        NULL
      }

      validation_status <- if (moduleState$is_valid) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All parameter data validated successfully.",
            nrow(moduleState$parameter_data),
            "parameter(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Add at least one complete, valid parameter to proceed.",
          class = "validation-status validation-warning"
        )
      }

      div(llm_indicator, validation_status, class = "validation-container")
    })

    ## output: checking of llm data ----
    # upstream: moduleState$show_validation, moduleState$validation_results
    # downstream: output$show_validation
    output$show_validation <- reactive({
      moduleState$show_validation
    })
    outputOptions(output, "show_validation", suspendWhenHidden = FALSE)

    output$parameter_validation_results <- renderText({
      if (!is.null(moduleState$validation_results)) {
        moduleState$validation_results$validation_text
      } else {
        "No validation results available."
      }
    })

    ## output: validated_data_display ----
    # upstream: moduleState$validated_data
    # downstream: UI data display
    output$validated_data_display <- renderText({
      if (isTruthy(moduleState$validated_data)) {
        # Format each parameter as a separate entry
        param_entries <- lapply(
          1:nrow(moduleState$validated_data),
          function(i) {
            param <- moduleState$validated_data[i, ]
            param_lines <- sapply(names(param), function(name) {
              value <- param[[name]]
              if (is.na(value) || is.null(value) || value == "") {
                paste0("  ", name, " = NA")
              } else if (is.character(value)) {
                paste0("  ", name, " = '", value, "'")
              } else {
                paste0("  ", name, " = ", as.character(value))
              }
            })
            paste0("Parameter ", i, ":\n", paste(param_lines, collapse = "\n"))
          }
        )

        paste(param_entries, collapse = "\n\n")
      } else {
        "# Parameters data will appear here when valid parameters are added"
      }
    })
  })
}

## To be copied in the UI ----
# mod_parameters_ui("parameters_1")

## To be copied in the server ----
# parameters_data <- mod_parameters_server("parameters_1")
