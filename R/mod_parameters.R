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
      full_screen = TRUE,
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
          )
        ),

        selectizeInput(
          inputId = ns("parameter_name_select"),
          label = "Parameter Name",
          choices = c("Select parameter type first..."),
          width = "100%",
          selected = "Formaldehyde",
          multiple = FALSE
        ),

        textAreaInput(
          inputId = ns("parameter_comment"),
          label = tooltip(
            list("Parameter Comments", bs_icon("info-circle-fill")),
            "Use this space to enter any potentially relevant or noteworthy comments or remarks about the measured parameter."
          ),
          placeholder = "Parameter notes (optional)",
          width = "100%",
          rows = 1
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

        ## LLM lookup validation ----
        conditionalPanel(
          condition = "output.llm_lookup_validation",
          ns = ns,
          accordion(
            open = TRUE,
            accordion_panel(
              title = "LLM extracted data validation",
              icon = bs_icon("cpu"),
              verbatimTextOutput(ns("parameter_llm_validation_results"))
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
      full_screen = TRUE,
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
#' @importFrom stats setNames
#' @export
mod_parameters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      parameters_data = initialise_parameters_tibble(),
      validated_data = initialise_parameters_tibble(),
      is_valid = FALSE,
      next_param_id = 1,
      session_parameters = list(),
      llm_validation_results = NULL,
      llm_lookup_validation = FALSE
    )

    ## Set initial empty data frame ----
    moduleState$parameters_data <- initialise_parameters_tibble()

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
        type_filtered_data <- dummy_parameters_vocabulary() |>
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
          dummy_parameters = dummy_parameters_vocabulary(),
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

    ## observe ~bindEvent(LLM data validates or updates): Load and validate LLM parameters ----
    # upstream: session$userData$reactiveValues$llmExtractionComplete
    # downstream: moduleState$parameters_data, moduleState$llm_lookup_validation, moduleState$llm_validation_results
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
          moduleState$llm_validation_results <- validation_result
          moduleState$llm_lookup_validation <- TRUE

          # Show notification based on validation
          if (validation_result$has_warnings) {
            showNotification(
              paste(
                "Populated",
                nrow(llm_parameters),
                "parameters (validation warning)"
              ),
              type = "warning"
            )
          } else {
            showNotification(
              paste(
                "Populated",
                nrow(llm_parameters),
                "parameters (validated))"
              ),
              type = "message"
            )
          }
        } else {
          showNotification(
            paste(
              "Populated",
              nrow(llm_parameters),
              "parameters. (validation not available)"
            ),
            type = "message"
          )
          moduleState$llm_lookup_validation <- FALSE
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
          dummy_parameters = dummy_parameters_vocabulary()
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
            # Check if this is a new parameter (not in dummy_parameters_vocabulary())
            if (
              is.null(dummy_parameters_vocabulary()[[param_type]][[param_name]])
            ) {
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
                ENTERED_BY = updated_data[i, "ENTERED_BY"],
                PARAMETER_COMMENT = updated_data[i, "PARAMETER_COMMENT"]
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
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- initialise_parameters_tibble()
      }
    })

    ## observer: receive data from session$userData$reactiveValues$parametersData (import) ----
    ## and update module data
    observe({
      moduleState$parameters_data <- session$userData$reactiveValues$parametersData
      print_dev("Assigned saved data to parameter moduleData.")
    }) |>
      bindEvent(
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    # 3. Outputs ----

    ## output: parameters_table ----
    # upstream: moduleState$parameters_data
    # downstream: UI table display
    output$parameters_table <- renderRHandsontable({
      if (nrow(moduleState$parameters_data) == 0) {
        # Show empty table structure
        rhandsontable(
          initialise_parameters_tibble(),
          stretchH = "all",
          height = 300,
          selectCallback = TRUE,
          width = NULL,
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
          stretchH = "all",
          height = 500,
          selectCallback = TRUE,
          width = NULL,
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
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
            source = parameter_types_vocabulary(),
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(
            "PARAMETER_TYPE_SUB",
            type = "dropdown",
            source = parameter_types_sub_vocabulary(),
            strict = TRUE
          ) |>
          hot_col(
            "MEASURED_TYPE",
            type = "dropdown",
            source = measured_types_vocabulary(),
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(c("INCHIKEY_SD", "PUBCHEM_CID", "CAS_RN"), type = "text") |>
          hot_col("ENTERED_BY", renderer = mandatory_highlight_text()) |>
          hot_col("PARAMETER_COMMENT", type = "text") |>
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
          class = "validation-status validation-llm",
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

    ## output: llm lookup validation ----
    # upstream: moduleState$llm_lookup_validation, moduleState$llm_validation_results
    # downstream: output$llm_lookup_validation
    output$llm_lookup_validation <- reactive({
      moduleState$llm_lookup_validation
    })
    outputOptions(output, "llm_lookup_validation", suspendWhenHidden = FALSE)

    output$parameter_llm_validation_results <- renderText({
      if (!is.null(moduleState$llm_validation_results)) {
        moduleState$llm_validation_results$validation_text
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
