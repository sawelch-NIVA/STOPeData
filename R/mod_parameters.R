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
#' @importFrom shiny NS tagList selectInput actionButton
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
mod_parameters_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      card_header("Parameters Data Management"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Parameters Data Information",
            icon = bs_icon("info-circle"),
            "This module manages measured parameters (stressors, quality parameters, etc.). Select parameter type and name from dropdowns, then add to table. You can add existing parameters (with pre-filled chemical IDs) or create new ones. Edit fields directly in the table."
          )
        ),

        ## Parameter selection controls ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          selectInput(
            inputId = ns("parameter_type_select"),
            label = "Parameter Type",
            choices = c(
              "Stressor" = "Stressor",
              "Quality parameter" = "Quality parameter",
              "Normalization" = "Normalization",
              "Background" = "Background"
            ),
            selected = "Stressor",
            width = "100%"
          ),

          selectInput(
            inputId = ns("parameter_name_select"),
            label = "Parameter Name",
            choices = c("Select parameter type first..."),
            width = "100%"
          )
        ),

        ## Action buttons ----
        div(
          style = "margin: 15px 0;",
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
          input_task_button(
            id = ns("remove_selected"),
            label = "Remove Selected",
            icon = icon("trash"),
            class = "btn-danger",
            width = "200px"
          )
        ),

        ## Parameters table ----
        rHandsontableOutput(ns("parameters_table")),

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

#' Parameters Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
mod_parameters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      parameters_data = data.frame(),
      validated_data = NULL,
      is_valid = FALSE,
      next_param_id = 1,
      session_parameters = list() # Store user-added parameters
    )

    ## Dummy parameter database ----
    dummy_parameters <- list(
      "Stressor" = list(
        "Copper" = list(
          PARAMETER_NAME = "Copper",
          STRESSOR_TYPE_SUB = "Metal/metalloid",
          MEASURED_TYPE = "Concentration",
          CAS_RN = "7440-50-8",
          PUBCHEM_CID = "23978",
          INCHIKEY_SD = "RYGMFSIKBFXOCR-UHFFFAOYSA-N"
        ),
        "Lead" = list(
          PARAMETER_NAME = "Lead",
          STRESSOR_TYPE_SUB = "Metal/metalloid",
          MEASURED_TYPE = "Concentration",
          CAS_RN = "7439-92-1",
          PUBCHEM_CID = "5352425",
          INCHIKEY_SD = "WABPQHHGFIMREM-UHFFFAOYSA-N"
        ),
        "Benzene" = list(
          PARAMETER_NAME = "Benzene",
          STRESSOR_TYPE_SUB = "Organic chemical",
          MEASURED_TYPE = "Concentration",
          CAS_RN = "71-43-2",
          PUBCHEM_CID = "241",
          INCHIKEY_SD = "UHOVQNZJYSORNB-UHFFFAOYSA-N"
        )
      ),
      "Quality parameter" = list(
        "pH" = list(
          PARAMETER_NAME = "pH",
          STRESSOR_TYPE_SUB = "pH",
          MEASURED_TYPE = "Physical parameter",
          CAS_RN = NA,
          PUBCHEM_CID = NA,
          INCHIKEY_SD = NA
        ),
        "Dissolved oxygen" = list(
          PARAMETER_NAME = "Dissolved oxygen",
          STRESSOR_TYPE_SUB = "Dissolved oxygen",
          MEASURED_TYPE = "Concentration",
          CAS_RN = "7782-44-7",
          PUBCHEM_CID = "977",
          INCHIKEY_SD = "MYMOFIZGZYHOMD-UHFFFAOYSA-N"
        )
      ),
      "Normalization" = list(),
      "Background" = list()
    )

    ## Controlled vocabulary options ----
    stressor_types <- c(
      "Not relevant",
      "Stressor",
      "Quality parameter",
      "Normalization",
      "Background",
      "Other"
    )

    stressor_type_subs <- c(
      "Not reported",
      "Organic chemical",
      "Metal/metalloid",
      "Organometal",
      "Inorganic chemical",
      "Nanomaterial",
      "Microplastic",
      "Chemical mixture",
      "Temperature",
      "Radiation",
      "Pressure",
      "Sound",
      "Pathogen",
      "Toxin",
      "pH",
      "Dissolved oxygen",
      "Conductivity",
      "Salinity",
      "Turbidity",
      "Total organic carbon",
      "Nutrient",
      "Other"
    )

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
      data.frame(
        STRESSOR_TYPE = character(0),
        STRESSOR_TYPE_SUB = character(0),
        MEASURED_TYPE = character(0),
        PARAMETER_NAME = character(0),
        PARAMETER_NAME_SUB = character(0),
        INCHIKEY_SD = character(0),
        PUBCHEM_CID = character(0),
        CAS_RN = character(0),
        stringsAsFactors = FALSE
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
        required_fields <- c("STRESSOR_TYPE", "MEASURED_TYPE", "PARAMETER_NAME")

        for (i in 1:nrow(moduleState$parameters_data)) {
          for (field in required_fields) {
            value <- moduleState$parameters_data[i, field]
            if (is.na(value) || value == "") {
              return(paste("Row", i, "is missing required field:", field))
            }
          }

          # Conditional validation for STRESSOR_TYPE_SUB
          stressor_type <- moduleState$parameters_data[i, "STRESSOR_TYPE"]
          stressor_type_sub <- moduleState$parameters_data[
            i,
            "STRESSOR_TYPE_SUB"
          ]

          if (
            stressor_type %in%
              c("Stressor", "Quality parameter", "Normalization", "Background")
          ) {
            if (is.na(stressor_type_sub) || stressor_type_sub == "") {
              return(paste(
                "Row",
                i,
                "requires STRESSOR_TYPE_SUB when STRESSOR_TYPE is",
                stressor_type
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

    # 2. Helper functions ----

    ## Get available parameter names for selected type ----
    get_parameter_names <- function(param_type) {
      base_params <- names(dummy_parameters[[param_type]] %||% list())
      session_params <- names(
        moduleState$session_parameters[[param_type]] %||% list()
      )

      all_params <- c("-- New Parameter --", base_params, session_params)
      return(all_params)
    }

    ## Create parameter row from existing parameter ----
    create_existing_parameter <- function(param_type, param_name) {
      # Check dummy parameters first
      param_data <- dummy_parameters[[param_type]][[param_name]]

      # If not found, check session parameters
      if (is.null(param_data)) {
        param_data <- moduleState$session_parameters[[param_type]][[param_name]]
      }

      if (is.null(param_data)) {
        return(NULL)
      }

      data.frame(
        STRESSOR_TYPE = param_type,
        STRESSOR_TYPE_SUB = param_data$STRESSOR_TYPE_SUB %||% "",
        MEASURED_TYPE = param_data$MEASURED_TYPE %||% "Concentration",
        PARAMETER_NAME = param_data$PARAMETER_NAME %||% param_name,
        PARAMETER_NAME_SUB = param_data$PARAMETER_NAME_SUB %||% "",
        INCHIKEY_SD = param_data$INCHIKEY_SD %||% "",
        PUBCHEM_CID = param_data$PUBCHEM_CID %||% "",
        CAS_RN = param_data$CAS_RN %||% "",
        stringsAsFactors = FALSE
      )
    }

    ## Create new blank parameter row ----
    create_new_parameter <- function(param_type) {
      data.frame(
        STRESSOR_TYPE = param_type,
        STRESSOR_TYPE_SUB = "",
        MEASURED_TYPE = "Concentration",
        PARAMETER_NAME = "",
        PARAMETER_NAME_SUB = "",
        INCHIKEY_SD = "",
        PUBCHEM_CID = "",
        CAS_RN = "",
        stringsAsFactors = FALSE
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Update parameter name dropdown when type changes ----
    # upstream: input$parameter_type_select
    # downstream: input$parameter_name_select choices
    observe({
      param_type <- input$parameter_type_select

      if (isTruthy(param_type)) {
        available_names <- get_parameter_names(param_type)

        updateSelectInput(
          session,
          "parameter_name_select",
          choices = available_names,
          selected = if (length(available_names) > 1) available_names[1] else ""
        )
      }
    })

    ## observe: Add existing parameter ----
    # upstream: user clicks input$add_existing
    # downstream: moduleState$parameters_data
    observe({
      param_type <- input$parameter_type_select
      param_name <- input$parameter_name_select

      if (
        isTruthy(param_type) &&
          isTruthy(param_name) &&
          param_name != "-- New Parameter --"
      ) {
        new_param <- create_existing_parameter(param_type, param_name)

        if (!is.null(new_param)) {
          moduleState$parameters_data <- rbind(
            moduleState$parameters_data,
            new_param
          )
          showNotification(
            paste("Added parameter:", param_name),
            type = "message"
          )
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
        new_param <- create_new_parameter(param_type)
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

    ## observe: Remove selected rows ----
    # upstream: user clicks input$remove_selected
    # downstream: moduleState$parameters_data
    observe({
      if (!is.null(input$parameters_table)) {
        current_data <- hot_to_r(input$parameters_table)

        # Remove last row (simplified - proper row selection is complex in rhandsontable)
        if (nrow(current_data) > 0) {
          moduleState$parameters_data <- current_data[
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

    ## observe: Handle table changes ----
    # upstream: input$parameters_table changes
    # downstream: moduleState$parameters_data
    observe({
      if (!is.null(input$parameters_table)) {
        updated_data <- hot_to_r(input$parameters_table)

        # Store any new parameters that were created in the session
        for (i in 1:nrow(updated_data)) {
          param_type <- updated_data[i, "STRESSOR_TYPE"]
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
                STRESSOR_TYPE_SUB = updated_data[i, "STRESSOR_TYPE_SUB"],
                MEASURED_TYPE = updated_data[i, "MEASURED_TYPE"],
                PARAMETER_NAME_SUB = updated_data[i, "PARAMETER_NAME_SUB"],
                INCHIKEY_SD = updated_data[i, "INCHIKEY_SD"],
                PUBCHEM_CID = updated_data[i, "PUBCHEM_CID"],
                CAS_RN = updated_data[i, "CAS_RN"]
              )
            }
          }
        }

        moduleState$parameters_data <- updated_data
      }
    })

    ## observe: Check overall validation status ----
    # upstream: moduleState$parameters_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$parameters_data) > 0) {
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$parameters_data
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    })

    # 4. Outputs ----

    ## output: parameters_table ----
    # upstream: moduleState$parameters_data
    # downstream: UI table display
    output$parameters_table <- renderRHandsontable({
      if (nrow(moduleState$parameters_data) == 0) {
        # Show empty table structure
        rhandsontable(init_parameters_df(), width = "100%")
      } else {
        rhandsontable(moduleState$parameters_data, width = "100%") |>
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
          hot_col(
            "STRESSOR_TYPE",
            type = "dropdown",
            source = stressor_types,
            strict = TRUE
          ) |>
          hot_col(
            "STRESSOR_TYPE_SUB",
            type = "dropdown",
            source = stressor_type_subs,
            strict = TRUE
          ) |>
          hot_col(
            "MEASURED_TYPE",
            type = "dropdown",
            source = measured_types,
            strict = TRUE
          ) |>
          hot_col(c("INCHIKEY_SD", "PUBCHEM_CID", "CAS_RN"), type = "text")
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
            "All parameter data validated successfully.",
            nrow(moduleState$parameters_data),
            "parameter(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Add at least one complete, valid parameter to proceed. Edit fields directly in the table above.",
          class = "validation-status validation-warning"
        )
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
# mod_parameters_ui("parameters_1")

## To be copied in the server ----
# parameters_data <- mod_parameters_server("parameters_1")
