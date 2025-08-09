# Compartments Import Module ----
# A Shiny module for environmental compartment combinations with form-then-table approach

#' Compartments UI Function ----
#'
#' @description A shiny Module for environmental compartment combinations data entry.
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
#' @export
mod_compartments_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      fill = TRUE,
      card_header("Environmental Compartments Data Management"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Environmental Compartments Information",
            icon = bs_icon("info-circle"),
            "This module manages environmental compartment combinations that will be sampled. Select an environmental compartment (e.g., Aquatic), then choose the appropriate sub-compartment (e.g., Freshwater) and measurement category. Each combination represents a different sampling context you'll use in your study."
          )
        ),

        ## Compartment selection form ----
        div(
          style = "padding: 15px; border-radius: 8px; margin: 15px 0;",
          h5("Add New Compartment Combination"),

          layout_column_wrap(
            width = "300px",
            fill = FALSE,
            fillable = FALSE,

            selectInput(
              inputId = ns("environ_compartment_select"),
              label = tooltip(
                list("Environmental Compartment", bs_icon("info-circle-fill")),
                "Which sphere does the sample come from?"
              ),
              choices = c(
                "Aquatic" = "Aquatic",
                "Atmospheric" = "Atmospheric",
                "Terrestrial" = "Terrestrial",
                "Biota" = "Biota"
              ),
              width = "100%",
              selected = "Aquatic"
            ),

            selectInput(
              inputId = ns("environ_compartment_sub_select"),
              label = tooltip(
                list(
                  "Environmental Sub-Compartment",
                  bs_icon("info-circle-fill")
                ),
                "Specific subset within the environmental compartment"
              ),
              choices = NULL,
              width = "100%",
              selected = "Marine/Salt Water"
            ),

            selectInput(
              inputId = ns("measured_category_select"),
              label = tooltip(
                list("Measured Category", bs_icon("info-circle-fill")),
                "Type of exposure measurement"
              ),
              choices = c(
                "External" = "External",
                "Internal" = "Internal",
                "Surface" = "Surface"
              ),
              selected = "External",
              width = "100%"
            )
          ),

          ## Add combination button ----
          div(
            style = "margin-top: 15px;",
            input_task_button(
              id = ns("add_combination"),
              label = "Add Combination",
              icon = icon("plus-circle"),
              class = "btn-success",
              width = "200px"
            )
          )
        ),

        ## Compartments table ----
        rHandsontableOutput(ns("compartments_table")),

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

#' Compartments Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
#' @export
mod_compartments_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      compartments_data = data.frame(),
      validated_data = NULL,
      is_valid = FALSE
    )

    ## Sub-compartment mappings ----
    sub_compartment_options <- list(
      "Aquatic" = c(
        "Freshwater" = "Freshwater",
        "Marine/Salt Water" = "Marine/Salt Water",
        "Brackish/Transitional Water" = "Brackish/Transitional Water",
        "Groundwater" = "Groundwater",
        "Wastewater" = "Wastewater",
        "Liquid Growth Medium" = "Liquid Growth Medium",
        "Rainwater" = "Rainwater",
        "Stormwater" = "Stormwater",
        "Leachate" = "Leachate"
      ),
      "Atmospheric" = c(
        "Indoor Air" = "Indoor Air",
        "Outdoor Air" = "Outdoor Air"
      ),
      "Terrestrial" = c(
        "Terrestrial Biological Residue" = "Terrestrial Biological Residue",
        "Soil H Horizon (Peat)" = "Soil H Horizon (Peat)",
        "Soil O Horizon (Organic)" = "Soil O Horizon (Organic)",
        "Soil A Horizon (Topsoil)" = "Soil A Horizon (Topsoil)",
        "Soil E Horizon (Mineral)" = "Soil E Horizon (Mineral)",
        "Soil S Horizon (Mineral)" = "Soil S Horizon (Mineral)",
        "Soil C Horizon (Parent Material)" = "Soil C Horizon (Parent Material)",
        "Soil R Horizon (Bedrock)" = "Soil R Horizon (Bedrock)"
      ),
      "Biota" = c(
        "Biota, Terrestrial" = "Biota, Terrestrial",
        "Biota, Aquatic" = "Biota, Aquatic",
        "Biota, Atmospheric" = "Biota, Atmospheric",
        "Biota, Other" = "Biota, Other"
      )
    )

    ## Controlled vocabulary for validation ----
    environ_compartments <- c(
      "Not relevant",
      "Not reported",
      "Aquatic",
      "Atmospheric",
      "Terrestrial",
      "Biota",
      "Other"
    )

    environ_compartment_subs <- c(
      "Not relevant",
      "Not reported",
      "Freshwater",
      "Marine/Salt Water",
      "Brackish/Transitional Water",
      "Groundwater",
      "Wastewater",
      "Liquid Growth Medium",
      "Rainwater",
      "Stormwater",
      "Leachate",
      "Aquatic Sediment",
      "Indoor Air",
      "Outdoor Air",
      "Terrestrial Biological Residue",
      "Soil H Horizon (Peat)",
      "Soil O Horizon (Organic)",
      "Soil A Horizon (Topsoil)",
      "Soil E Horizon (Mineral)",
      "Soil S Horizon (Mineral)",
      "Soil C Horizon (Parent Material)",
      "Soil R Horizon (Bedrock)",
      "Biota, Terrestrial",
      "Biota, Aquatic",
      "Biota, Atmospheric",
      "Biota, Other",
      "Other"
    )

    measured_categories <- c("Not relevant", "External", "Internal", "Surface")

    ## Initialize empty compartments data frame ----
    init_compartments_df <- function() {
      data.frame(
        ENVIRON_COMPARTMENT = character(0),
        ENVIRON_COMPARTMENT_SUB = character(0),
        MEASURED_CATEGORY = character(0),
        stringsAsFactors = FALSE
      )
    }

    ## Set initial empty data frame ----
    moduleState$compartments_data <- init_compartments_df()

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    iv$add_rule("compartments_table_validation", function(value) {
      if (nrow(moduleState$compartments_data) == 0) {
        "At least one compartment combination must be added"
      } else {
        # Check required fields
        required_fields <- c(
          "ENVIRON_COMPARTMENT",
          "ENVIRON_COMPARTMENT_SUB",
          "MEASURED_CATEGORY"
        )

        for (i in 1:nrow(moduleState$compartments_data)) {
          for (field in required_fields) {
            value <- moduleState$compartments_data[i, field]
            if (is.na(value) || value == "") {
              return(paste("Row", i, "is missing required field:", field))
            }
          }

          # Validate compartment/sub-compartment combinations
          compartment <- moduleState$compartments_data[i, "ENVIRON_COMPARTMENT"]
          sub_compartment <- moduleState$compartments_data[
            i,
            "ENVIRON_COMPARTMENT_SUB"
          ]

          # Check if sub-compartment is valid for the compartment
          if (compartment %in% names(sub_compartment_options)) {
            valid_subs <- sub_compartment_options[[compartment]]
            if (!sub_compartment %in% valid_subs) {
              return(paste(
                "Row",
                i,
                "has invalid sub-compartment",
                sub_compartment,
                "for compartment",
                compartment
              ))
            }
          }
        }
        NULL # All validations passed
      }
    })

    iv$enable()

    # 2. Helper functions ----

    ## Check if combination already exists ----
    combination_exists <- function(compartment, sub_compartment, category) {
      if (nrow(moduleState$compartments_data) == 0) {
        return(FALSE)
      }

      existing <- moduleState$compartments_data
      any(
        existing$ENVIRON_COMPARTMENT == compartment &
          existing$ENVIRON_COMPARTMENT_SUB == sub_compartment &
          existing$MEASURED_CATEGORY == category
      )
    }

    ## Create new compartment combination ----
    create_compartment_combination <- function(
      compartment,
      sub_compartment,
      category
    ) {
      data.frame(
        ENVIRON_COMPARTMENT = compartment,
        ENVIRON_COMPARTMENT_SUB = sub_compartment,
        MEASURED_CATEGORY = category,
        stringsAsFactors = FALSE
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Update sub-compartment dropdown when compartment changes ----
    # upstream: input$environ_compartment_select
    # downstream: input$environ_compartment_sub_select choices
    observe({
      compartment <- input$environ_compartment_select
      if (isTruthy(compartment) && compartment != "") {
        available_subs <- sub_compartment_options[[compartment]]
        if (!is.null(available_subs)) {
          choices <- c("Select sub-compartment..." = "", available_subs)
          updateSelectInput(
            session,
            "environ_compartment_sub_select",
            choices = choices
          )
        } else {
          updateSelectInput(
            session,
            "environ_compartment_sub_select",
            choices = c("No sub-compartments available" = "")
          )
        }
      } else {
        updateSelectInput(
          session,
          "environ_compartment_sub_select",
          choices = c("Select main compartment first..." = "")
        )
      }
    }) |>
      bindEvent(input$environ_compartment_select)

    ## observe: Add compartment combination ----
    # upstream: user clicks input$add_combination
    # downstream: moduleState$compartments_data
    observe({
      compartment <- input$environ_compartment_select
      sub_compartment <- input$environ_compartment_sub_select
      category <- input$measured_category_select

      if (
        isTruthy(compartment) &&
          compartment != "" &&
          isTruthy(sub_compartment) &&
          sub_compartment != "" &&
          isTruthy(category)
      ) {
        # Check if combination already exists
        if (combination_exists(compartment, sub_compartment, category)) {
          showNotification(
            "This combination already exists in the table",
            type = "warning"
          )
          return()
        }

        # Add new combination
        new_combination <- create_compartment_combination(
          compartment,
          sub_compartment,
          category
        )
        moduleState$compartments_data <- rbind(
          moduleState$compartments_data,
          new_combination
        )

        # Reset form
        updateSelectInput(session, "environ_compartment_select", selected = "")
        updateSelectInput(
          session,
          "environ_compartment_sub_select",
          choices = c("Select main compartment first..." = "")
        )
        updateSelectInput(
          session,
          "measured_category_select",
          selected = "External"
        )

        showNotification(
          paste(
            "Added combination:",
            compartment,
            "→",
            sub_compartment,
            "→",
            category
          ),
          type = "message"
        )
      } else {
        showNotification(
          "Please select all required fields before adding",
          type = "warning"
        )
      }
    }) |>
      bindEvent(input$add_combination)

    ## observe: Handle table changes ----
    # upstream: input$compartments_table changes
    # downstream: moduleState$compartments_data
    observe({
      if (!is.null(input$compartments_table)) {
        updated_data <- hot_to_r(input$compartments_table)
        moduleState$compartments_data <- updated_data
      }
    })

    ## observe: Check overall validation status ----
    # upstream: moduleState$compartments_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$compartments_data) > 0) {
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$compartments_data

        session$userData$reactiveValues$compartmentsData <- moduleState$validated_data
        print_dev(glue(
          "moduleState$is_valid: {moduleState$is_valid},
                       session$userData$reactiveValues$compartmentsData: {session$userData$reactiveValues$sitesData}"
        ))
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    })

    ## observe: Load from LLM data when available ----
    # upstream: session$userData$reactiveValues$compartmentsDataLLM
    # downstream: moduleState$compartments_data
    observe({
      llm_compartments <- session$userData$reactiveValues$compartmentsDataLLM
      if (
        !is.null(llm_compartments) &&
          nrow(llm_compartments) > 0 &&
          session$userData$reactiveValues$llmExtractionComplete
      ) {
        # Replace current compartments data with LLM data
        moduleState$compartments_data <- llm_compartments

        showNotification(
          paste(
            "Loaded",
            nrow(llm_compartments),
            "compartments from LLM extraction. Verify compartment combinations."
          ),
          type = "message"
        )
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$compartmentsDataLLM,
        session$userData$reactiveValues$llmExtractionComplete,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    # 4. Outputs ----

    ## output: compartments_table ----
    # upstream: moduleState$compartments_data
    # downstream: UI table display
    output$compartments_table <- renderRHandsontable({
      if (nrow(moduleState$compartments_data) == 0) {
        # Show empty table structure
        rhandsontable(
          init_compartments_df(),
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
          moduleState$compartments_data,
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
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
            "All compartment data validated successfully.",
            nrow(moduleState$compartments_data),
            "combination(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Add at least one valid compartment combination to proceed. Use the form above to add combinations.",
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: validated_data_display ----
    # upstream: moduleState$validated_data
    # downstream: UI data display
    output$validated_data_display <- renderText({
      if (isTruthy(moduleState$validated_data)) {
        # Format each combination as a separate entry
        combo_entries <- lapply(
          1:nrow(moduleState$validated_data),
          function(i) {
            combo <- moduleState$validated_data[i, ]
            combo_lines <- sapply(names(combo), function(name) {
              value <- combo[[name]]
              if (is.na(value) || is.null(value) || value == "") {
                paste0("  ", name, " = NA")
              } else if (is.character(value)) {
                paste0("  ", name, " = '", value, "'")
              } else {
                paste0("  ", name, " = ", as.character(value))
              }
            })
            paste0(
              "Combination ",
              i,
              ":\n",
              paste(combo_lines, collapse = "\n")
            )
          }
        )

        paste(combo_entries, collapse = "\n\n")
      } else {
        "# Compartment combinations will appear here when valid combinations are added"
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
# mod_compartments_ui("compartments_1")

## To be copied in the server ----
# compartments_data <- mod_compartments_server("compartments_1")
