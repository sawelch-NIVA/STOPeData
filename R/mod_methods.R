# Methods Import Module ----
# A Shiny module for analytical and Sampling Protocols with categorized protocol selection

#' Methods UI Function ----
#'
#' @description A shiny Module for methods data entry with categorized protocol selection.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput actionButton
#' @importFrom bslib card  card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @export
mod_methods_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_methods.md"),

        ## Method selection form ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,
          margin = "-10px",

          selectInput(
            inputId = ns("protocol_category_select"),
            label = tooltip(
              list("Protocol Category", bs_icon("info-circle-fill")),
              "Type of protocol or method being used"
            ),
            choices = c(
              "Sampling Protocol" = "Sampling Protocol",
              "Fractionation Protocol" = "Fractionation Protocol",
              "Extraction Protocol" = "Extraction Protocol",
              "Analytical Protocol" = "Analytical Protocol"
            ),
            selected = "Sampling Protocol",
            width = "100%"
          ),

          selectInput(
            inputId = ns("protocol_name_select"),
            label = tooltip(
              list("Protocol Name", bs_icon("info-circle-fill")),
              "Specific method or protocol within the category"
            ),
            choices = c("Select category first..." = ""),
            width = "100%",
            multiple = TRUE
          )
        ),

        ## Add method button and validation status ----
        div(
          style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 15px 0;",

          input_task_button(
            id = ns("add_method"),
            label = "Add Method",
            icon = icon("plus-circle"),
            class = "btn-success",
            width = "200px"
          ),

          ### Validation status ----
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

    ## Methods table card ----
    card(
      div(
        rHandsontableOutput(ns("methods_table")),
        style = "margin-bottom: 10px;"
      )
    )
  )
}

#' Methods Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
#' @importFrom arrow read_parquet
#' @importFrom tibble tibble deframe
#' @importFrom dplyr filter select
#' @importFrom purrr is_empty
#' @export
mod_methods_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      methods_data = data.frame(),
      validated_data = NULL,
      is_valid = FALSE
    )

    ## Protocol options by category ----
    protocol_options <- read_parquet(
      "inst/data/clean/inst-claude-2025-08-05-methods.parquet"
    )

    ## Controlled vocabulary for validation ----
    # ! FORMAT-BASED
    protocol_categories <- c(
      "Sampling Protocol",
      "Fractionation Protocol",
      "Extraction Protocol",
      "Analytical Protocol"
    )

    all_protocol_names <- unique(protocol_options$Short_Name)

    ## Initialize empty methods data frame ----
    # ! FORMAT-BASED
    init_methods_df <- function() {
      tibble(
        PROTOCOL_CATEGORY = character(0),
        PROTOCOL_NAME = character(0),
        PROTOCOL_COMMENT = character(0)
      )
    }

    ## Set initial empty data frame ----
    moduleState$methods_data <- init_methods_df()

    ## InputValidator for table-level validation ----
    # ! FORMAT-BASED
    iv <- InputValidator$new()
    iv$add_rule("methods_table_validation", function(value) {
      if (nrow(moduleState$methods_data) == 0) {
        "At least one method must be added"
      } else {
        # Check required fields
        required_fields <- c("PROTOCOL_CATEGORY", "PROTOCOL_NAME")

        for (i in 1:nrow(moduleState$methods_data)) {
          for (field in required_fields) {
            value <- moduleState$methods_data[i, field]
            if (is_empty(value) || value == "") {
              return(paste("Row", i, "is missing required field:", field))
            }
          }

          # Validate category/protocol combinations
          category <- moduleState$methods_data[i, "PROTOCOL_CATEGORY"]
          protocol <- moduleState$methods_data[i, "PROTOCOL_NAME"]

          # Check if protocol is valid for the category
          if (category %in% names(protocol_options)) {
            valid_protocols <- protocol_options[[category]]
            if (!protocol %in% valid_protocols) {
              return(paste(
                "Row",
                i,
                "has invalid protocol",
                protocol,
                "for category",
                category
              ))
            }
          }
        }
        NULL # All validations passed
      }
    })

    iv$enable()

    # 2. Helper functions ----

    ## Create new method entry ----
    create_method_entry <- function(category, protocol) {
      tibble(
        PROTOCOL_CATEGORY = category,
        PROTOCOL_NAME = protocol,
        PROTOCOL_COMMENT = NA_character_
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Update protocol name dropdown when category changes ----
    # upstream: input$protocol_category_select
    # downstream: input$protocol_name_select choices
    observe({
      category <- input$protocol_category_select

      if (isTruthy(category)) {
        available_protocols <- protocol_options |>
          filter(Protocol_Type == category) |>
          select(Long_Name, Short_Name) |>
          deframe()

        if (!is.null(available_protocols)) {
          choices <- c("Select protocol..." = "", available_protocols)
          updateSelectInput(
            session,
            "protocol_name_select",
            choices = choices
          )
        } else {
          updateSelectInput(
            session,
            "protocol_name_select",
            choices = c("No protocols available" = "")
          )
        }
      } else {
        updateSelectInput(
          session,
          "protocol_name_select",
          choices = c("Select category first..." = "")
        )
      }
    }) |>
      bindEvent(
        input$protocol_category_select,
        ignoreInit = FALSE,
        ignoreNULL = FALSE
      )

    ## observe ~bindEvent(add_method): Add method to basket ----
    # upstream: user clicks input$add_method
    # downstream: moduleState$methods_data
    observe({
      category <- input$protocol_category_select
      protocols <- input$protocol_name_select

      if (
        isTruthy(category) &&
          isTruthy(protocols) &&
          length(protocols) > 0 &&
          all(protocols != "")
      ) {
        # Use lapply instead of sapply to avoid issues with the assignment
        lapply(protocols, function(protocol) {
          # Add new method (allow duplicates as user might need multiple instances)
          new_method <- create_method_entry(category, protocol)
          moduleState$methods_data <<- rbind(
            # Use <<- for assignment in lapply
            moduleState$methods_data,
            new_method
          )
          session$userData$reactiveValues$methodsData <- moduleState$methods_data
        })

        # Reset form
        updateSelectInput(
          session,
          "protocol_name_select",
          selected = ""
        )

        showNotification(
          paste(
            "Added",
            length(protocols),
            "method(s):",
            category,
            "→",
            paste(protocols, collapse = ", ")
          ),
          type = "message"
        )
      } else {
        showNotification(
          "Please select both category and protocol(s) before adding",
          type = "warning"
        )
      }
    }) |>
      bindEvent(input$add_method)

    ## observe: Handle table changes ----
    # upstream: input$methods_table changes
    # downstream: moduleState$methods_data
    observe({
      if (!is.null(input$methods_table)) {
        updated_data <- hot_to_r(input$methods_table)
        moduleState$methods_data <- updated_data
        session$userData$reactiveValues$methodsData <- moduleState$methods_data
      }
    }) |>
      bindEvent(input$methods_table)

    ## observe: Check overall validation status ----
    # upstream: moduleState$methods_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$methods_data) > 0) {
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$methods_data
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    })

    ## observe ~ bindEvent: Load methods data from LLM extraction ----
    observe({
      if (!is.null(session$userData$reactiveValues$methodsDataLLM)) {
        llm_methods_data <- session$userData$reactiveValues$methodsDataLLM

        # Replace existing data
        moduleState$methods_data <- llm_methods_data

        print_dev(glue(
          "mod_methods loaded {nrow(llm_methods_data)} entries from LLM"
        ))
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$methodsDataLLM,
        ignoreNULL = TRUE
      )

    # 4. Outputs ----

    ## output: methods_table ----
    # upstream: moduleState$methods_data
    # downstream: UI table display
    output$methods_table <- renderRHandsontable({
      if (nrow(moduleState$methods_data) == 0) {
        # Show empty table structure
        rhandsontable(
          init_methods_df(),
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
          moduleState$methods_data,
          selectCallback = TRUE,
          stretchH = "all",
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
          hot_col(
            "PROTOCOL_CATEGORY",
            type = "dropdown",
            source = protocol_categories,
            strict = TRUE
          ) |>
          hot_col(
            "PROTOCOL_NAME",
            type = "dropdown",
            source = all_protocol_names,
            strict = TRUE
          ) |>
          hot_col(
            "PROTOCOL_COMMENT",
            type = "text"
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
            "All methods data validated successfully.",
            nrow(moduleState$methods_data),
            "method(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Add at least one valid method to proceed. Use the form above to add methods.",
          class = "validation-status validation-warning"
        )
      }

      div(llm_indicator, validation_status, class = "validation-container")
    })

    ## output: validated_data_display ----
    # upstream: moduleState$validated_data
    # downstream: UI data display
    output$validated_data_display <- renderText({
      if (isTruthy(moduleState$validated_data)) {
        # Format each method as a separate entry
        method_entries <- lapply(
          1:nrow(moduleState$validated_data),
          function(i) {
            method <- moduleState$validated_data[i, ]
            method_lines <- sapply(names(method), function(name) {
              value <- method[[name]]
              if (is.na(value) || is.null(value) || value == "") {
                paste0("  ", name, " = NA")
              } else if (is.character(value)) {
                paste0("  ", name, " = '", value, "'")
              } else {
                paste0("  ", name, " = ", as.character(value))
              }
            })
            paste0("Method ", i, ":\n", paste(method_lines, collapse = "\n"))
          }
        )

        paste(method_entries, collapse = "\n\n")
      } else {
        "# Methods data will appear here when valid methods are added"
      }
    })
  })
}

## To be copied in the UI ----
# mod_methods_ui("methods_1")

## To be copied in the server ----
# methods_data <- mod_methods_server("methods_1")
