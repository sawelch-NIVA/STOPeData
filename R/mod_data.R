# Data Entry Module ----
# A Shiny module for consolidating setup data and entering measurement values

#' Data UI Function ----
#'
#' @description A shiny Module for measurement data entry with consolidated sample-parameter combinations.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_body accordion accordion_panel
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @export
mod_data_ui <- function(id) {
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
        info_accordion(content_file = "inst/app/www/md/intro_data.md"),

        ## Dynamic validation status accordion ----
        uiOutput(ns("validation_accordion_ui")),

        ## Data entry controls ----
        div(
          style = "margin: 20px 0 0 0;",
          h5("Measurement Data Entry"),
          p(
            "Complete measurement data for all sample-parameter combinations below. All fields marked as required must be filled.",
            class = "text-muted"
          )
        ),

        ## Raw data accordion ----
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Preview Complete Dataset",
            icon = bs_icon("eye"),
            verbatimTextOutput(ns("complete_data_preview"))
          )
        )
      )
    ),

    # Measurement data table card ----
    card(
      full_screen = TRUE,
      ## Measurement Data Validation status ----
      div(
        style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 15px 0;",

        ### Validation status ----
        uiOutput(ns("validation_reporter"))
      ),
      # style = "overflow: clip;",
      div(
        rHandsontableOutput(
          ns("measurement_table"),
          width = "100%",
          height = "100%"
        ),
        style = "margin-bottom: 10px; margin-left: 10px;"
      )
    )
  )
}

#' Data Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification
#' @importFrom rhandsontable renderRHandsontable hot_cols rhandsontable hot_to_r hot_col hot_context_menu hot_cols
#' @importFrom shinyjs enable disable
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom dplyr cross_join mutate select rename pull filter relocate
#' @importFrom tibble tibble
#' @importFrom utils capture.output head
#' @importFrom purrr is_empty
#' @export
mod_data_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      all_modules_valid = FALSE,
      data_entry_ready = FALSE,
      measurement_combinations = data.frame(),
      complete_dataset = NULL,
      is_valid = FALSE,
      validation_message = ""
    )

    ## Controlled vocabulary options ----
    measured_flags <- c("", "< LOQ", "< LOD")

    measured_units <- parameter_units("MEASURED_UNIT")

    ## InputValidator for measurement data validation ----
    iv <- InputValidator$new()

    # Rule 1: Check if data entry is ready
    iv$add_rule("measurement_table_validation", function(value) {
      if (!moduleState$data_entry_ready) {
        moduleState$validation_message <<- "Complete all setup modules before entering measurement data"
        return("Complete all setup modules before entering measurement data")
      }
    })

    # Rule 2: Check if measurement combinations exist
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(moduleState$measurement_combinations) == 0) {
        moduleState$validation_message <<- "No sample-parameter combinations available"
        return("No sample-parameter combinations available")
      }
    })

    # Rule 3: Check for missing required fields
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(moduleState$measurement_combinations) > 0) {
        required_fields <- c("MEASURED_UNIT")

        for (i in 1:nrow(moduleState$measurement_combinations)) {
          for (field in required_fields) {
            value <- moduleState$measurement_combinations[i, field]
            if (is.na(value) || value == "") {
              message <- paste("Row", i, "is missing required field:", field)
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 4: Check MEASURED_VALUE when no flag is set
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(moduleState$measurement_combinations) > 0) {
        for (i in 1:nrow(moduleState$measurement_combinations)) {
          measured_flag <- moduleState$measurement_combinations[
            i,
            "MEASURED_FLAG"
          ]
          measured_value <- moduleState$measurement_combinations[
            i,
            "MEASURED_VALUE"
          ]

          if (is.na(measured_flag) || measured_flag == "") {
            if (is.na(measured_value) || measured_value == "") {
              message <- paste(
                "Row",
                i,
                "requires MEASURED_VALUE when no flag is set"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 5: Check LOQ_VALUE when flag is <LOQ
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(moduleState$measurement_combinations) > 0) {
        for (i in 1:nrow(moduleState$measurement_combinations)) {
          measured_flag <- moduleState$measurement_combinations[
            i,
            "MEASURED_FLAG"
          ]
          loq_value <- moduleState$measurement_combinations[i, "LOQ_VALUE"]

          if (!is.na(measured_flag) && measured_flag == "< LOQ") {
            if (is.na(loq_value) || loq_value == "") {
              message <- paste(
                "Row",
                i,
                "requires LOQ_VALUE when flag is '< LOQ'"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 6: Check LOD_VALUE when flag is < LOD
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(moduleState$measurement_combinations) > 0) {
        for (i in 1:nrow(moduleState$measurement_combinations)) {
          measured_flag <- moduleState$measurement_combinations[
            i,
            "MEASURED_FLAG"
          ]
          lod_value <- moduleState$measurement_combinations[i, "LOD_VALUE"]

          if (!is.na(measured_flag) && measured_flag == "< LOD") {
            if (is.na(lod_value) || lod_value == "") {
              message <- paste(
                "Row",
                i,
                "requires LOD_VALUE when flag is '< LOD'"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 7: Validate numeric values are positive
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(moduleState$measurement_combinations) > 0) {
        numeric_fields <- c(
          "MEASURED_VALUE",
          "LOQ_VALUE",
          "LOD_VALUE",
          "MEASURED_SD"
        )

        for (i in 1:nrow(moduleState$measurement_combinations)) {
          for (field in numeric_fields) {
            field_value <- moduleState$measurement_combinations[i, field]
            if (
              !is.na(field_value) &&
                field_value != "" &&
                as.numeric(field_value) < 0
            ) {
              message <- paste(
                "Row",
                i,
                "has invalid",
                field,
                "(must be positive)"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 8: Check that all protocol fields are filled
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(moduleState$measurement_combinations) > 0) {
        protocol_fields <- c(
          "SAMPLING_PROTOCOL",
          "FRACTIONATION_PROTOCOL",
          "EXTRACTION_PROTOCOL",
          "ANALYTICAL_PROTOCOL"
        )

        for (i in 1:nrow(moduleState$measurement_combinations)) {
          for (field in protocol_fields) {
            field_value <- moduleState$measurement_combinations[i, field]
            if (is.na(field_value) || field_value == "") {
              message <- paste(
                "Row",
                i,
                "is missing required protocol field:",
                field
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    iv$enable()

    # 2. Helper functions ----

    ## Check if all required modules are validated and return data of >1 row ----
    check_all_modules_valid <- function() {
      required_data <- list(
        campaign = session$userData$reactiveValues$campaignData,
        references = session$userData$reactiveValues$referenceData,
        sites = session$userData$reactiveValues$sitesData,
        parameters = session$userData$reactiveValues$parametersData,
        compartments = session$userData$reactiveValues$compartmentsData,
        methods = session$userData$reactiveValues$methodsData,
        samples = session$userData$reactiveValues$samplesDataWithBiota %|truthy|%
          session$userData$reactiveValues$samplesData
      )

      all(sapply(required_data, function(x) {
        !is.null(x) && nrow(x) > 0
      }))
    }

    ## Get validation status for each module ----
    get_module_status <- function() {
      modules <- list(
        Campaign = session$userData$reactiveValues$campaignData,
        References = session$userData$reactiveValues$referenceData,
        Sites = session$userData$reactiveValues$sitesData,
        Parameters = session$userData$reactiveValues$parametersData,
        Compartments = session$userData$reactiveValues$compartmentsData,
        Methods = session$userData$reactiveValues$methodsData,
        Samples = session$userData$reactiveValues$samplesData,
        Biota = session$userData$reactiveValues$biotaValidated
      )

      status_list <- lapply(names(modules), function(name) {
        data <- modules[[name]]
        status <- if (isTruthy(data) && nrow(data) > 0 || !is_empty(data)) {
          "Validated"
        } else {
          "Attention required"
        }
        count <- if (isTruthy(data)) {
          nrow(data)
        } else {
          0
        }

        list(module = name, status = status, count = count)
      })

      return(status_list)
    }

    ## Create sample-parameter combinations for measurement entry ----
    # ! FORMAT-BASED
    create_measurement_combinations <- function() {
      # Get all validated data
      samples_data <- session$userData$reactiveValues$samplesDataWithBiota %|truthy|%
        session$userData$reactiveValues$samplesData %|truthy|%
        data.frame()
      parameters_data <- session$userData$reactiveValues$parametersData
      campaign_data <- session$userData$reactiveValues$campaignData
      reference_data <- session$userData$reactiveValues$referenceData

      if (is.null(samples_data) || is.null(parameters_data)) {
        return(data.frame())
      }

      # browser()
      # Create cartesian product of samples and parameters
      combinations <- cross_join(
        samples_data |>
          select(
            SAMPLE_ID,
            SITE_CODE,
            SAMPLING_DATE,
            ENVIRON_COMPARTMENT,
            ENVIRON_COMPARTMENT_SUB,
            REP
          ),
        parameters_data |> select(PARAMETER_NAME, PARAMETER_TYPE, MEASURED_TYPE)
      )

      # Add campaign and reference info
      combinations <- combinations |>
        mutate(
          # Add measurement fields with empty defaults
          MEASURED_FLAG = "",
          MEASURED_VALUE = NA,
          MEASURED_SD = NA,
          MEASURED_UNIT = "mg/L", # Default unit
          LOQ_VALUE = NA,
          LOQ_UNIT = "mg/L", # Should match MEASURED_UNIT
          LOD_VALUE = NA,
          LOD_UNIT = "mg/L", # Should match MEASURED_UNIT

          # Add method info
          SAMPLING_PROTOCOL = "",
          FRACTIONATION_PROTOCOL = "",
          EXTRACTION_PROTOCOL = "",
          ANALYTICAL_PROTOCOL = "",

          REFERENCE_ID = reference_data$REFERENCE_ID,
        ) |>
        relocate(SAMPLE_ID, ENVIRON_COMPARTMENT, .after = REFERENCE_ID)

      return(combinations)
    }

    ## Initialize measurement combinations data frame ----
    # ! FORMAT-BASED
    init_measurement_df <- function() {
      tibble(
        SITE_CODE = character(0),
        PARAMETER_NAME = character(0),
        SAMPLING_DATE = character(0),
        ENVIRON_COMPARTMENT_SUB = character(0),
        REP = integer(0),
        MEASURED_FLAG = character(0),
        MEASURED_VALUE = numeric(0),
        MEASURED_SD = numeric(0),
        MEASURED_UNIT = character(0),
        LOQ_VALUE = numeric(0),
        LOQ_UNIT = character(0),
        LOD_VALUE = numeric(0),
        LOD_UNIT = character(0),
        SAMPLING_PROTOCOL = character(0),
        EXTRACTION_PROTOCOL = character(0),
        FRACTIONATION_PROTOCOL = character(0),
        ANALYTICAL_PROTOCOL = character(0),
        REFERENCE_ID = character(0),
        SAMPLE_ID = character(0),
        ENVIRON_COMPARTMENT = character(0)
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Check upstream modules validation status continuously ----
    # upstream: all session$userData$reactiveValues
    # downstream: moduleState$all_modules_valid, moduleState$data_entry_ready
    observe({
      moduleState$all_modules_valid <- check_all_modules_valid()

      if (moduleState$all_modules_valid) {
        moduleState$data_entry_ready <- TRUE
        # Create measurement combinations when ready
        moduleState$measurement_combinations <- create_measurement_combinations()

        print_dev(glue(
          "mod_data: All modules validated, created {nrow(moduleState$measurement_combinations)} measurement combinations"
        ))
      } else {
        moduleState$data_entry_ready <- FALSE
        moduleState$measurement_combinations <- init_measurement_df()

        print_dev("mod_data: Some modules pending, data entry disabled")
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$campaignData,
        session$userData$reactiveValues$referenceData,
        session$userData$reactiveValues$sitesData,
        session$userData$reactiveValues$parametersData,
        session$userData$reactiveValues$compartmentsData,
        session$userData$reactiveValues$methodsData,
        session$userData$reactiveValues$samplesData,
        session$userData$reactiveValues$biotaValidated,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    ## observe: Handle table changes ----
    # upstream: input$measurement_table changes
    # downstream: moduleState$measurement_combinations
    # ! FORMAT-BASED
    observe({
      if (!is.null(input$measurement_table) && moduleState$data_entry_ready) {
        updated_data <- hot_to_r(input$measurement_table)
        moduleState$measurement_combinations <- updated_data

        # Update LOQ_UNIT and LOD_UNIT to match MEASURED_UNIT
        for (i in 1:nrow(moduleState$measurement_combinations)) {
          measured_unit <- moduleState$measurement_combinations[
            i,
            "MEASURED_UNIT"
          ]
          if (!is.na(measured_unit) && measured_unit != "") {
            moduleState$measurement_combinations[i, "LOQ_UNIT"] <- measured_unit
            moduleState$measurement_combinations[i, "LOD_UNIT"] <- measured_unit
          }
        }
      }
    }) |>
      bindEvent(input$measurement_table)

    ## observe: Check measurement data validation and save to session ----
    # upstream: moduleState$measurement_combinations, iv
    # downstream: moduleState$is_valid, moduleState$complete_dataset, session$userData
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$measurement_combinations) > 0) {
        moduleState$is_valid <- TRUE

        # Create complete dataset by merging all components
        complete_data <- moduleState$measurement_combinations

        # Add campaign info
        campaign_data <- session$userData$reactiveValues$campaignData
        for (col in names(campaign_data)) {
          if (!col %in% names(complete_data)) {
            complete_data[[col]] <- campaign_data[[col]]
          }
        }

        moduleState$complete_dataset <- complete_data
        session$userData$reactiveValues$dataData <- moduleState$complete_dataset

        print_dev(glue(
          "mod_data: Data validated and saved - {nrow(moduleState$complete_dataset)} complete records"
        ))
      } else {
        moduleState$is_valid <- FALSE
        moduleState$complete_dataset <- NULL
        session$userData$reactiveValues$dataData <- NULL

        print_dev("mod_data: Data validation failed")
      }
    }) |>
      bindEvent(iv, input$measurement_table, ignoreInit = TRUE)

    ## observe: Update method dropdown options whenever methods change----
    # Define some methods for the UI to find so it doesn't crash
    sampling_methods <- analytical_methods <- extraction_methods <- fractionation_methods <- c(
      ""
    )

    observe({
      # Get available methods from mod_methods
      available_methods <- session$userData$reactiveValues$methodsData

      if (nrow(available_methods) > 0) {
        analytical_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Analytical Protocol") |>
          pull(PROTOCOL_NAME)

        # Ensure at least 2 elements
        if (length(analytical_methods) == 0) {
          analytical_methods <<- c("", "")
        } else if (length(analytical_methods) == 1) {
          analytical_methods <<- c("", analytical_methods)
        }

        sampling_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Sampling Protocol") |>
          pull(PROTOCOL_NAME)

        if (length(sampling_methods) == 0) {
          sampling_methods <<- c("", "")
        } else if (length(sampling_methods) == 1) {
          sampling_methods <<- c("", sampling_methods)
        }

        extraction_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Extraction Protocol") |>
          pull(PROTOCOL_NAME)

        if (length(extraction_methods) == 0) {
          extraction_methods <<- c("", "")
        } else if (length(extraction_methods) == 1) {
          extraction_methods <<- c("", extraction_methods)
        }

        fractionation_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Fractionation Protocol") |>
          pull(PROTOCOL_NAME)

        if (length(fractionation_methods) == 0) {
          fractionation_methods <<- c("", "")
        } else if (length(fractionation_methods) == 1) {
          fractionation_methods <<- c("", fractionation_methods)
        }
      } else {
        # No methods data available - set all to 2-element vectors
        analytical_methods <<- c("", "")
        sampling_methods <<- c("", "")
        extraction_methods <<- c("", "")
        fractionation_methods <<- c("", "")
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$methodsData,
        ignoreInit = TRUE
      )

    ## observe: Navigate to Campaign when go_to_campaign clicked ----
    # Upstream: input$go_to_campaign button click
    # Downstream: Updates main navbar to campaign tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "01-campaign"
      )
    }) |>
      bindEvent(input$go_to_campaign)

    ## observe: Navigate to References when go_to_references clicked ----
    # Upstream: input$go_to_references button click
    # Downstream: Updates main navbar to references tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "02-references"
      )
    }) |>
      bindEvent(input$go_to_references)

    ## observe: Navigate to Sites when go_to_sites clicked ----
    # Upstream: input$go_to_sites button click
    # Downstream: Updates main navbar to sites tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "03-sites"
      )
    }) |>
      bindEvent(input$go_to_sites)

    ## observe: Navigate to Parameters when go_to_parameters clicked ----
    # Upstream: input$go_to_parameters button click
    # Downstream: Updates main navbar to parameters tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "04-parameters"
      )
    }) |>
      bindEvent(input$go_to_parameters)

    ## observe: Navigate to Compartments when go_to_compartments clicked ----
    # Upstream: input$go_to_compartments button click
    # Downstream: Updates main navbar to compartments tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "05-compartments"
      )
    }) |>
      bindEvent(input$go_to_compartments)

    ## observe: Navigate to Methods when go_to_methods clicked ----
    # Upstream: input$go_to_methods button click
    # Downstream: Updates main navbar to methods tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "06-methods"
      )
    }) |>
      bindEvent(input$go_to_methods)

    ## observe: Navigate to Samples when go_to_samples clicked ----
    # Upstream: input$go_to_samples button click
    # Downstream: Updates main navbar to samples tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "07-samples"
      )
    }) |>
      bindEvent(input$go_to_samples)

    ## observe: Navigate to Biota when go_to_biota clicked ----
    # Upstream: input$go_to_biota button click
    # Downstream: Updates main navbar to biota tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "08-biota"
      )
    }) |>
      bindEvent(input$go_to_biota)

    # 4. Outputs ----

    ## output: validation_accordion_ui ----
    # upstream: moduleState$is_valid, moduleState$data_entry_ready, moduleState$all_modules_valid
    # downstream: UI validation accordion with dynamic styling
    output$validation_accordion_ui <- renderUI({
      # Determine validation state
      if (moduleState$all_modules_valid) {
        accordion_class <- "accordion-validation-valid"
        accordion_text <-
          paste0(
            "All modules validated - ",
            nrow(moduleState$measurement_combinations),
            " samples ready for data"
          )
        icon_name <- "check-circle"
        style_css <- "
          .accordion-validation-valid .accordion-button {
            background-color: var(--color-success) !important;
            color: white !important;
          }
          .accordion-validation-valid .accordion-button:not(.collapsed) {
            background-color: #198754 !important;
            color: white !important;
          }
        "
      } else {
        accordion_class <- "accordion-validation-warning"
        accordion_text <- "All modules must be valid to add measurements"
        icon_name <- "exclamation-triangle"
        style_css <- "
          .accordion-validation-warning .accordion-button {
            background-color: var(--color-warning) !important;
            color: var(--text-dark) !important;
          }
          .accordion-validation-warning .accordion-button:not(.collapsed) {
            background-color: #e0a800 !important;
            color: var(--text-dark) !important;
          }
        "
      }

      tagList(
        # Dynamic CSS for current state
        tags$head(
          tags$style(HTML(paste0(
            style_css,
            "
            .accordion-body {
              margin: 0px;
            }
            "
          )))
        ),

        # Dynamic accordion
        div(
          class = accordion_class,
          accordion(
            id = ns("validation_accordion"),
            accordion_panel(
              value = "validation_accordion_panel",
              style = "margin: 20px 0;",
              title = accordion_text,
              icon = bs_icon(icon_name),
              uiOutput(ns("validation_overview"))
            )
          )
        )
      )
    })

    ## output: validation_overview ----
    # upstream: session data
    # downstream: UI validation status display
    output$validation_overview <- renderUI({
      status_list <- get_module_status() # when this gets called with sampleS, we get a crash?

      # Create all module elements ----
      module_elements <- lapply(1:8, function(i) {
        item <- status_list[[i]]
        valid <- grepl("Validated", item$status)
        icon <- if (valid) "clipboard2-check" else "exclamation-triangle"
        class <- if (valid) {
          "validation-status validation-complete"
        } else {
          "validation-status validation-warning"
        }
        button_id <- paste0("go_to_", tolower(item$module))
        button_id <- NS(id, button_id)

        list(
          # Column 1: Module name
          span(
            bs_icon("table"),
            strong(item$module, ":"),
            paste(item$count, "record(s)"),
            style = "align-self: center;"
          ),
          # Column 2: Validation status (big column)
          div(
            class = class,
            style = "padding: 8px; border-radius: 4px; margin: 0px;",
            bs_icon(icon),
            " ",
            item$status
          ),
          # Column 3: Button
          input_task_button(
            id = button_id,
            label = HTML(paste(
              "Edit",
              bsicons::bs_icon("pencil-square")
            )),
            class = "btn-sm",
            style = "align-self: center;"
          )
        )
      })

      # Flatten the list for layout_column_wrap
      all_elements <- do.call(c, module_elements)

      # Return complete UI ----
      div(
        layout_column_wrap(
          width = NULL,
          fill = FALSE,
          fillable = FALSE,
          style = css(
            grid_template_columns = "1fr 8fr 1fr",
            align_items = "center",
            margin = "-10px 0"
          ),
          !!!all_elements,
          gap = "0.5rem"
        )
      )
    })

    ## output: measurement_table ----
    # upstream: moduleState$measurement_combinations, moduleState$data_entry_ready
    # downstream: UI table display
    # ! FORMAT-BASED
    output$measurement_table <- renderRHandsontable({
      if (
        !moduleState$data_entry_ready ||
          nrow(moduleState$measurement_combinations) == 0
      ) {
        rhandsontable(
          init_measurement_df(),
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all")
      } else {
        rhandsontable(
          moduleState$measurement_combinations,
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
          hot_col(
            c(
              "SITE_CODE",
              "PARAMETER_NAME",
              "SAMPLING_DATE",
              "ENVIRON_COMPARTMENT_SUB",
              "REP"
            ),
            readOnly = TRUE
          ) |>
          # Configure measurement fields
          hot_col(
            "MEASURED_FLAG",
            type = "dropdown",
            source = measured_flags,
            strict = TRUE
          ) |>
          hot_col(
            "MEASURED_VALUE",
            type = "numeric",
            format = "0.0000"
          ) |>
          hot_col(
            c("MEASURED_SD", "LOQ_VALUE", "LOD_VALUE"),
            type = "numeric",
            format = "0.0000"
          ) |>
          hot_col(
            c("MEASURED_UNIT", "LOQ_UNIT", "LOD_UNIT"),
            type = "dropdown",
            source = measured_units,
            strict = TRUE
          ) |>
          # Configure protocol fields as dropdowns
          hot_col(
            "SAMPLING_PROTOCOL",
            type = "dropdown",
            source = sampling_methods,
            strict = TRUE
          ) |>
          hot_col(
            "FRACTIONATION_PROTOCOL",
            type = "dropdown",
            source = fractionation_methods,
            strict = TRUE
          ) |>
          hot_col(
            "EXTRACTION_PROTOCOL",
            type = "dropdown",
            source = extraction_methods,
            strict = TRUE
          ) |>
          hot_col(
            "ANALYTICAL_PROTOCOL",
            type = "dropdown",
            source = analytical_methods,
            strict = TRUE
          ) |>
          hot_col(
            c("SAMPLE_ID", "REFERENCE_ID", "ENVIRON_COMPARTMENT"),
            readOnly = TRUE
          ) |>
          hot_cols(
            # fixedColumnsLeft = 5,
            manualColumnMove = TRUE,
            manualColumnResize = TRUE,
            columnSorting = TRUE
          )
      }
    })

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid, moduleState$data_entry_ready, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      llm_indicator <- if (
        session$userData$reactiveValues$llmExtractionComplete
      ) {
        div(
          bs_icon("cpu"),
          "Some data populated from LLM extraction - please review for accuracy",
          class = "validation-status validation-llm ",
          style = "margin-bottom: 10px;"
        )
      } else {
        NULL
      }

      validation_status <- if (!moduleState$data_entry_ready) {
        div(
          bs_icon("info-circle"),
          "Complete all setup modules to enable measurement data entry.",
          class = "validation-status validation-info"
        )
      } else if (moduleState$is_valid) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All measurement data validated successfully.",
            nrow(moduleState$measurement_combinations),
            "complete measurement record(s)."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          moduleState$validation_message,
          class = "validation-status validation-warning"
        )
      }

      div(llm_indicator, validation_status, class = "validation-container")
    })

    ## output: complete_data_preview ----
    # upstream: moduleState$complete_dataset
    # downstream: UI data display
    # ! FORMAT-BASED
    output$complete_data_preview <- renderText({
      if (isTruthy(moduleState$complete_dataset)) {
        # Show overview of complete dataset
        dataset_info <- paste0(
          "Complete Dataset Overview:\n",
          "Total Records: ",
          nrow(moduleState$complete_dataset),
          "\n",
          "Total Columns: ",
          ncol(moduleState$complete_dataset),
          "\n",
          "Sample-Parameter Combinations: ",
          length(unique(paste(
            moduleState$complete_dataset$SAMPLE_ID,
            moduleState$complete_dataset$PARAMETER_NAME
          ))),
          "\n",
          "Unique Sites: ",
          length(unique(moduleState$complete_dataset$SITE_CODE)),
          "\n",
          "Unique Parameters: ",
          length(unique(moduleState$complete_dataset$PARAMETER_NAME)),
          "\n\n",
          "First 3 records:\n",
          paste(
            capture.output(head(moduleState$complete_dataset, 3)),
            collapse = "\n"
          )
        )

        return(dataset_info)
      } else if (moduleState$data_entry_ready) {
        "# Complete dataset will appear here when all measurement data is validated"
      } else {
        "# Complete all setup modules to preview dataset structure"
      }
    })
  })
}

## To be copied in the UI ----
# mod_data_ui("data_1")

## To be copied in the server ----
# mod_data_server("data_1")
