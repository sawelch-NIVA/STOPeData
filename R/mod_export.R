# Export Module ----
# A Shiny module for validating, consolidating, and exporting all data to Excel

#' Export UI Function ----
#'
#' @description A shiny Module for data export with validation status and Excel generation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList downloadButton
#' @importFrom bslib card card_header card_body layout_column_wrap accordion accordion_panel
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs disabled disable enable
mod_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main export card ----
    card(
      card_header("Data Export"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Export Information",
            icon = bs_icon("info-circle"),
            "This module validates all data from previous modules and exports it as an Excel workbook. All modules must be completed and validated before export is enabled. The export includes campaign data, references, sites, parameters, compartments, methods, and samples with merged biota data."
          )
        ),

        ## Validation status overview ----
        div(
          style = "margin: 20px 0;",
          h5("Module Validation Status"),
          uiOutput(ns("validation_overview"))
        ),

        ## Export controls ----
        div(
          style = "margin: 20px 0;",
          h5("Export Controls"),
          layout_column_wrap(
            width = "300px",
            fill = FALSE,
            fillable = FALSE,

            downloadButton(
              outputId = ns("download_excel"),
              label = "Download Excel Export",
              icon = icon("download"),
              class = "btn-success",
              width = "100%"
            ) |>
              disabled()
          )
        ),

        ## Export preview ----
        accordion(
          id = ns("preview_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Preview Export Data",
            icon = bs_icon("eye"),
            div(
              h6("Campaign Data"),
              verbatimTextOutput(ns("preview_campaign")),
              h6("References Data"),
              verbatimTextOutput(ns("preview_references")),
              h6("Sites Data"),
              verbatimTextOutput(ns("preview_sites")),
              h6("Parameters Data"),
              verbatimTextOutput(ns("preview_parameters")),
              h6("Compartments Data"),
              verbatimTextOutput(ns("preview_compartments")),
              h6("Methods Data"),
              verbatimTextOutput(ns("preview_methods")),
              h6("Samples Data (with merged biota)"),
              verbatimTextOutput(ns("preview_samples"))
            )
          )
        )
      )
    )
  )
}

#' Export Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI downloadHandler
#' @importFrom shinyjs enable disable
#' @importFrom openxlsx2 wb_workbook wb_add_worksheet wb_add_data wb_save
#' @importFrom digest digest
#' @importFrom glue glue
#' @importFrom golem print_dev
mod_export_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      all_modules_valid = FALSE,
      export_ready = FALSE,
      consolidated_data = list()
    )

    # 2. Helper functions ----

    ## Check if all required modules are validated ----
    check_all_modules_valid <- function() {
      required_data <- list(
        campaign = session$userData$reactiveValues$campaignData,
        references = session$userData$reactiveValues$referencesData,
        sites = session$userData$reactiveValues$sitesData,
        parameters = session$userData$reactiveValues$parametersData,
        compartments = session$userData$reactiveValues$compartmentsData,
        methods = session$userData$reactiveValues$methodsData,
        samples = session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
          session$userData$reactiveValues$sampleData
      )

      all(sapply(required_data, function(x) !is.null(x)))
    }

    ## Get validation status for each module ----
    get_module_status <- function() {
      modules <- list(
        Campaign = session$userData$reactiveValues$campaignData,
        References = session$userData$reactiveValues$referencesData,
        Sites = session$userData$reactiveValues$sitesData,
        Parameters = session$userData$reactiveValues$parametersData,
        Compartments = session$userData$reactiveValues$compartmentsData,
        Methods = session$userData$reactiveValues$methodsData,
        Samples = session$userData$reactiveValues$sampleData,
        Biota = session$userData$reactiveValues$biotaValidated
      )

      status_list <- lapply(names(modules), function(name) {
        data <- modules[[name]]
        if (name == "Biota") {
          # Special handling for biota validation flag
          status <- if (isTruthy(data)) "✓ Validated" else
            "⚠ No validated data available"
          count <- if (isTruthy(session$userData$reactiveValues$biotaData)) {
            nrow(session$userData$reactiveValues$biotaData)
          } else {
            "No biota samples"
          }
        } else {
          status <- if (isTruthy(data) && nrow(data) > 0) "✓ Validated" else
            "⚠ Pending"
          count <- if (isTruthy(data)) {
            nrow(data)
          } else {
            0
          }
        }

        list(module = name, status = status, count = count)
      })

      return(status_list)
    }

    ## Consolidate all data for export ----
    consolidate_export_data <- function() {
      # Get samples data with biota merged if available
      samples_data <- session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
        session$userData$reactiveValues$sampleData

      consolidated <- list(
        campaign = session$userData$reactiveValues$campaignData,
        references = session$userData$reactiveValues$referencesData,
        sites = session$userData$reactiveValues$sitesData,
        parameters = session$userData$reactiveValues$parametersData,
        compartments = session$userData$reactiveValues$compartmentsData,
        methods = session$userData$reactiveValues$methodsData,
        samples = samples_data
      )

      return(consolidated)
    }

    ## Create Excel workbook ----
    create_excel_export <- function() {
      data <- consolidate_export_data()

      # Create workbook
      wb <- wb_workbook()

      # Add data sheets
      wb <- wb |>
        wb_add_worksheet("Campaign") |>
        wb_add_data(sheet = "Campaign", x = as.data.frame(data$campaign))

      wb <- wb |>
        wb_add_worksheet("References") |>
        wb_add_data(sheet = "References", x = as.data.frame(data$references))

      wb <- wb |>
        wb_add_worksheet("Sites") |>
        wb_add_data(sheet = "Sites", x = data$sites)

      wb <- wb |>
        wb_add_worksheet("Parameters") |>
        wb_add_data(sheet = "Parameters", x = data$parameters)

      wb <- wb |>
        wb_add_worksheet("Compartments") |>
        wb_add_data(sheet = "Compartments", x = data$compartments)

      wb <- wb |>
        wb_add_worksheet("Methods") |>
        wb_add_data(sheet = "Methods", x = data$methods)

      wb <- wb |>
        wb_add_worksheet("Samples") |>
        wb_add_data(sheet = "Samples", x = data$samples)

      # Create metadata
      metadata <- data.frame(
        Field = c(
          "Export_Timestamp",
          "App_Version",
          "User",
          "Campaign_Name",
          "Total_Samples",
          "Total_Sites",
          "Total_Parameters",
          "Workbook_Hash"
        ),
        Value = c(
          as.character(Sys.time()),
          "1.0.0", # TODO: Get from golem
          data$campaign$ENTERED_BY %|truthy|% "Unknown",
          data$campaign$CAMPAIGN_NAME %|truthy|% "Unknown",
          nrow(data$samples),
          nrow(data$sites),
          nrow(data$parameters),
          "TBD" # Will be calculated after workbook creation
        ),
        stringsAsFactors = FALSE
      )

      wb <- wb |>
        wb_add_worksheet("Metadata") |>
        wb_add_data(sheet = "Metadata", x = metadata)

      return(wb)
    }

    ## Calculate workbook hash ----
    calculate_workbook_hash <- function(wb) {
      # Save to temporary file to calculate hash
      temp_file <- tempfile(fileext = ".xlsx")
      wb_save(wb, temp_file)

      # Calculate hash
      file_hash <- digest(file = temp_file, algo = "md5")

      # Clean up
      unlink(temp_file)

      return(file_hash)
    }

    # 3. Observers and Reactives ----

    ## observe: Check validation status continuously ----
    # upstream: all session$userData$reactiveValues
    # downstream: moduleState$all_modules_valid, download button state
    observe({
      moduleState$all_modules_valid <- check_all_modules_valid()

      if (moduleState$all_modules_valid) {
        moduleState$export_ready <- TRUE
        enable("download_excel")
        print_dev("mod_export: All modules validated, export enabled")
      } else {
        moduleState$export_ready <- FALSE
        disable("download_excel")
        print_dev("mod_export: Some modules pending, export disabled")
      }
    })

    ## observe: Update consolidated data when ready ----
    # upstream: moduleState$export_ready
    # downstream: moduleState$consolidated_data
    observe({
      if (moduleState$export_ready) {
        moduleState$consolidated_data <- consolidate_export_data()
      }
    }) |>
      bindEvent(moduleState$export_ready)

    # 4. Outputs ----

    ## output: validation_overview ----
    # upstream: session data
    # downstream: UI validation status display
    output$validation_overview <- renderUI({
      status_list <- get_module_status()

      status_divs <- lapply(status_list, function(item) {
        status_class <- if (grepl("✓", item$status)) {
          "validation-status validation-complete"
        } else {
          "validation-status validation-warning"
        }

        div(
          class = status_class,
          style = "margin: 5px 0; padding: 8px; border-radius: 4px;",
          strong(item$module),
          ": ",
          item$status,
          " (",
          item$count,
          if (item$module == "Campaign" || item$module == "References")
            " record" else " records",
          ")"
        )
      })

      div(
        do.call(tagList, status_divs),
        hr(),
        if (moduleState$all_modules_valid) {
          div(
            bs_icon("check-circle"),
            " All modules validated - export ready",
            class = "validation-status validation-complete",
            style = "margin-top: 10px; padding: 10px; border-radius: 4px;"
          )
        } else {
          div(
            bs_icon("exclamation-triangle"),
            " Complete all modules to enable export",
            class = "validation-status validation-warning",
            style = "margin-top: 10px; padding: 10px; border-radius: 4px;"
          )
        }
      )
    })

    ## output: download_excel ----
    # upstream: moduleState$consolidated_data
    # downstream: Excel file download
    output$download_excel <- downloadHandler(
      filename = function() {
        campaign_name <- moduleState$consolidated_data$campaign$CAMPAIGN_NAME %|truthy|%
          "Export"
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0(campaign_name, "_", timestamp, ".xlsx")
      },
      content = function(file) {
        wb <- create_excel_export()

        # Calculate and update hash in metadata
        wb_hash <- calculate_workbook_hash(wb)

        # Update the metadata sheet with the hash
        metadata_updated <- data.frame(
          Field = c(
            "Export_Timestamp",
            "App_Version",
            "User",
            "Campaign_Name",
            "Total_Samples",
            "Total_Sites",
            "Total_Parameters",
            "Workbook_Hash"
          ),
          Value = c(
            as.character(Sys.time()),
            "1.0.0",
            moduleState$consolidated_data$campaign$ENTERED_BY %|truthy|%
              "Unknown",
            moduleState$consolidated_data$campaign$CAMPAIGN_NAME %|truthy|%
              "Unknown",
            nrow(moduleState$consolidated_data$samples),
            nrow(moduleState$consolidated_data$sites),
            nrow(moduleState$consolidated_data$parameters),
            wb_hash
          ),
          stringsAsFactors = FALSE
        )

        wb <- wb |>
          wb_add_data(sheet = "Metadata", x = metadata_updated, start_row = 1)

        wb_save(wb, file)
      }
    )

    ## Preview outputs ----
    # upstream: moduleState$consolidated_data
    # downstream: UI preview displays
    output$preview_campaign <- renderText({
      if (isTruthy(moduleState$consolidated_data$campaign)) {
        paste(
          capture.output(str(moduleState$consolidated_data$campaign)),
          collapse = "\n"
        )
      } else {
        "Campaign data not available"
      }
    })

    output$preview_references <- renderText({
      if (isTruthy(moduleState$consolidated_data$references)) {
        paste(
          capture.output(str(moduleState$consolidated_data$references)),
          collapse = "\n"
        )
      } else {
        "References data not available"
      }
    })

    output$preview_sites <- renderText({
      if (isTruthy(moduleState$consolidated_data$sites)) {
        paste0(
          "Sites data: ",
          nrow(moduleState$consolidated_data$sites),
          " rows, ",
          ncol(moduleState$consolidated_data$sites),
          " columns\n",
          paste(
            capture.output(head(moduleState$consolidated_data$sites, 3)),
            collapse = "\n"
          )
        )
      } else {
        "Sites data not available"
      }
    })

    output$preview_parameters <- renderText({
      if (isTruthy(moduleState$consolidated_data$parameters)) {
        paste0(
          "Parameters data: ",
          nrow(moduleState$consolidated_data$parameters),
          " rows, ",
          ncol(moduleState$consolidated_data$parameters),
          " columns\n",
          paste(
            capture.output(head(moduleState$consolidated_data$parameters, 3)),
            collapse = "\n"
          )
        )
      } else {
        "Parameters data not available"
      }
    })

    output$preview_compartments <- renderText({
      if (isTruthy(moduleState$consolidated_data$compartments)) {
        paste0(
          "Compartments data: ",
          nrow(moduleState$consolidated_data$compartments),
          " rows, ",
          ncol(moduleState$consolidated_data$compartments),
          " columns\n",
          paste(
            capture.output(head(moduleState$consolidated_data$compartments, 3)),
            collapse = "\n"
          )
        )
      } else {
        "Compartments data not available"
      }
    })

    output$preview_methods <- renderText({
      if (isTruthy(moduleState$consolidated_data$methods)) {
        paste0(
          "Methods data: ",
          nrow(moduleState$consolidated_data$methods),
          " rows, ",
          ncol(moduleState$consolidated_data$methods),
          " columns\n",
          paste(
            capture.output(head(moduleState$consolidated_data$methods, 3)),
            collapse = "\n"
          )
        )
      } else {
        "Methods data not available"
      }
    })

    output$preview_samples <- renderText({
      if (isTruthy(moduleState$consolidated_data$samples)) {
        biota_cols <- intersect(
          names(moduleState$consolidated_data$samples),
          c(
            "SPECIES_GROUP",
            "SAMPLE_SPECIES",
            "SAMPLE_TISSUE",
            "SAMPLE_SPECIES_LIFESTAGE",
            "SAMPLE_SPECIES_GENDER"
          )
        )
        biota_status <- if (length(biota_cols) > 0) {
          paste("(includes", length(biota_cols), "biota columns)")
        } else {
          "(no biota data merged)"
        }

        paste0(
          "Samples data: ",
          nrow(moduleState$consolidated_data$samples),
          " rows, ",
          ncol(moduleState$consolidated_data$samples),
          " columns ",
          biota_status,
          "\n",
          paste(
            capture.output(head(moduleState$consolidated_data$samples, 3)),
            collapse = "\n"
          )
        )
      } else {
        "Samples data not available"
      }
    })
  })
}

## To be copied in the UI ----
# mod_export_ui("export_1")

## To be copied in the server ----
# mod_export_server("export_1")
