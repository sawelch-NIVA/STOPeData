# Export Module ----
# A Shiny module for exporting measurement data to Excel or CSV formats

#' Export UI Function ----
#'
#' @description A shiny Module for data export functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList downloadButton hr div
#' @importFrom bslib card card_body accordion accordion_panel layout_column_wrap
#' @importFrom bsicons bs_icon
#' @export
mod_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main export card ----
    card(
      full_screen = TRUE,
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Export Information",
            icon = bs_icon("info-circle"),
            "Export your campaign data to Excel or CSV format. You can download individual datasets as CSV files (with accompanying metadata JSON), or download all data as a single Excel workbook with each dataset on a separate sheet and a metadata sheet, or as a ZIP file containing all CSV + JSON files."
          ),
          div(
            style = "margin: 10px 10px 0 10px; display: flex; align-items: center; gap: 15px;",
            actionButton(
              inputId = ns("get_data"),
              label = "Get Data from Modules",
              icon = icon("refresh"),
              class = "btn-primary"
            ),
            uiOutput(
              ns("export_status"),
            )
          )
        ),
        hr(),
        ## Individual dataset exports ----
        div(
          h4("Individual Dataset Exports (CSV + JSON)"),
          uiOutput(ns("individual_downloads"))
        ),
        hr(),
        ## Combined workbook export ----
        div(
          h4("Combined Export (Excel Workbook or Zipped CSV + JSON)"),
          uiOutput(ns("workbook_summary"), style = "margin-bottom: 10px;"),
          br(),
          uiOutput(ns("download_combined_ui"))
        )
      )
    )
  )
}

#' Export Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive renderUI downloadHandler req HTML
#' @importFrom openxlsx2 wb_workbook wb_add_worksheet wb_add_data wb_save
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom bsicons bs_icon
#' @importFrom bslib layout_column_wrap
#' @importFrom golem get_golem_version print_dev
#' @importFrom utils write.csv
#' @importFrom jsonlite toJSON write_json
#' @importFrom zip zip
#' @export
mod_export_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      export_ready = FALSE,
      available_datasets = character(0),
      campaign_name = "Unknown_Campaign",
      dataset_dimensions = list()
    )

    # 2. Helper functions ----

    ## Function: get_git_commit ----
    # Get git commit hash with error handling
    get_git_commit <- function() {
      tryCatch(
        {
          system("git rev-parse --short HEAD", intern = TRUE)
        },
        error = function(e) "Git hash not available"
      )
    }

    ## Function: get_export_metadata ----
    # upstream: session, moduleState
    # downstream: all export handlers
    get_export_metadata <- function() {
      rv <- session$userData$reactiveValues

      list(
        campaign_name = moduleState$campaign_name,
        export_datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
        app_name = "STOPeData",
        app_url = "https://github.com/sawelch-NIVA/STOPedata",
        app_version = get_golem_version() %||% "Version not available",
        git_commit = get_git_commit(),
        browser = session$clientData$user_agent %||% "Unknown browser",
        user = rv$ENTERED_BY %||% "Unknown user"
      )
    }

    ## Function: create_metadata_df ----
    # Create metadata as dataframe for Excel sheets
    create_metadata_df <- function(metadata_list) {
      data.frame(
        Property = names(metadata_list),
        Value = as.character(unlist(metadata_list)),
        stringsAsFactors = FALSE
      )
    }

    ## Function: get_dataset_display_name ----
    # Convert internal dataset names to user-friendly names
    get_dataset_display_name <- function(dataset_name) {
      display_names <- c(
        sitesData = "Sites",
        parametersData = "Parameters",
        compartmentsData = "Compartments",
        referencesData = "Reference",
        campaignData = "Campaign",
        methodsData = "Methods",
        samplesData = "Samples",
        biotaData = "Biota",
        dataData = "Measurements"
      )

      display_names[[dataset_name]] %||% dataset_name
    }

    # 3. Observers and Reactives ----

    ## observe: Check data availability ----
    # upstream: input$get_data (action button)
    # downstream: moduleState$export_ready, moduleState$available_datasets, moduleState$dataset_dimensions
    observe({
      rv <- session$userData$reactiveValues

      # Define all possible datasets
      dataset_names <- c(
        "sitesData",
        "parametersData",
        "compartmentsData",
        "referencesData",
        "campaignData",
        "methodsData",
        "samplesData",
        "biotaData",
        "dataData"
      )

      # Check which datasets have data and store dimensions
      available <- character(0)
      dimensions <- list()

      for (dataset in dataset_names) {
        data <- rv[[dataset]]
        if (!is.null(data) && nrow(data) > 0) {
          available <- c(available, dataset)
          dimensions[[dataset]] <- list(
            rows = nrow(data),
            cols = ncol(data)
          )
        }
      }

      moduleState$available_datasets <- available
      moduleState$dataset_dimensions <- dimensions
      moduleState$export_ready <- length(available) > 0

      # Get campaign name for filename
      if (!is.null(rv$campaignData) && nrow(rv$campaignData) > 0) {
        if ("CAMPAIGN_NAME" %in% names(rv$campaignData)) {
          campaign_name <- rv$campaignData$CAMPAIGN_NAME[1]
          if (!is.na(campaign_name) && campaign_name != "") {
            moduleState$campaign_name <- campaign_name
          }
        }
      }

      print_dev(glue(
        "mod_export: {length(available)} datasets available: {paste(available, collapse = ', ')}"
      ))
    }) |>
      bindEvent(input$get_data)

    ## observe: Create individual CSV download handlers ----
    # upstream: moduleState$available_datasets
    # downstream: individual CSV + JSON downloads
    observe({
      req(moduleState$export_ready)

      # Create a download handler for each available dataset
      lapply(moduleState$available_datasets, function(dataset_name) {
        output[[paste0("download_", dataset_name)]] <- downloadHandler(
          filename = function() {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
            display_name <- gsub(
              " ",
              "_",
              get_dataset_display_name(dataset_name)
            )
            glue("{campaign}_{display_name}_{timestamp}.zip")
          },

          content = function(file) {
            print_dev(glue(
              "mod_export: Exporting {dataset_name} as CSV + JSON..."
            ))

            rv <- session$userData$reactiveValues
            data <- rv[[dataset_name]]
            metadata <- get_export_metadata()

            # Create temporary directory for files
            temp_dir <- tempdir()
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
            display_name <- gsub(
              " ",
              "_",
              get_dataset_display_name(dataset_name)
            )
            base_name <- glue("{campaign}_{display_name}_{timestamp}")

            csv_file <- file.path(temp_dir, glue("{base_name}.csv"))
            json_file <- file.path(temp_dir, glue("{base_name}_metadata.json"))

            # Write CSV
            write.csv(data, file = csv_file, row.names = FALSE)

            # Write metadata JSON
            write_json(metadata, json_file, pretty = TRUE, auto_unbox = TRUE)

            # Create zip file
            zip(
              zipfile = file,
              files = c(csv_file, json_file),
              mode = "cherry-pick"
            )

            # Clean up temp files
            unlink(csv_file)
            unlink(json_file)

            print_dev(glue(
              "mod_export: ZIP export complete for {dataset_name} ({nrow(data)} rows)"
            ))
          },

          contentType = "application/zip"
        )
      })
    }) |>
      bindEvent(moduleState$available_datasets)

    # 4. Outputs ----

    ## output: export_status ----
    # upstream: moduleState$export_ready, moduleState$available_datasets
    # downstream: UI status display
    output$export_status <- renderUI({
      if (moduleState$export_ready) {
        datasets <- moduleState$available_datasets

        div(
          bs_icon("check-circle"),
          glue(" Export ready: {length(datasets)} datasets available"),
          class = "validation-status validation-complete",
          style = "display: inline-block;"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          " Click to check data availability.",
          class = "validation-status validation-warning",
          style = "display: inline-block;"
        )
      }
    })

    ## output: workbook_summary ----
    # upstream: moduleState$available_datasets, moduleState$dataset_dimensions
    # downstream: UI summary of available worksheets
    output$workbook_summary <- renderUI({
      req(moduleState$export_ready)

      # Create summary text for each dataset
      summaries <- sapply(
        moduleState$available_datasets,
        function(dataset_name) {
          display_name <- get_dataset_display_name(dataset_name)
          dims <- moduleState$dataset_dimensions[[dataset_name]]
          glue("{display_name} ({dims$rows} × {dims$cols})")
        }
      )

      summary_text <- paste(summaries, collapse = ", ")

      tags$code(summary_text)
    }) |>
      bindEvent(input$get_data)

    ## output: download_combined_ui ----
    # upstream: moduleState$export_ready
    # downstream: UI download buttons for combined exports (enabled/disabled based on data)
    output$download_combined_ui <- renderUI({
      if (moduleState$export_ready) {
        div(
          style = "display: flex; gap: 20px; margin-top: 10px;",
          downloadButton(
            outputId = ns("download_workbook"),
            label = "Download All as Excel Workbook",
            class = "btn-secondary",
            icon = icon("file-excel")
          ),
          downloadButton(
            outputId = ns("download_all_csv"),
            label = "Download All as CSV + JSON (ZIP)",
            class = "btn-secondary",
            icon = icon("file-zipper")
          )
        )
      } else {
        div(
          style = "display: flex; gap: 20px; margin-top: 10px;",
          tags$button(
            "Download All as Excel Workbook",
            class = "btn btn-secondary",
            disabled = "disabled",
            style = "opacity: 0.6; cursor: not-allowed;",
            icon("file-excel")
          ),
          tags$button(
            "Download All as CSV + JSON (ZIP)",
            class = "btn btn-secondary",
            disabled = "disabled",
            style = "opacity: 0.6; cursor: not-allowed;",
            icon("file-zipper")
          )
        )
      }
    })

    ## output: individual_downloads ----
    # upstream: moduleState$available_datasets, moduleState$dataset_dimensions, session$userData$reactiveValues
    # downstream: UI download buttons for each dataset (always visible, enabled/disabled based on data)
    output$individual_downloads <- renderUI({
      rv <- session$userData$reactiveValues

      # Define all possible datasets
      all_datasets <- c(
        "sitesData",
        "parametersData",
        "compartmentsData",
        "referencesData",
        "campaignData",
        "methodsData",
        "samplesData",
        "biotaData",
        "dataData"
      )

      # Create a card for each dataset (available or not)
      cards <- lapply(all_datasets, function(dataset_name) {
        display_name <- get_dataset_display_name(dataset_name)

        # Check if we have dimension info (only available after "Get Data" is clicked)
        if (dataset_name %in% names(moduleState$dataset_dimensions)) {
          dims <- moduleState$dataset_dimensions[[dataset_name]]
          dimensions <- glue("{dims$rows} rows × {dims$cols} columns")
          has_data <- TRUE
        } else {
          dimensions <- "No data available"
          has_data <- FALSE
        }

        if (has_data) {
          button_element <- downloadButton(
            outputId = ns(paste0("download_", dataset_name)),
            label = "Download CSV + JSON",
            class = "btn-secondary",
            icon = icon("download")
          )
        } else {
          button_element <- tags$button(
            "Download CSV + JSON",
            class = "btn btn-secondary",
            disabled = "disabled",
            style = "opacity: 0.6; cursor: not-allowed;",
            icon("download")
          )
        }

        div(
          style = "border: 1px solid #ddd; border-radius: 4px; padding: 15px; margin: 10px 0;",
          h5(display_name),
          tags$code(dimensions),
          br(),
          br(),
          button_element
        )
      })

      layout_column_wrap(
        width = "250px",
        !!!cards
      )
    }) |>
      bindEvent(input$get_data)

    ## output: download_workbook ----
    # upstream: session$userData$reactiveValues, moduleState
    # downstream: Excel workbook download
    output$download_workbook <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
        glue("{campaign}_{timestamp}.xlsx")
      },

      content = function(file) {
        print_dev("mod_export: Starting Excel workbook export...")

        rv <- session$userData$reactiveValues
        metadata <- get_export_metadata()

        # Create workbook
        wb <- wb_workbook()

        # Add metadata sheet first
        metadata_df <- create_metadata_df(metadata)
        wb <- wb |>
          wb_add_worksheet(sheet = "Metadata") |>
          wb_add_data(
            sheet = "Metadata",
            x = metadata_df,
            startRow = 1,
            startCol = 1
          )

        # Add each available dataset as a sheet
        for (dataset_name in moduleState$available_datasets) {
          data <- rv[[dataset_name]]

          if (!is.null(data) && nrow(data) > 0) {
            # Use display name for sheet
            sheet_name <- get_dataset_display_name(dataset_name)
            sheet_name <- substr(sheet_name, 1, 31) # Excel sheet name limit

            print_dev(glue(
              "mod_export: Adding sheet '{sheet_name}' with {nrow(data)} rows"
            ))

            # Add worksheet and data
            wb <- wb |>
              wb_add_worksheet(sheet = sheet_name) |>
              wb_add_data(
                sheet = sheet_name,
                x = data,
                startRow = 1,
                startCol = 1
              )
          }
        }

        # Save workbook
        wb_save(wb, file = file, overwrite = TRUE)

        print_dev(glue(
          "mod_export: Excel file saved with {length(moduleState$available_datasets) + 1} sheets"
        ))
      },

      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    ## output: download_all_csv ----
    # upstream: session$userData$reactiveValues, moduleState
    # downstream: ZIP file containing all CSV + JSON files
    output$download_all_csv <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
        glue("{campaign}_AllData_{timestamp}.zip")
      },

      content = function(file) {
        print_dev("mod_export: Starting combined CSV + JSON export...")

        rv <- session$userData$reactiveValues
        metadata <- get_export_metadata()

        # Create temporary directory for files
        temp_dir <- tempdir()
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)

        all_files <- character(0)

        # Export each dataset as CSV + JSON
        for (dataset_name in moduleState$available_datasets) {
          data <- rv[[dataset_name]]

          if (!is.null(data) && nrow(data) > 0) {
            display_name <- gsub(
              " ",
              "_",
              get_dataset_display_name(dataset_name)
            )
            base_name <- glue("{campaign}_{display_name}_{timestamp}")

            csv_file <- file.path(temp_dir, glue("{base_name}.csv"))
            json_file <- file.path(temp_dir, glue("{base_name}_metadata.json"))

            # Write CSV
            write.csv(data, file = csv_file, row.names = FALSE)

            # Write metadata JSON
            write_json(metadata, json_file, pretty = TRUE, auto_unbox = TRUE)

            all_files <- c(all_files, csv_file, json_file)

            print_dev(glue(
              "mod_export: Added {display_name} to combined export ({nrow(data)} rows)"
            ))
          }
        }

        # Create zip file with all CSVs and JSONs
        zip(
          zipfile = file,
          files = all_files,
          mode = "cherry-pick"
        )

        # Clean up temp files
        unlink(all_files)

        print_dev(glue(
          "mod_export: Combined ZIP export complete with {length(moduleState$available_datasets)} datasets"
        ))
      },

      contentType = "application/zip"
    )
  })
}

## To be copied in the UI ----
# mod_export_ui("export_1")

## To be copied in the server ----
# mod_export_server("export_1")
