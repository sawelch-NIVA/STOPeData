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
#' @importFrom shiny NS tagList downloadButton
#' @importFrom bslib card card_body accordion accordion_panel layout_column_wrap
#' @importFrom bsicons bs_icon
#' @export
mod_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main export card ----
    card(
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Export Information",
            icon = bs_icon("info-circle"),
            "Export your campaign data to Excel or CSV format. You can download individual datasets as CSV files (with accompanying metadata JSON), or download all data as a single Excel workbook with each dataset on a separate sheet and a metadata sheet."
          )
        ),

        ## Export status ----
        div(
          style = "margin: 20px 0;",
          uiOutput(ns("export_status"))
        ),

        ## Individual dataset exports ----
        div(
          style = "margin: 20px 0;",
          h4("Individual Dataset Exports (CSV + JSON)"),
          uiOutput(ns("individual_downloads"))
        ),

        ## Combined workbook export ----
        div(
          style = "margin: 20px 0; text-align: center; border-top: 1px solid #ddd; padding-top: 20px;",
          h4("Combined Export (Excel)"),
          uiOutput(ns("download_workbook_ui"))
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
      campaign_name = "Unknown_Campaign"
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
    # upstream: session$userData$reactiveValues
    # downstream: moduleState$export_ready, moduleState$available_datasets
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

      # Check which datasets have data
      available <- character(0)

      for (dataset in dataset_names) {
        data <- rv[[dataset]]
        if (!is.null(data) && nrow(data) > 0) {
          available <- c(available, dataset)
        }
      }

      moduleState$available_datasets <- available
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
      bindEvent(
        session$userData$reactiveValues$campaignData,
        session$userData$reactiveValues$referenceData,
        session$userData$reactiveValues$sitesData,
        session$userData$reactiveValues$parametersData,
        session$userData$reactiveValues$compartmentsData,
        session$userData$reactiveValues$methodsData,
        session$userData$reactiveValues$samplesData,
        session$userData$reactiveValues$biotaData,
        session$userData$reactiveValues$dataData,
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )

    # 4. Outputs ----

    ## output: export_status ----
    # upstream: moduleState$export_ready, moduleState$available_datasets
    # downstream: UI status display
    output$export_status <- renderUI({
      if (moduleState$export_ready) {
        datasets <- moduleState$available_datasets
        dataset_list <- paste(
          sapply(datasets, get_dataset_display_name),
          collapse = ", "
        )

        div(
          bs_icon("check-circle"),
          glue(" Export ready: {length(datasets)} datasets available"),
          br(),
          dataset_list,
          class = "validation-status validation-complete",
          style = "padding: 10px; border-radius: 4px;"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          " No data available for export. Complete at least one data entry module first.",
          class = "validation-status validation-warning",
          style = "padding: 10px; border-radius: 4px;"
        )
      }
    })

    ## output: individual_downloads ----
    # upstream: moduleState$available_datasets, session$userData$reactiveValues
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
        data <- rv[[dataset_name]]

        # Check if dataset has data
        has_data <- !is.null(data) && nrow(data) > 0

        if (has_data) {
          dimensions <- glue("{nrow(data)} rows × {ncol(data)} columns")
          button_element <- downloadButton(
            outputId = ns(paste0("download_", dataset_name)),
            label = "Download CSV + JSON",
            class = "btn-secondary",
            icon = icon("download")
          )
        } else {
          dimensions <- "No data available"
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
    })

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

    ## output: download_workbook_ui ----
    # upstream: moduleState$export_ready
    # downstream: UI download button for workbook (enabled/disabled based on data)
    output$download_workbook_ui <- renderUI({
      if (moduleState$export_ready) {
        downloadButton(
          outputId = ns("download_workbook"),
          label = "Download All as Excel Workbook",
          class = "btn-primary btn-lg",
          icon = icon("file-excel")
        )
      } else {
        tags$button(
          "Download All as Excel Workbook",
          class = "btn btn-primary btn-lg",
          disabled = "disabled",
          style = "opacity: 0.6; cursor: not-allowed;",
          icon("file-excel")
        )
      }
    })

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
  })
}

## To be copied in the UI ----
# mod_export_ui("export_1")

## To be copied in the server ----
# mod_export_server("export_1")
