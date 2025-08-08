# Export Module ----
# A Shiny module for exporting measurement data to Excel format

#' Export UI Function ----
#'
#' @description A shiny Module for data export functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList downloadButton
#' @importFrom bslib card card_header card_body accordion accordion_panel
#' @importFrom bsicons bs_icon
#' @export
mod_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
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
            "Export your campaign data to an Excel workbook. Each data module will be saved as a separate sheet in the workbook. Only modules with data will be included in the export."
          )
        ),

        ## Export status ----
        div(
          style = "margin: 20px 0;",
          uiOutput(ns("export_status"))
        ),

        ## Export button ----
        div(
          style = "margin: 20px 0; text-align: center;",
          downloadButton(
            outputId = ns("download_data"),
            label = "Download Excel File",
            class = "btn-primary btn-lg",
            icon = icon("download")
          )
        )
      )
    )
  )
}

#' Export Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive renderUI downloadHandler
#' @importFrom openxlsx2 wb_workbook wb_add_worksheet wb_add_data wb_save
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom bsicons bs_icon
#' @importFrom golem print_dev
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

    # 2. Observers and Reactives ----

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
        "referenceData",
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

    # 3. Outputs ----

    ## output: export_status ----
    # upstream: moduleState$export_ready, moduleState$available_datasets
    # downstream: UI status display
    output$export_status <- renderUI({
      "Hello!"
      if (moduleState$export_ready) {
        datasets <- moduleState$available_datasets
        dataset_list <- paste(datasets, collapse = ", ")

        div(
          bs_icon("check-circle"),
          glue("Export ready: {length(datasets)} datasets available"),
          br(),
          tags$small(glue("Datasets: {dataset_list}")),
          class = "validation-status validation-complete",
          style = "padding: 10px; border-radius: 4px;"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "No data available for export. Complete at least one data entry module first.",
          class = "validation-status validation-warning",
          style = "padding: 10px; border-radius: 4px;"
        )
      }
    })

    ## output: download_data ----
    # upstream: session$userData$reactiveValues, moduleState
    # downstream: Excel file download
    output$download_data <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
        glue("{campaign}_{timestamp}.xlsx")
      },

      content = function(file) {
        print_dev("mod_export: Starting Excel export...")

        rv <- session$userData$reactiveValues

        # Create workbook
        wb <- wb_workbook()

        # Add each available dataset as a sheet
        for (dataset_name in moduleState$available_datasets) {
          data <- rv[[dataset_name]]

          if (!is.null(data) && nrow(data) > 0) {
            # Clean sheet name (Excel has restrictions)
            sheet_name <- gsub("Data$", "", dataset_name) # Remove "Data" suffix
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
          "mod_export: Excel file saved with {length(moduleState$available_datasets)} sheets"
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
