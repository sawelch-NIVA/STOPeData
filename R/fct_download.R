# Metadata Format Helper Functions ----
# Functions to create and read human-readable metadata files

#' Create readable metadata text file ----
#'
#' @description Create a human-readable text file with export metadata
#' @param metadata_list List containing metadata
#' @param file_path Path where to write the metadata file
#' @importFrom glue glue
#' @export
write_metadata_txt <- function(metadata_list, file_path) {
  # Helper operator for string repetition
  `%r%` <- function(string, times) {
    paste(rep(string, times), collapse = "")
  }

  # Create human-readable content
  content <- c(
    "STOPeData Export Metadata",
    "=" %r% 50, # 50 equals signs
    "",
    glue("Campaign Name: {metadata_list$campaign_name}"),
    glue("Export Date/Time: {metadata_list$export_datetime}"),
    glue("Exported By: {metadata_list$user}"),
    "",
    "Application Information:",
    glue("  App Name: {metadata_list$app_name}"),
    glue("  App Version: {metadata_list$app_version}"),
    glue("  Git Commit: {metadata_list$git_commit}"),
    glue("  App URL: {metadata_list$app_url}"),
    "",
    "Technical Information:",
    glue("  Browser: {metadata_list$browser}"),
    "",
    "=" %r% 50,
    "",
    "This ZIP file contains CSV data files exported from STOPeData.",
    "Each CSV file contains one type of data (sites, parameters, etc.).",
    "To re-import this data, use the 'Upload session data' option",
    "in STOPeData."
  )

  # Write to file
  writeLines(content, file_path)
}

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
get_export_metadata <- function(session = NULL) {
  if (is.null(session)) {
    stop("session reactive object must be supplied to create CSVs")
  }
  rv <- session$userData$reactiveValues

  list(
    campaign_name = rv$campaignData$CAMPAIGN_NAME,
    export_datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    app_name = "STOPeData",
    app_url = "https://github.com/sawelch-NIVA/STOPedata",
    app_version = get_golem_version() %||% "Version not available",
    git_commit = get_git_commit(),
    browser = session$clientData$user_agent %||% "Unknown browser",
    user = rv$ENTERED_BY %||% "Unknown user"
  )
}

## Function: create_metadata_tibble ----
# Create metadata as dataframe for Excel sheets
create_metadata_tibble <- function(metadata_list) {
  tibble(
    Property = names(metadata_list),
    Value = as.character(unlist(metadata_list))
  )
}

## Function: get_dataset_display_name ----
# Convert internal dataset names to user-friendly names
get_dataset_display_name <- function(dataset_name) {
  display_names <- c(
    sitesData = "Sites",
    parametersData = "Parameters",
    compartmentsData = "Compartments",
    referenceData = "Reference",
    campaignData = "Campaign",
    methodsData = "Methods",
    samplesData = "Samples",
    biotaData = "Biota",
    measurementsData = "Measurements"
  )

  display_names[[dataset_name]] %||% dataset_name
}


download_all_csv <-
  function(session, moduleState = NULL) {
    if (is.null(moduleState) | is.null(session)) {
      stop(
        "moduleState & session reactive objects must be supplied to create CSVs"
      )
    }
    downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
        glue("{campaign}AllData_{timestamp}.zip")
      },

      content = function(file) {
        print_dev("mod_export: Starting combined CSV + TXT export...")

        rv <- session$userData$reactiveValues
        metadata <- get_export_metadata(session = session)

        # Create temporary directory for files
        temp_dir <- tempdir()
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)

        all_files <- character(0)

        # Export each dataset as CSV + individual metadata TXT
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

            # Write CSV
            write.csv(data, file = csv_file, row.names = FALSE)

            all_files <- c(all_files, csv_file)

            print_dev(glue(
              "mod_export: Added {display_name} to combined export ({nrow(data)} rows)"
            ))
          }
        }

        # Write one metadata file for the entire export
        combined_metadata_file <- file.path(
          temp_dir,
          glue("{campaign}_export_metadata_{timestamp}.txt")
        )
        write_metadata_txt(metadata, combined_metadata_file)
        all_files <- c(all_files, combined_metadata_file)

        # Create zip file with all CSVs and one metadata TXT
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
  }
