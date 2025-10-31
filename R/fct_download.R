# Metadata Format Helper Functions ----
# Functions to create and read human-readable metadata files

#' Create readable metadata text file
#'
#' @description Create a human-readable text file with export metadata
#' @param metadata_list List containing metadata fields (campaign_name, export_datetime, etc.)
#' @param file_path Character. Path where to write the metadata file
#' @return NULL (invisibly). File is written to disk as a side effect.
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
    "=" %r% 50,
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

#' Get git commit hash
#'
#' @description Retrieve the short git commit hash of the current repository state
#' @return Character. Short git commit hash, or "Git hash not available" if retrieval fails
#' @export
get_git_commit <- function() {
  tryCatch(
    {
      system("git rev-parse --short HEAD", intern = TRUE)
    },
    error = function(e) "Git hash not available"
  )
}

#' Get export metadata
#'
#' @description Gather metadata about the current export session
#' @param session Shiny session object. Required to access user data and client information.
#' @return List containing export metadata fields (campaign_name, export_datetime, app_name, etc.)
#' @export
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

#' Create metadata tibble
#'
#' @description Convert metadata list to tibble format suitable for Excel sheets
#' @param metadata_list List containing metadata fields
#' @return Tibble with Property and Value columns
#' @importFrom tibble tibble
#' @export
create_metadata_tibble <- function(metadata_list) {
  tibble(
    Property = names(metadata_list),
    Value = as.character(unlist(metadata_list))
  )
}

#' Get dataset display name
#'
#' @description Convert internal dataset names to user-friendly display names
#' @param dataset_name Character. Internal name of the dataset (e.g., "sitesData")
#' @return Character. User-friendly display name (e.g., "Sites")
#' @export
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
    measurementsData = "Measurements",
    schemaLLM = "LLM_Schema",
    promptLLM = "LLM_Prompt",
    rawLLM = "LLM_Raw_Response"
  )

  display_names[[dataset_name]] %||% dataset_name
}

#' Convert object to human-readable text
#'
#' @description Convert various R objects (character vectors, lists, S3/S4 objects) to
#'   readable text format suitable for writing to .txt files
#' @param obj The object to convert (character, list, ellmer_schema, or other object types)
#' @param dataset_name Character. Name of the dataset for header context
#' @return Character vector suitable for use with writeLines()
#' @importFrom glue glue
#' @importFrom utils str capture.output
#' @export
object_to_text <- function(obj, dataset_name = "unknown") {
  if (is.character(obj)) {
    # Already a character vector - return as-is
    return(obj)
  } else if (is.list(obj)) {
    # For lists, use str() to get a structured view
    header <- c(
      glue("# {dataset_name}"),
      glue("# Exported: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
      "# Type: List",
      "",
      "# Structure:"
    )
    structure_text <- capture.output(str(obj, max.level = 3))

    return(c(header, structure_text))
  } else if (inherits(obj, "ellmer_schema") || is.object(obj)) {
    # For schema objects or other S3/S4 objects, use print()
    header <- c(
      glue("# {dataset_name}"),
      glue("# Exported: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
      glue("# Type: {class(obj)[1]}"),
      ""
    )
    object_text <- capture.output(print(obj))

    return(c(header, object_text))
  } else {
    # Fallback for other types
    header <- c(
      glue("# {dataset_name}"),
      glue("# Exported: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
      glue("# Type: {class(obj)[1]}"),
      ""
    )
    text <- capture.output(print(obj))

    return(c(header, text))
  }
}

#' Download all data as CSV and TXT files in a ZIP archive
#'
#' @description Creates a Shiny downloadHandler that exports all available datasets
#'   as CSV files (for tabular data) or TXT files (for text/object data) in a single ZIP archive.
#'   Also includes a metadata file with export information.
#' @param session Shiny session object. Required to access reactive values.
#' @param moduleState ReactiveValues object containing export_ready flag, available_datasets,
#'   and campaign_name fields.
#' @return A Shiny downloadHandler function
#' @importFrom glue glue
#' @importFrom zip zip
#' @importFrom readr write_excel_csv
#' @export
download_all_csv <- function(session, moduleState = NULL) {
  if (is.null(moduleState) || is.null(session)) {
    stop(
      "moduleState & session reactive objects must be supplied to create CSVs"
    )
  }

  campaign_short <- function() {
    if (nrow(session$userData$reactiveValues$referenceData) > 0) {
      generate_reference_id(
        session$userData$reactiveValues$referenceData$YEAR,
        session$userData$reactiveValues$referenceData$AUTHOR,
        session$userData$reactiveValues$referenceData$TITLE
      )
    } else {
      "ReferenceNotFound"
    }
  }

  downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      campaign <- campaign_short()
      glue("{campaign}_AllData_{timestamp}.zip")
    },

    content = function(file) {
      print_dev("mod_export: Starting combined CSV + TXT export...")

      rv <- session$userData$reactiveValues
      metadata <- get_export_metadata(session = session)

      # Setup temporary directory and naming
      temp_dir <- tempdir()
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      campaign <- campaign_short()

      # Define which datasets should be exported as text files
      text_datasets <- c("schemaLLM", "promptLLM", "rawLLM")

      all_files <- character(0)

      # Export each dataset ----
      for (dataset_name in moduleState$available_datasets) {
        data <- rv[[dataset_name]]

        display_name <- gsub(
          " ",
          "_",
          get_dataset_display_name(dataset_name)
        )
        base_name <- glue("{campaign}_{display_name}_{timestamp}")

        if (dataset_name %in% text_datasets) {
          # Handle text/object data
          if (!is.null(data)) {
            txt_file <- file.path(temp_dir, glue("{base_name}.txt"))

            # Convert object to text using helper function
            text_content <- object_to_text(data, dataset_name = display_name)
            writeLines(text_content, txt_file)

            all_files <- c(all_files, txt_file)

            print_dev(glue(
              "mod_export: Added {display_name} to combined export (text file, {length(text_content)} lines)"
            ))
          }
        } else {
          # Handle tabular data
          if (!is.null(data) && nrow(data) > 0) {
            csv_file <- file.path(temp_dir, glue("{base_name}.csv"))

            write_excel_csv(data, file = csv_file, row.names = FALSE)

            all_files <- c(all_files, csv_file)

            print_dev(glue(
              "mod_export: Added {display_name} to combined export ({nrow(data)} rows)"
            ))
          }
        }
      }

      # Write metadata file ----
      combined_metadata_file <- file.path(
        temp_dir,
        glue("{campaign}_export_metadata_{timestamp}.txt")
      )
      write_metadata_txt(metadata, combined_metadata_file)
      all_files <- c(all_files, combined_metadata_file)

      # Create and cleanup ZIP archive ----
      zip(
        zipfile = file,
        files = all_files,
        mode = "cherry-pick"
      )

      unlink(all_files)

      print_dev(glue(
        "mod_export: Combined ZIP export complete with {length(moduleState$available_datasets)} datasets"
      ))
    },

    contentType = "application/zip"
  )
}
