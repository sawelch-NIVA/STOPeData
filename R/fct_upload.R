# Import Helper Functions ----
# Functions to handle importing data from ZIP files exported by mod_export

#' Import data from exported ZIP file ----
#'
#' @description Import datasets from a ZIP file created by mod_export
#' @param zip_path Path to the ZIP file
#' @param session Shiny session object
#' @return List with success status and any error messages
#' @importFrom utils unzip read.csv
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @export
import_session_from_zip <- function(zip_path, session) {
  # Validate inputs ----
  if (!file.exists(zip_path)) {
    return(list(
      success = FALSE,
      message = "ZIP file not found"
    ))
  }

  # Extract ZIP contents ----
  temp_dir <- tempdir()
  extract_dir <- file.path(temp_dir, "import_extract")

  tryCatch(
    {
      # Clean up any previous extractions
      if (dir.exists(extract_dir)) {
        unlink(extract_dir, recursive = TRUE)
      }
      dir.create(extract_dir, recursive = TRUE)

      # Extract ZIP
      extracted_files <- unzip(zip_path, exdir = extract_dir)
      print_dev(glue("import: Extracted {length(extracted_files)} files"))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        message = glue("Failed to extract ZIP file: {e$message}")
      ))
    }
  )

  # Find and validate files ----
  csv_files <- extracted_files[grepl(
    "\\.csv$",
    extracted_files,
    ignore.case = TRUE
  )]
  txt_files <- extracted_files[grepl(
    "\\.txt$",
    extracted_files,
    ignore.case = TRUE
  )]

  if (length(csv_files) == 0) {
    return(list(
      success = FALSE,
      message = "No CSV files found in ZIP"
    ))
  }

  # Look for metadata file
  metadata_file <- txt_files[grepl("metadata", txt_files, ignore.case = TRUE)][
    1
  ]
  metadata <- NULL
  if (!is.na(metadata_file)) {
    metadata <- read_metadata_txt(metadata_file)
    if (!is.null(metadata)) {
      print_dev(glue(
        "import: Found metadata - Campaign: {metadata$campaign_name %||% 'Unknown'}"
      ))
    }
  }

  print_dev(glue(
    "import: Found {length(csv_files)} CSV files and {length(txt_files)} TXT files"
  ))

  # Process each CSV file ----
  results <- list()
  successful_imports <- 0
  failed_imports <- character(0)

  for (csv_file in csv_files) {
    result <- import_module_table(csv_file, session)

    if (result$success) {
      successful_imports <- successful_imports + 1
      print_dev(glue("import: Successfully imported {result$dataset_type}"))
    } else {
      failed_imports <- c(
        failed_imports,
        result$dataset_type %||% basename(csv_file)
      )
      print_dev(glue(
        "import: Failed to import {result$dataset_type %||% basename(csv_file)}: {result$message}"
      ))
    }

    results[[basename(csv_file)]] <- result
  }

  # Clean up ----
  unlink(extract_dir, recursive = TRUE)

  # Return summary ----
  if (successful_imports == 0) {
    return(list(
      success = FALSE,
      message = "No datasets could be imported successfully",
      details = results
    ))
  } else if (length(failed_imports) == 0) {
    return(list(
      success = TRUE,
      message = glue("Successfully imported all {successful_imports} datasets"),
      details = results
    ))
  } else {
    return(list(
      success = TRUE,
      message = glue(
        "Successfully imported {successful_imports} datasets. ",
        "Failed to import: {paste(failed_imports, collapse = ', ')}"
      ),
      details = results
    ))
  }
}

#' Import module dataset from CSV ----
#'
#' @description Import a single dataset and add to session reactiveValues
#' @param csv_path Path to CSV file
#' @param session Shiny session object
#' @return List with success status and details
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @export
import_module_table <- function(csv_path, session) {
  # Determine dataset type from filename ----
  filename <- basename(csv_path)
  dataset_type <- detect_dataset_type(filename)

  if (is.null(dataset_type)) {
    return(list(
      success = FALSE,
      dataset_type = filename,
      message = "Could not determine dataset type from filename"
    ))
  }

  # Read and validate CSV ----
  tryCatch(
    {
      data <- read.csv(csv_path, stringsAsFactors = FALSE) |>
        as_tibble()

      if (nrow(data) == 0) {
        return(list(
          success = FALSE,
          dataset_type = dataset_type,
          message = "CSV file is empty"
        ))
      }

      print_dev(glue("import: Read {nrow(data)} rows from {dataset_type} CSV"))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        dataset_type = dataset_type,
        message = glue("Failed to read CSV: {e$message}")
      ))
    }
  )

  # Validate dataset structure ----
  validation_result <- validate_dataset_structure(data, dataset_type)

  if (!validation_result$valid) {
    return(list(
      success = FALSE,
      dataset_type = dataset_type,
      message = glue("Invalid dataset structure: {validation_result$message}")
    ))
  }

  # Add to session reactiveValues ----
  rv_key <- get_reactiveValues_key(dataset_type)

  if (is.null(rv_key)) {
    return(list(
      success = FALSE,
      dataset_type = dataset_type,
      message = "Unknown reactiveValues key for dataset type"
    ))
  }

  tryCatch(
    {
      # Replace existing data
      session$userData$reactiveValues[[rv_key]] <- data

      return(list(
        success = TRUE,
        dataset_type = dataset_type,
        message = glue("Successfully imported {nrow(data)} rows"),
        rows_imported = nrow(data)
      ))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        dataset_type = dataset_type,
        message = glue("Failed to add to session: {e$message}")
      ))
    }
  )
}

#' Detect dataset type from filename ----
#'
#' @description Determine which type of dataset based on filename patterns
#' @param filename Name of the file
#' @return Character string of dataset type or NULL if not recognized
#' @export
detect_dataset_type <- function(filename) {
  # Remove timestamp and campaign name patterns to get core name
  # Expected patterns: Campaign_DatasetType_20241024_123456.csv

  # Convert to lowercase for easier matching
  filename_lower <- tolower(filename)

  # Define patterns for each dataset type
  patterns <- list(
    # TODO: Needs to be strict rather than speculative
    "Sites" = c("sites", "site"),
    "Parameters" = c("parameters", "parameter", "param"),
    "Compartments" = c("compartments", "compartment", "comp"),
    "Reference" = c("reference", "ref"),
    "Campaign" = c("campaign", "camp"),
    "Methods" = c("methods", "method"),
    "Samples" = c("samples", "sample"),
    "Biota" = c("biota", "bio"),
    "Measurements" = c("measurements", "measurement", "data")
  )

  # Check each pattern
  for (dataset_type in names(patterns)) {
    for (pattern in patterns[[dataset_type]]) {
      if (grepl(pattern, filename_lower)) {
        return(dataset_type)
      }
    }
  }

  return(NULL)
}

#' Get reactiveValues key for dataset type ----
#'
#' @description Map dataset type to reactiveValues key
#' @param dataset_type Character string of dataset type
#' @return Character string of reactiveValues key or NULL
#' @export
get_reactiveValues_key <- function(dataset_type) {
  key_mapping <- list(
    "Sites" = "sitesData",
    "Parameters" = "parametersData",
    "Compartments" = "compartmentsData",
    "Reference" = "referenceData",
    "Campaign" = "campaignData",
    "Methods" = "methodsData",
    "Samples" = "samplesData",
    "Biota" = "biotaData",
    "Measurements" = "measurementsData"
  )

  return(key_mapping[[dataset_type]])
}

#' Validate dataset structure ----
#'
#' @description Basic validation of dataset structure
#' @param data Tibble/data.frame to validate
#' @param dataset_type Character string of dataset type
#' @return List with valid (logical) and message (character)
#' @export
validate_dataset_structure <- function(data, dataset_type) {
  # Basic validation - just check that it's a data.frame/tibble with columns
  if (!is.data.frame(data)) {
    return(list(
      valid = FALSE,
      message = "Data is not a tibble"
    ))
  }
  if (ncol(data) == 0) {
    return(list(
      valid = FALSE,
      message = "Data has no columns"
    ))
  }

  return(list(
    valid = TRUE,
    message = "Valid structure"
  ))
}

#' Read metadata from text file ----
#'
#' @description Read metadata from a human-readable text file
#' @param file_path Path to the metadata text file
#' @return List with metadata or NULL if not found/readable
#' @export
read_metadata_txt <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  tryCatch(
    {
      lines <- readLines(file_path)

      # Extract key information using simple pattern matching
      metadata <- list()

      for (line in lines) {
        if (grepl("^Campaign Name:", line)) {
          metadata$campaign_name <- gsub("^Campaign Name: ", "", line)
        } else if (grepl("^Export Date/Time:", line)) {
          metadata$export_datetime <- gsub("^Export Date/Time: ", "", line)
        } else if (grepl("^Exported By:", line)) {
          metadata$user <- gsub("^Exported By: ", "", line)
        } else if (grepl("^  App Version:", line)) {
          metadata$app_version <- gsub("^  App Version: ", "", line)
        }
      }

      return(metadata)
    },
    error = function(e) {
      return(NULL)
    }
  )
}
