# Helper mapping functions for parameters ----

#' Generic string-to-vocabulary mapper using CSV lookup
#' @description Maps input strings to controlled vocabulary using pattern matching from CSV
#' @param input_value Character string to map
#' @param variable_name Name of the mapping variable to use from CSV
#' @param default_value Default return value if no matches found
#' @param case_sensitive Logical, whether pattern matching should be case sensitive
#' @importFrom readr read_csv
#' @noRd
# ! FORMAT-BASED
map_to_vocabulary <- function(
  input_value,
  variable_name,
  default_value,
  case_sensitive = FALSE
) {
  if (is.null(input_value) || input_value == "") {
    return(default_value)
  }

  mapping_table <- readr::read_csv(
    "inst/data/clean/llm_vocabulary_mappings.csv",
    show_col_types = FALSE
  )

  # Filter for the specific variable
  variable_mappings <- mapping_table[
    mapping_table$VARIABLE_NAME == variable_name,
  ]

  if (nrow(variable_mappings) == 0) {
    warning("No mappings found for variable: ", variable_name)
    return(default_value)
  }

  # Prepare input for matching
  search_value <- if (case_sensitive) input_value else tolower(input_value)

  # Sort by priority if column exists (lower numbers = higher priority)
  if ("priority" %in% names(variable_mappings)) {
    variable_mappings <- variable_mappings[order(variable_mappings$priority), ]
  }

  # Find first matching pattern
  for (i in seq_len(nrow(variable_mappings))) {
    pattern <- if (case_sensitive) {
      variable_mappings$pattern[i]
    } else {
      tolower(variable_mappings$pattern[i])
    }

    if (grepl(pattern, search_value)) {
      return(variable_mappings$output_value[i])
    }
  }

  return(default_value)
}

#' Map LLM parameter type to strict controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_parameter_type_strict <- function(param_type) {
  map_to_vocabulary(
    input_value = param_type,
    variable_name = "parameter_type",
    default_value = "Stressor"
  )
}

#' Map measured category to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_measured_category_strict <- function(category) {
  map_to_vocabulary(
    input_value = category,
    variable_name = "measured_category",
    default_value = "External"
  )
}

#' Map compartment to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_compartment_strict <- function(compartment) {
  map_to_vocabulary(
    input_value = compartment,
    variable_name = "compartment",
    default_value = "Other"
  )
}

#' Map compartment sub to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_compartment_sub_strict <- function(compartment_sub) {
  map_to_vocabulary(
    input_value = compartment_sub,
    variable_name = "compartment_sub",
    default_value = "Other"
  )
}

#' Map LLM geographic feature to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in sites module
#' @noRd
# ! FORMAT-BASED
map_geographic_feature_strict <- function(feature) {
  map_to_vocabulary(
    input_value = feature,
    variable_name = "geographic_feature",
    default_value = "Other"
  )
}

# Validation functions (these remain largely unchanged) ----

#' Validate latitude value
#' @noRd
validate_latitude <- function(lat) {
  if (is.null(lat) || is.na(lat)) {
    return(NA)
  }
  lat_num <- as.numeric(lat)
  if (is.na(lat_num) || lat_num < -90 || lat_num > 90) {
    return(NA)
  }
  return(lat_num)
}

#' Validate longitude value
#' @noRd
validate_longitude <- function(lon) {
  if (is.null(lon) || is.na(lon)) {
    return(NA)
  }
  lon_num <- as.numeric(lon)
  if (is.na(lon_num) || lon_num < -180 || lon_num > 180) {
    return(NA)
  }
  return(lon_num)
}

#' Validate Parameters Against Database
#'
#' @description Validates parameters against the chemical_parameters database
#' @param parameters_data Parameters data frame (in module format with PARAMETER_NAME, CAS_RN columns)
#' @param chemical_parameters Reference database
#' @return List with validation results and formatted text output
#' @noRd
# ! FORMAT-BASED
validate_parameters_against_database <- function(
  parameters_data,
  chemical_parameters
) {
  if (is.null(parameters_data) || nrow(parameters_data) == 0) {
    return(list(
      validation_text = "No parameters to validate.",
      has_warnings = FALSE
    ))
  }

  output_lines <- c("Parameter Database Validation Results:", "")
  has_warnings <- FALSE

  for (i in seq_len(nrow(parameters_data))) {
    param_name <- parameters_data[i, "PARAMETER_NAME"]
    cas_rn <- parameters_data[i, "CAS_RN"]

    output_lines <- c(
      output_lines,
      paste0(
        "Row ",
        i,
        ": \"",
        param_name,
        "\"",
        if (!is.na(cas_rn) && cas_rn != "") {
          paste0(" (CAS: ", cas_rn, ")")
        } else {
          ""
        }
      )
    )

    # Name validation (exact match only)
    name_found <- FALSE
    if (!is.na(param_name) && param_name != "") {
      exact_matches <- chemical_parameters[
        tolower(chemical_parameters$PARAMETER_NAME) == tolower(param_name),
      ]

      if (nrow(exact_matches) > 0) {
        name_found <- TRUE
        output_lines <- c(output_lines, "  ✓ Name found in database")

        # Suggest CAS if missing
        if (
          (is.na(cas_rn) || cas_rn == "") && !is.na(exact_matches$CAS_RN[1])
        ) {
          output_lines <- c(
            output_lines,
            paste0("  → Suggested CAS: ", exact_matches$CAS_RN[1])
          )
        }
      } else {
        output_lines <- c(output_lines, "  ⚠ Name not found in database")
        has_warnings <- TRUE
      }
    } else {
      output_lines <- c(output_lines, "  ⚠ No parameter name provided")
      has_warnings <- TRUE
    }

    # CAS validation
    cas_found <- FALSE
    if (!is.na(cas_rn) && cas_rn != "") {
      cas_matches <- chemical_parameters[
        !is.na(chemical_parameters$CAS_RN) &
          chemical_parameters$CAS_RN == cas_rn,
      ]

      if (nrow(cas_matches) > 0) {
        cas_found <- TRUE
        output_lines <- c(output_lines, "  ✓ CAS number found in database")

        # Check name-CAS consistency
        if (
          name_found &&
            !tolower(param_name) %in% tolower(cas_matches$PARAMETER_NAME)
        ) {
          output_lines <- c(
            output_lines,
            paste0(
              "  ⚠ Name-CAS mismatch. CAS ",
              cas_rn,
              " belongs to: ",
              cas_matches$PARAMETER_NAME[1]
            )
          )
          has_warnings <- TRUE
        }
      } else {
        output_lines <- c(output_lines, "  ⚠ CAS number not found in database")
        has_warnings <- TRUE
      }
    }

    # Overall status
    if (!name_found && !cas_found) {
      output_lines <- c(
        output_lines,
        "  → Recommendation: Verify parameter identity"
      )
    }

    output_lines <- c(output_lines, "") # Blank line between parameters
  }

  # Summary
  total_params <- nrow(parameters_data)
  name_matches <- sum(
    !is.na(parameters_data$PARAMETER_NAME) &
      parameters_data$PARAMETER_NAME != "" &
      sapply(1:nrow(parameters_data), function(i) {
        param_name <- parameters_data[i, "PARAMETER_NAME"]
        nrow(chemical_parameters[
          tolower(chemical_parameters$PARAMETER_NAME) == tolower(param_name),
        ]) >
          0
      })
  )

  cas_matches <- sum(
    !is.na(parameters_data$CAS_RN) &
      parameters_data$CAS_RN != "" &
      sapply(1:nrow(parameters_data), function(i) {
        cas_rn <- parameters_data[i, "CAS_RN"]
        nrow(chemical_parameters[
          !is.na(chemical_parameters$CAS_RN) &
            chemical_parameters$CAS_RN == cas_rn,
        ]) >
          0
      })
  )

  output_lines <- c(output_lines, "Summary:")
  output_lines <- c(
    output_lines,
    paste0("  Parameters processed: ", total_params)
  )
  output_lines <- c(
    output_lines,
    paste0("  Names found in database: ", name_matches, "/", total_params)
  )
  output_lines <- c(
    output_lines,
    paste0("  CAS numbers found in database: ", cas_matches, "/", total_params)
  )

  if (has_warnings) {
    output_lines <- c(output_lines, "")
    output_lines <- c(
      output_lines,
      "⚠ Some parameters need review. Check suggestions above."
    )
  } else {
    output_lines <- c(output_lines, "")
    output_lines <- c(output_lines, "✓ All parameters found in database!")
  }

  return(list(
    validation_text = paste(output_lines, collapse = "\n"),
    has_warnings = has_warnings
  ))
}
