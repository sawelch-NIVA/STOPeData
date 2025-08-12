#' Create Bibliography Reference
#'
#' @description Creates a formatted bibliographic reference from reference data
#' @param ref_data Reference data frame
#' @return Character string with formatted reference
#' @noRd
create_bibliography_reference <- function(ref_data) {
  if (is.null(ref_data) || nrow(ref_data) == 0) {
    return("Relevant data not found")
  }

  # Basic format: Author (Year). Title. Journal/Source.
  ref_parts <- c()

  if (!is.na(ref_data$AUTHOR) && ref_data$AUTHOR != "") {
    ref_parts <- c(ref_parts, ref_data$AUTHOR)
  }

  if (!is.na(ref_data$YEAR)) {
    ref_parts <- c(ref_parts, paste0("(", ref_data$YEAR, ")"))
  }

  if (!is.na(ref_data$TITLE) && ref_data$TITLE != "") {
    ref_parts <- c(ref_parts, ref_data$TITLE)
  }

  if (
    !is.na(ref_data$PERIODICAL_JOURNAL) && ref_data$PERIODICAL_JOURNAL != ""
  ) {
    ref_parts <- c(ref_parts, ref_data$PERIODICAL_JOURNAL)
  } else if (!is.na(ref_data$PUBLISHER) && ref_data$PUBLISHER != "") {
    ref_parts <- c(ref_parts, ref_data$PUBLISHER)
  }

  if (!is.na(ref_data$DOI) && ref_data$DOI != "") {
    ref_parts <- c(ref_parts, paste0("(", ref_data$DOI, ")"))
  }

  if (length(ref_parts) > 0) {
    return(paste(ref_parts, collapse = ". "))
  } else {
    return("Reference data incomplete")
  }
}

#' Summarize Multiple Values
#'
#' @description Creates a summary string from multiple values
#' @param values Vector of values to summarize
#' @param prefix Optional prefix for output
#' @param max_display Maximum items to display before truncating
#' @return Character string summary
#' @noRd
summarize_multiple <- function(values, prefix = "", max_display = 10) {
  if (is.null(values) || length(values) == 0) {
    return("Relevant data not found")
  }

  unique_values <- unique(values[!is.na(values) & values != ""])

  if (length(unique_values) == 0) {
    return("Relevant data not found")
  }

  if (length(unique_values) <= max_display) {
    result <- paste(unique_values, collapse = ", ")
  } else {
    displayed <- paste(unique_values[1:max_display], collapse = ", ")
    result <- paste0(
      displayed,
      " (and ",
      length(unique_values) - max_display,
      " more)"
    )
  }

  if (prefix != "" && length(unique_values) > 1) {
    result <- paste0(prefix, " (", length(unique_values), "): ", result)
  } else if (prefix != "") {
    result <- paste0(prefix, ": ", result)
  }

  return(result)
}

#' Calculate Date Range
#'
#' @description Creates a date range string from vector of dates
#' @param dates Vector of dates
#' @return Character string with date range
#' @noRd
calculate_date_range <- function(dates) {
  if (is.null(dates) || length(dates) == 0) {
    return("Relevant data not found")
  }

  valid_dates <- dates[!is.na(dates)]
  if (length(valid_dates) == 0) {
    return("Relevant data not found")
  }

  min_date <- min(valid_dates)
  max_date <- max(valid_dates)

  if (min_date == max_date) {
    return(as.character(min_date))
  } else {
    return(paste(min_date, "to", max_date))
  }
}

#' Generate Units Summary by Parameter
#'
#' @description Creates a summary of measurement units grouped by parameter name
#' @param measurement_data Measurement data frame
#' @param parameters_data Parameters data frame
#' @return Character string with units grouped by parameter
#' @noRd
generate_units_summary <- function(measurement_data, parameters_data) {
  if (is.null(measurement_data) || is.null(parameters_data)) {
    return("Relevant data not found")
  }

  # Join measurement data with parameter names
  merged_data <- merge(
    measurement_data,
    parameters_data,
    by = "PARAMETER_ID",
    all.x = TRUE
  )

  # Group units by parameter
  param_units <- split(
    merged_data$MEASURED_UNIT[!is.na(merged_data$MEASURED_UNIT)],
    merged_data$PARAMETER_NAME[!is.na(merged_data$MEASURED_UNIT)]
  )

  if (length(param_units) == 0) {
    return("Relevant data not found")
  }

  # Create summary for each parameter
  param_summaries <- lapply(names(param_units), function(param) {
    unique_units <- unique(param_units[[param]])
    paste0(param, ": ", paste(unique_units, collapse = ", "))
  })

  return(paste(param_summaries, collapse = "; "))
}

#' Get Dataset Details Summaries
#'
#' @description Gets all dataset detail summaries for auto-population
#' @param module_data List containing all module data
#' @return Named list of dataset summaries
#' @noRd
get_dataset_summaries <- function(module_data) {
  list(
    source = if (!is.null(module_data$references)) {
      create_bibliography_reference(module_data$references)
    } else {
      "Relevant data not found"
    },

    analytes = if (!is.null(module_data$parameters)) {
      summarize_multiple(module_data$parameters$PARAMETER_NAME, "Parameters")
    } else {
      "Relevant data not found"
    },

    medium = if (!is.null(module_data$compartments)) {
      summarize_multiple(
        module_data$compartments$ENVIRON_COMPARTMENT,
        "Compartments"
      )
    } else {
      "Relevant data not found"
    },

    study_area = if (!is.null(module_data$sites)) {
      countries <- summarize_multiple(module_data$sites$COUNTRY, "Countries")
      areas <- summarize_multiple(module_data$sites$AREA, "Areas")
      paste(countries, areas, sep = "; ")
    } else {
      "Relevant data not found"
    },

    num_sites = if (!is.null(module_data$sites)) {
      as.character(nrow(module_data$sites))
    } else {
      "Relevant data not found"
    },

    site_types = if (!is.null(module_data$sites)) {
      summarize_multiple(
        module_data$sites$SITE_GEOGRAPHICAL_FEATURE,
        "Site Types"
      )
    } else {
      "Relevant data not found"
    },

    num_samples = if (!is.null(module_data$samples)) {
      as.character(nrow(module_data$samples))
    } else {
      "Relevant data not found"
    },

    sampling_period = if (!is.null(module_data$samples)) {
      calculate_date_range(module_data$samples$SAMPLING_DATE)
    } else {
      "Relevant data not found"
    },

    sampling_methods = if (!is.null(module_data$methods)) {
      sampling_only <- module_data$methods[
        module_data$methods$PROTOCOL_CATEGORY == "Sampling Protocol",
      ]
      if (nrow(sampling_only) > 0) {
        summarize_multiple(
          sampling_only$PROTOCOL_NAME,
          "Sampling Protocols"
        )
      } else {
        "Relevant data not found"
      }
    } else {
      "Relevant data not found"
    },

    analytical_methods = if (!is.null(module_data$methods)) {
      analytical_only <- module_data$methods[
        module_data$methods$PROTOCOL_CATEGORY == "Analytical Protocol",
      ]
      if (nrow(analytical_only) > 0) {
        summarize_multiple(
          analytical_only$PROTOCOL_NAME,
          "Analytical Protocols"
        )
      } else {
        "Relevant data not found"
      }
    } else {
      "Relevant data not found"
    },

    loq_info = if (!is.null(module_data$measurements)) {
      loq_values <- module_data$measurements$LOQ_VALUE[
        !is.na(module_data$measurements$LOQ_VALUE)
      ]
      lod_values <- module_data$measurements$LOD_VALUE[
        !is.na(module_data$measurements$LOD_VALUE)
      ]

      info_parts <- c()
      if (length(loq_values) > 0) {
        loq_range <- paste(min(loq_values), "to", max(loq_values))
        loq_unit <- module_data$measurements$LOQ_UNIT[
          !is.na(module_data$measurements$LOQ_UNIT)
        ][1]
        info_parts <- c(info_parts, paste("LOQ:", loq_range, loq_unit))
      }
      if (length(lod_values) > 0) {
        lod_range <- paste(min(lod_values), "to", max(lod_values))
        lod_unit <- module_data$measurements$LOD_UNIT[
          !is.na(module_data$measurements$LOD_UNIT)
        ][1]
        info_parts <- c(info_parts, paste("LOD:", lod_range, lod_unit))
      }

      if (length(info_parts) > 0) {
        paste(info_parts, collapse = "; ")
      } else {
        "Relevant data not found"
      }
    } else {
      "Relevant data not found"
    }
  )
}

#' Get All Gateway Data Summaries
#'
#' @description Gets all gateway criteria data summaries
#' @param module_data List containing all module data
#' @return Named list of gateway summaries
#' @noRd
get_gateway_summaries <- function(module_data) {
  list(
    medium = if (!is.null(module_data$compartments)) {
      summarize_multiple(
        module_data$compartments$ENVIRON_COMPARTMENT,
        "Compartments"
      )
    } else {
      "Relevant data not found"
    },

    analyte = if (!is.null(module_data$parameters)) {
      summarize_multiple(module_data$parameters$PARAMETER_NAME, "Parameters")
    } else {
      "Relevant data not found"
    },

    location = if (!is.null(module_data$sites)) {
      countries <- summarize_multiple(module_data$sites$COUNTRY, "Countries")
      areas <- summarize_multiple(module_data$sites$AREA, "Areas")
      paste(countries, areas, sep = "; ")
    } else {
      "Relevant data not found"
    },

    year = if (!is.null(module_data$samples)) {
      calculate_date_range(module_data$samples$SAMPLING_DATE)
    } else {
      "Relevant data not found"
    },

    units = generate_units_summary(
      module_data$measurements,
      module_data$parameters
    ),

    citation = if (!is.null(module_data$references)) {
      create_bibliography_reference(module_data$references)
    } else {
      "Relevant data not found"
    }
  )
}

#' Check Gateway Criteria Availability
#'
#' @description Checks if data is available for each gateway criterion
#' @param module_data List containing all module data
#' @return Named list of TRUE/FALSE values
#' @noRd
check_gateway_availability <- function(module_data) {
  list(
    medium = !is.null(module_data$compartments) &&
      any(
        !is.na(module_data$compartments$ENVIRON_COMPARTMENT) &
          module_data$compartments$ENVIRON_COMPARTMENT != ""
      ),

    analyte = !is.null(module_data$parameters) &&
      any(
        !is.na(module_data$parameters$PARAMETER_NAME) &
          module_data$parameters$PARAMETER_NAME != ""
      ),

    location = !is.null(module_data$sites) &&
      (any(
        !is.na(module_data$sites$COUNTRY) & module_data$sites$COUNTRY != ""
      ) ||
        any(!is.na(module_data$sites$AREA) & module_data$sites$AREA != "")),

    year = !is.null(module_data$samples) &&
      any(!is.na(module_data$samples$SAMPLING_DATE)),

    units = !is.null(module_data$measurements) &&
      any(
        !is.na(module_data$measurements$MEASURED_UNIT) &
          module_data$measurements$MEASURED_UNIT != ""
      ),

    citation = !is.null(module_data$references) &&
      (any(
        !is.na(module_data$references$AUTHOR) &
          module_data$references$AUTHOR != ""
      ) ||
        any(
          !is.na(module_data$references$TITLE) &
            module_data$references$TITLE != ""
        ) ||
        any(
          !is.na(module_data$references$DOI) & module_data$references$DOI != ""
        ))
  )
}
