# Sample Helper Functions ----
# Utility functions for the samples module

#' Update Sites Selectize Input ----
#' @param session Shiny session
#' @param sites_data Data frame with SITE_CODE and SITE_NAME columns
#' @noRd
update_sites_selectize <- function(session, sites_data) {
  if (is.null(sites_data) || nrow(sites_data) == 0) {
    choices <- character(0)
    placeholder <- "No sites available - add sites first"
  } else {
    # Validate expected columns exist
    if (!all(c("SITE_CODE", "SITE_NAME") %in% names(sites_data))) {
      warning("Sites data missing expected columns: SITE_CODE, SITE_NAME")
      choices <- character(0)
      placeholder <- "Invalid sites data format"
    } else {
      choices <- setNames(
        sites_data$SITE_CODE,
        paste(sites_data$SITE_CODE, "-", sites_data$SITE_NAME)
      )
      placeholder <- "Select sites..."
    }
  }

  updateSelectizeInput(
    session,
    "sites_select",
    choices = choices,
    options = list(placeholder = placeholder)
  )
}

#' Update Parameters Selectize Input ----
#' @param session Shiny session
#' @param parameters_data Data frame with PARAMETER_NAME and STRESSOR_TYPE columns
#' @noRd
update_parameters_selectize <- function(session, parameters_data) {
  if (is.null(parameters_data) || nrow(parameters_data) == 0) {
    choices <- character(0)
    placeholder <- "No parameters available - add parameters first"
  } else {
    # Validate expected columns exist
    if (!all(c("PARAMETER_NAME", "STRESSOR_TYPE") %in% names(parameters_data))) {
      warning("Parameters data missing expected columns: PARAMETER_NAME, STRESSOR_TYPE")
      choices <- character(0)
      placeholder <- "Invalid parameters data format"
    } else {
      choices <- setNames(
        parameters_data$PARAMETER_NAME,
        paste(
          parameters_data$PARAMETER_NAME,
          paste0("(", parameters_data$STRESSOR_TYPE, ")")
        )
      )
      placeholder <- "Select parameters..."
    }
  }

  updateSelectizeInput(
    session,
    "parameters_select",
    choices = choices,
    options = list(placeholder = placeholder)
  )
}

#' Update Compartments Selectize Input ----
#' @param session Shiny session
#' @param compartments_data Data frame with compartment columns
#' @noRd
update_compartments_selectize <- function(session, compartments_data) {
  if (is.null(compartments_data) || nrow(compartments_data) == 0) {
    choices <- character(0)
    placeholder <- "No compartments available - add compartments first"
  } else {
    # Validate expected columns exist
    expected_cols <- c("ENVIRON_COMPARTMENT", "ENVIRON_COMPARTMENT_SUB", "MEASURED_CATEGORY")
    if (!all(expected_cols %in% names(compartments_data))) {
      warning("Compartments data missing expected columns:",
              paste(setdiff(expected_cols, names(compartments_data)), collapse = ", "))
      choices <- character(0)
      placeholder <- "Invalid compartments data format"
    } else {
      comp_values <- paste(
        compartments_data$ENVIRON_COMPARTMENT,
        compartments_data$ENVIRON_COMPARTMENT_SUB,
        sep = " | "
      )
      comp_labels <- paste(
        compartments_data$ENVIRON_COMPARTMENT,
        "→",
        compartments_data$ENVIRON_COMPARTMENT_SUB,
        "→",
        compartments_data$MEASURED_CATEGORY
      )
      choices <- setNames(comp_values, comp_labels)
      placeholder <- "Select compartments..."
    }
  }

  updateSelectizeInput(
    session,
    "compartments_select",
    choices = choices,
    options = list(placeholder = placeholder)
  )
}

#' Generate Sample ID ----
#' @param site_code Site code
#' @param parameter_name Parameter name
#' @param compartment Compartment
#' @param date Sampling date
#' @noRd
generate_sample_id <- function(site_code, parameter_name, compartment, date) {
  paste(
    site_code,
    substr(gsub("[^A-Za-z0-9]", "", parameter_name), 1, 10),
    substr(gsub("[^A-Za-z0-9]", "", compartment), 1, 10),
    gsub("-", "", date),
    sep = "_"
  )
}

#' Check if Sample Combination Exists ----
#' @param existing_data Existing samples data frame
#' @param site Site code
#' @param parameter Parameter name
#' @param compartment Compartment
#' @param date Sampling date
#' @noRd
combination_exists <- function(existing_data, site, parameter, compartment, date) {
  if (nrow(existing_data) == 0) return(FALSE)

  any(
    existing_data$SITE_CODE == site &
      existing_data$PARAMETER_NAME == parameter &
      existing_data$COMPARTMENT == compartment &
      existing_data$SAMPLING_DATE == as.character(date)
  )
}

#' Create Sample Combinations with Duplicate Checking ----
#' @description
#' Create all valid combinations of sites, parameters, compartments, and dates,
#' except for those already found in existing_data
#'
#' @param sites Vector of site codes
#' @param parameters Vector of parameter names
#' @param compartments Vector of compartments
#' @param dates Vector of sampling dates
#' @param existing_data Existing samples data frame
#' @noRd
create_sample_combinations <- function(sites, parameters, compartments, dates, existing_data) {
  stopifnot(isTruthy(sites), isTruthy(parameters), isTruthy(compartments), isTruthy(dates))
  all_combinations <- data.frame()
  skipped_count <- 0

  for (date in dates) {
    date_combinations <- expand.grid(
      SITE_CODE = sites,
      PARAMETER_NAME = parameters,
      COMPARTMENT = compartments,
      SAMPLING_DATE = as.character(date),
      stringsAsFactors = FALSE
    )

    # Filter out existing combinations
    new_combinations <- data.frame()
    for (i in 1:nrow(date_combinations)) {
      if (!combination_exists(
        existing_data,
        date_combinations$SITE_CODE[i],
        date_combinations$PARAMETER_NAME[i],
        date_combinations$COMPARTMENT[i],
        date_combinations$SAMPLING_DATE[i]
      )) {
        new_combinations <- rbind(new_combinations, date_combinations[i, ])
      } else {
        skipped_count <- skipped_count + 1
      }
    }

    all_combinations <- rbind(all_combinations, new_combinations)
  }

  if (nrow(all_combinations) > 0) {
    # Add additional columns
    all_combinations$SITE_NAME <- ""
    all_combinations$PARAMETER_TYPE <- ""
    all_combinations$COMPARTMENT_SUB <- ""
    all_combinations$SAMPLE_ID <- mapply(
      generate_sample_id,
      all_combinations$SITE_CODE,
      all_combinations$PARAMETER_NAME,
      all_combinations$COMPARTMENT,
      all_combinations$SAMPLING_DATE
    )

    # Reorder columns
    all_combinations <- all_combinations[, c(
      "SITE_CODE", "SITE_NAME", "PARAMETER_NAME", "PARAMETER_TYPE",
      "COMPARTMENT", "COMPARTMENT_SUB", "SAMPLING_DATE", "SAMPLE_ID"
    )]
  }

  return(list(combinations = all_combinations, skipped = skipped_count))
}

#' Update Combination Preview ----
#' @param sites_count Number of selected sites
#' @param params_count Number of selected parameters
#' @param comps_count Number of selected compartments
#' @param dates_count Number of selected dates
#' @noRd
update_combination_preview <- function(sites_count, params_count, comps_count, dates_count) {
  total_combinations <- sites_count * params_count * comps_count * dates_count

  div(
    strong("Preview: "),
    sprintf(
      "%d sites × %d parameters × %d compartments × %d dates = %d total combinations",
      sites_count, params_count, comps_count, dates_count, total_combinations
    )
  )
}

#' Initialize Empty Samples Data Frame ----
#' @noRd
init_samples_df <- function() {
  data.frame(
    SITE_CODE = character(0),
    SITE_NAME = character(0),
    PARAMETER_NAME = character(0),
    PARAMETER_TYPE = character(0),
    COMPARTMENT = character(0),
    COMPARTMENT_SUB = character(0),
    SAMPLING_DATE = character(0),
    SAMPLE_ID = character(0),
    stringsAsFactors = FALSE
  )
}

#' dummy_sites ----
#' @noRd
## Dummy data for standalone testing ----
dummy_sites <- data.frame(
  SITE_CODE = c("SITE_001", "SITE_002", "SITE_003"),
  SITE_NAME = c("River Site A", "Lake Site B", "Coastal Site C"),
  stringsAsFactors = FALSE
)

#' dummy_parameters ----
#' @noRd
## Dummy data for standalone testing ----
dummy_parameters <- data.frame(
  STRESSOR_NAME = c("Copper", "Lead", "pH", "Dissolved oxygen"),
  STRESSOR_TYPE = c(
    "Stressor",
    "Stressor",
    "Quality parameter",
    "Quality parameter"
  ),
  stringsAsFactors = FALSE
)

#' dummy_compartments ----
#' @noRd
## Dummy data for standalone testing ----
dummy_compartments <- data.frame(
  ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Terrestrial"),
  ENVIRON_COMPARTMENT_SUB = c(
    "Freshwater",
    "Marine/Salt Water",
    "Soil A Horizon (Topsoil)"
  ),
  MEASURED_CATEGORY = c("External", "External", "External"),
  stringsAsFactors = FALSE
)
