# LLM Form Population Helper Functions
# Functions to populate module forms from LLM extracted data

# mod_campaign ----

#' Populate Campaign Form from LLM Data
#'
#' @description Updates campaign module input fields with LLM extracted data
#' @param session Shiny session object
#' @param llm_campaign_data Campaign data extracted by LLM
#' @noRd
#' @importFrom shiny updateTextInput updateDateInput updateSelectInput updateTextAreaInput
# ! FORMAT-BASED
populate_campaign_from_llm <- function(session, llm_campaign_data) {
  if (is.null(llm_campaign_data)) {
    return()
  }
  # Update campaign fields with extracted data
  if (!is.null(llm_campaign_data$campaign_name)) {
    updateTextInput(
      session,
      "CAMPAIGN_NAME",
      value = llm_campaign_data$campaign_name
    )
  }

  if (!is.null(llm_campaign_data$campaign_start_date)) {
    # Validate and parse date
    tryCatch(
      {
        start_date <- as.Date(llm_campaign_data$campaign_start_date)
        if (!is.na(start_date)) {
          updateDateInput(
            session,
            "CAMPAIGN_START_DATE",
            value = start_date
          )
        }
      },
      error = function(e) {
        print_dev(paste(
          "Invalid start date from LLM:",
          llm_campaign_data$campaign_start_date
        ))
      }
    )
  }

  if (!is.null(llm_campaign_data$campaign_end_date)) {
    tryCatch(
      {
        end_date <- as.Date(llm_campaign_data$campaign_end_date)
        if (!is.na(end_date)) {
          updateDateInput(
            session,
            "CAMPAIGN_END_DATE",
            value = end_date
          )
        }
      },
      error = function(e) {
        print_dev(paste(
          "Invalid end date from LLM:",
          llm_campaign_data$campaign_end_date
        ))
      }
    )
  }

  if (!is.null(llm_campaign_data$organisation)) {
    updateTextInput(
      session,
      "ORGANISATION",
      value = llm_campaign_data$organisation
    )
  }

  if (!is.null(llm_campaign_data$campaign_comment)) {
    updateTextAreaInput(
      session,
      "CAMPAIGN_COMMENT",
      value = llm_campaign_data$campaign_comment
    )
  }

  # Set today's date for ENTERED_DATE and current user for ENTERED_BY
  updateDateInput(session, "ENTERED_DATE", value = Sys.Date())

  print_dev("Campaign form populated from LLM data")
}

# mod_references ----

#' Populate References Form from LLM Data
#'
#' @description Updates references module input fields with LLM extracted data
#' @param session Shiny session object
#' @param llm_references_data References data extracted by LLM
#' @noRd
# ! FORMAT-BASED
populate_references_from_llm <- function(session, llm_references_data) {
  if (is.null(llm_references_data)) {
    return()
  }
  # Determine reference type based on available fields
  ref_type <- "journal" # Default
  if (!is.null(llm_references_data$periodical_journal)) {
    ref_type <- "journal"
  } else if (!is.null(llm_references_data$publisher)) {
    ref_type <- "book"
  }

  updateSelectInput(
    session,
    "REFERENCE_TYPE",
    selected = ref_type
  )

  if (!is.null(llm_references_data$author)) {
    updateTextAreaInput(
      session,
      "AUTHOR",
      value = llm_references_data$author
    )
  }

  if (!is.null(llm_references_data$title)) {
    updateTextAreaInput(
      session,
      "TITLE",
      value = llm_references_data$title
    )
  }

  if (!is.null(llm_references_data$year)) {
    # Validate year is within acceptable range
    year <- as.numeric(llm_references_data$year)
    if (!is.na(year) && year >= 1800 && year <= 2026) {
      updateNumericInput(
        session,
        "YEAR",
        value = year
      )
    }
  }

  if (!is.null(llm_references_data$periodical_journal)) {
    updateTextInput(
      session,
      "PERIODICAL_JOURNAL",
      value = llm_references_data$periodical_journal
    )
  }

  if (!is.null(llm_references_data$volume)) {
    updateNumericInput(
      session,
      "VOLUME",
      value = as.numeric(llm_references_data$volume)
    )
  }

  if (!is.null(llm_references_data$issue)) {
    updateNumericInput(
      session,
      "ISSUE",
      value = as.numeric(llm_references_data$issue)
    )
  }

  if (!is.null(llm_references_data$publisher)) {
    updateTextInput(
      session,
      "PUBLISHER",
      value = llm_references_data$publisher
    )
  }

  if (!is.null(llm_references_data$doi)) {
    updateTextInput(
      session,
      "DOI",
      value = llm_references_data$doi
    )
  }

  # Set today's date for ACCESS_DATE
  updateDateInput(session, "ACCESS_DATE", value = Sys.Date())

  print_dev("References form populated from LLM data")
}

# mod_sites ----

#' Populate Sites Data from LLM Data
#'
#' @description Creates sites data frame from LLM extracted sites data frame
#' @param llm_sites_data Sites data frame extracted by LLM
#' @return Data frame in sites module format
#' @noRd
# ! FORMAT-BASED
create_sites_from_llm <- function(llm_sites_data) {
  if (is.null(llm_sites_data) || nrow(llm_sites_data) == 0) {
    return(data.frame())
  }

  sites_df <- data.frame()

  # Process each row of the sites data frame
  for (i in seq_len(nrow(llm_sites_data))) {
    site <- llm_sites_data[i, ]

    # Create site row with LLM data, filling in defaults where needed
    site_row <- data.frame(
      SITE_CODE = safe_extract_field(
        site,
        "site_code",
        paste0("SITE_", sprintf("%03d", i))
      ),
      SITE_NAME = safe_extract_field(site, "site_name", ""),
      SITE_GEOGRAPHIC_FEATURE = map_geographic_feature_strict(safe_extract_field(
        site,
        "site_geographic_feature",
        ""
      )),
      SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported", # LLM doesn't extract this level of detail
      SITE_COORDINATE_SYSTEM = "WGS 84", # Assume WGS84 for LLM coordinates
      LATITUDE = validate_latitude(safe_extract_field(
        site,
        "latitude",
        NA
      )),
      LONGITUDE = validate_longitude(safe_extract_field(
        site,
        "longitude",
        NA
      )),
      COUNTRY = safe_extract_field(site, "country", "Not reported"),
      AREA = "Not reported", # LLM doesn't typically extract area info
      ALTITUDE_VALUE = 0, # Default altitude, LLM rarely has this
      ALTITUDE_UNIT = "m",
      ENTERED_BY = "", # Will be filled by module
      ENTERED_DATE = as.character(Sys.Date()),
      SITE_COMMENT = "",
      stringsAsFactors = FALSE
    )

    sites_df <- rbind(sites_df, site_row)
  }

  print_dev(glue("Created {nrow(sites_df)} sites from LLM data"))
  return(sites_df)
}


# mod_parameters ----

#' Create Parameters Data from LLM with Database Lookup
#'
#' @description Creates parameters data frame from LLM extracted parameters with database lookup
#' @param llm_parameters_data Parameters data frame extracted by LLM
#' @param chemical_parameters Reference database for lookups (optional)
#' @return Data frame in parameters module format
#' @noRd
# ! FORMAT-BASED
create_parameters_from_llm <- function(
  llm_parameters_data,
  chemical_parameters = NULL
) {
  if (is.null(llm_parameters_data) || nrow(llm_parameters_data) == 0) {
    return(data.frame())
  }

  params_df <- data.frame()

  # Process each row of the parameters data frame
  for (i in seq_len(nrow(llm_parameters_data))) {
    param <- llm_parameters_data[i, ]

    param_name <- safe_extract_field(param, "parameter_name", "")
    cas_rn <- safe_extract_field(param, "cas_rn", "")

    # Try to get data from chemical database
    db_match <- NULL
    if (!is.null(chemical_parameters) && param_name != "") {
      # Try exact name match first
      exact_match <- chemical_parameters[
        tolower(chemical_parameters$PARAMETER_NAME) == tolower(param_name),
      ]
      if (nrow(exact_match) > 0) {
        db_match <- exact_match[1, ] # Take first match
      }
    }

    # If no name match but we have a CAS, try CAS lookup
    if (is.null(db_match) && !is.null(chemical_parameters) && cas_rn != "") {
      cas_match <- chemical_parameters[
        !is.na(chemical_parameters$CAS_RN) &
          chemical_parameters$CAS_RN == cas_rn,
      ]
      if (nrow(cas_match) > 0) {
        db_match <- cas_match[1, ] # Take first match
      }
    }

    # Build parameter row
    param_row <- data.frame(
      PARAMETER_TYPE = if (!is.null(db_match)) {
        db_match$PARAMETER_TYPE
      } else {
        map_parameter_type_strict(safe_extract_field(
          param,
          "parameter_type",
          ""
        ))
      },
      PARAMETER_TYPE_SUB = if (!is.null(db_match)) {
        db_match$PARAMETER_TYPE_SUB
      } else {
        "Not reported"
      },
      MEASURED_TYPE = if (!is.null(db_match)) {
        db_match$MEASURED_TYPE
      } else {
        "Concentration"
      },
      PARAMETER_NAME = param_name,
      PARAMETER_NAME_SUB = "",
      INCHIKEY_SD = if (!is.null(db_match)) db_match$INCHIKEY_SD else "",
      PUBCHEM_CID = if (!is.null(db_match)) db_match$PUBCHEM_CID else "",
      CAS_RN = cas_rn,
      ENTERED_BY = "",
      stringsAsFactors = FALSE
    )

    params_df <- rbind(params_df, param_row)
  }

  print_dev(glue("Created {nrow(params_df)} parameters from LLM data"))
  return(params_df)
}

## mod_parameters helper functions ----

#' Safely extract field from LLM data object
#' @param data_obj The data object (list or atomic)
#' @param field_name Name of field to extract
#' @param default Default value if field missing or null
#' @noRd
safe_extract_field <- function(data_obj, field_name, default = NA) {
  tryCatch(
    {
      if (is.list(data_obj) && field_name %in% names(data_obj)) {
        value <- data_obj[[field_name]]
        if (
          is.null(value) || is.na(value) || (is.character(value) && value == "")
        ) {
          return(default)
        }
        return(value)
      } else {
        return(default)
      }
    },
    error = function(e) {
      return(default)
    }
  )
}

# mod_compartment ----

#' Populate Compartments Data from LLM Data
#'
#' @description Creates compartments data frame from LLM extracted compartments data frame
#' @param llm_compartments_data Compartments data frame extracted by LLM
#' @return Data frame in compartments module format
#' @noRd
# ! FORMAT-BASED
create_compartments_from_llm <- function(llm_compartments_data) {
  if (is.null(llm_compartments_data) || nrow(llm_compartments_data) == 0) {
    return(data.frame())
  }

  comps_df <- data.frame()

  # Process each row of the compartments data frame
  for (i in seq_len(nrow(llm_compartments_data))) {
    comp <- llm_compartments_data[i, ]

    comp_row <- data.frame(
      ENVIRON_COMPARTMENT = map_compartment_strict(safe_extract_field(
        comp,
        "environ_compartment",
        ""
      )),
      ENVIRON_COMPARTMENT_SUB = map_compartment_sub_strict(safe_extract_field(
        comp,
        "environ_compartment_sub",
        ""
      )),
      MEASURED_CATEGORY = map_measured_category_strict(safe_extract_field(
        comp,
        "measured_category",
        ""
      )),
      stringsAsFactors = FALSE
    )

    comps_df <- rbind(comps_df, comp_row)
  }

  print_dev(glue("Created {nrow(comps_df)} compartments from LLM data"))
  return(comps_df)
}
