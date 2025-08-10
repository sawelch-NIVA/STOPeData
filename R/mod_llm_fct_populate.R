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

## mod_sites helper functions ----

#' Map LLM geographic feature to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in sites module
#' @noRd
# ! FORMAT-BASED
map_geographic_feature_strict <- function(feature) {
  if (is.null(feature) || feature == "") {
    return("Not reported")
  }

  feature_lower <- tolower(feature)

  # Exact mappings to controlled vocabulary
  if (grepl("river|stream|canal", feature_lower)) {
    return("River, stream, canal")
  }
  if (grepl("lake|pond|pool|reservoir", feature_lower)) {
    return("Lake, pond, pool, reservoir")
  }
  if (grepl("ocean|sea|territorial", feature_lower)) {
    return("Ocean, sea, territorial waters")
  }
  if (grepl("coast|fjord", feature_lower)) {
    return("Coastal, fjord")
  }
  if (grepl("estuary", feature_lower)) {
    return("Estuary")
  }
  if (grepl("drainage|sewer|artificial", feature_lower)) {
    return("Drainage, sewer, artificial water")
  }
  if (grepl("swamp|wetland", feature_lower)) {
    return("Swamp, wetland")
  }
  if (grepl("ground|aquifer", feature_lower)) {
    return("Groundwater, aquifer")
  }
  if (grepl("wwtp|wastewater.*treatment", feature_lower)) {
    return("WWTP")
  }
  if (grepl("urban|artificial.*land", feature_lower)) {
    return("Artificial Land/Urban Areas")
  }
  if (grepl("landfill", feature_lower)) {
    return("Landfills")
  }
  if (grepl("crop|farm|agricultural", feature_lower)) {
    return("Cropland")
  }
  if (grepl("forest|wood", feature_lower)) {
    return("Woodland, forest")
  }
  if (grepl("shrub", feature_lower)) {
    return("Shrubland")
  }
  if (grepl("grass", feature_lower)) {
    return("Grassland")
  }
  if (grepl("bare.*land|lichen|moss", feature_lower)) {
    return("Bare land and lichen/moss")
  }

  return("Other")
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


#' Map LLM parameter type to controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_parameter_type <- function(param_type) {
  if (is.null(param_type) || param_type == "") {
    return("Stressor")
  }

  type_lower <- tolower(param_type)

  if (grepl("stress", type_lower)) {
    return("Stressor")
  }
  if (grepl("quality", type_lower)) {
    return("Quality parameter")
  }
  if (grepl("normal", type_lower)) {
    return("Normalization")
  }
  if (grepl("background", type_lower)) {
    return("Background")
  }

  return("Stressor") # Default assumption
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

# Helper mapping functions for parameters ----

#' Map LLM parameter type to strict controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_parameter_type_strict <- function(param_type) {
  if (is.null(param_type) || param_type == "") {
    return("Stressor")
  }

  type_lower <- tolower(param_type)

  if (grepl("stress", type_lower)) {
    return("Stressor")
  }
  if (grepl("quality", type_lower)) {
    return("Quality parameter")
  }
  if (grepl("normal", type_lower)) {
    return("Normalization")
  }
  if (grepl("background", type_lower)) {
    return("Background")
  }

  return("Stressor") # Default assumption
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

## mod_compartment helper functions ----

#' Map compartment to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_compartment_strict <- function(compartment) {
  if (is.null(compartment) || compartment == "") {
    return("Not reported")
  }

  comp_lower <- tolower(compartment)

  if (grepl("aquatic|water", comp_lower)) {
    return("Aquatic")
  }
  if (grepl("atmosph|air", comp_lower)) {
    return("Atmospheric")
  }
  if (grepl("terrestrial|soil|land", comp_lower)) {
    return("Terrestrial")
  }
  if (grepl("biota|organism", comp_lower)) {
    return("Biota")
  }

  return("Other")
}

#' Map compartment sub to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_compartment_sub_strict <- function(compartment_sub) {
  if (is.null(compartment_sub) || compartment_sub == "") {
    return("Not reported")
  }

  sub_lower <- tolower(compartment_sub)

  # Aquatic sub-compartments
  if (grepl("fresh", sub_lower)) {
    return("Freshwater")
  }
  if (grepl("marine|salt|sea", sub_lower)) {
    return("Marine/Salt Water")
  }
  if (grepl("brackish", sub_lower)) {
    return("Brackish/Transitional Water")
  }
  if (grepl("ground", sub_lower)) {
    return("Groundwater")
  }
  if (grepl("waste", sub_lower)) {
    return("Wastewater")
  }
  if (grepl("growth.*medium", sub_lower)) {
    return("Liquid Growth Medium")
  }
  if (grepl("rain", sub_lower)) {
    return("Rainwater")
  }
  if (grepl("storm", sub_lower)) {
    return("Stormwater")
  }
  if (grepl("leachate", sub_lower)) {
    return("Leachate")
  }

  # Atmospheric sub-compartments
  if (grepl("indoor", sub_lower)) {
    return("Indoor Air")
  }
  if (grepl("outdoor", sub_lower)) {
    return("Outdoor Air")
  }

  # Terrestrial sub-compartments
  if (grepl("biological.*residue", sub_lower)) {
    return("Terrestrial Biological Residue")
  }
  if (grepl("h.*horizon|peat", sub_lower)) {
    return("Soil H Horizon (Peat)")
  }
  if (grepl("o.*horizon|organic", sub_lower)) {
    return("Soil O Horizon (Organic)")
  }
  if (grepl("a.*horizon|topsoil", sub_lower)) {
    return("Soil A Horizon (Topsoil)")
  }
  if (grepl("e.*horizon.*mineral", sub_lower)) {
    return("Soil E Horizon (Mineral)")
  }
  if (grepl("s.*horizon.*mineral", sub_lower)) {
    return("Soil S Horizon (Mineral)")
  }
  if (grepl("c.*horizon|parent.*material", sub_lower)) {
    return("Soil C Horizon (Parent Material)")
  }
  if (grepl("r.*horizon|bedrock", sub_lower)) {
    return("Soil R Horizon (Bedrock)")
  }

  # Biota sub-compartments
  if (grepl("biota.*terrestrial", sub_lower)) {
    return("Biota, Terrestrial")
  }
  if (grepl("biota.*aquatic", sub_lower)) {
    return("Biota, Aquatic")
  }
  if (grepl("biota.*atmospheric", sub_lower)) {
    return("Biota, Atmospheric")
  }
  if (grepl("biota.*other", sub_lower)) {
    return("Biota, Other")
  }

  return("Other")
}

#' Map measured category to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_measured_category_strict <- function(category) {
  if (is.null(category) || category == "") {
    return("External")
  }

  cat_lower <- tolower(category)

  if (grepl("external", cat_lower)) {
    return("External")
  }
  if (grepl("internal", cat_lower)) {
    return("Internal")
  }
  if (grepl("surface", cat_lower)) {
    return("Surface")
  }

  return("External") # Default assumption
}

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
