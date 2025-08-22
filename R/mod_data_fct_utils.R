#' Read in ecotoxicological units and conversion factors from csv
#'
#' @param select_column name of column to pull ("MEASURED_UNIT", "BASE_SI_UNIT", "CONVERSION_FACTOR", "UNIT_COMMENTS")
#'
#' @returns a dataframe or a character vector
#'
#' @export
#' @importFrom readr read_csv
parameter_units <- function(select_column = NULL) {
  units <- read_csv(
    file = "inst/data/clean/unit_conversion_factors.csv",
    col_names = TRUE,
    show_col_types = FALSE
  )
  if (is.null(select_column)) {
    return(units)
  }
  stopifnot(select_column %in% names(units))
  return(units[[select_column]])
}

#' Check if all required modules are validated
#'
#' @description Checks whether all upstream modules have valid data for enabling measurement data entry
#'
#' @param session Shiny session object containing userData$reactiveValues
#'
#' @returns logical indicating whether all required modules are validated
#'
#' @importFrom golem print_dev
check_all_modules_valid <- function(session) {
  required_data <- list(
    campaign = session$userData$reactiveValues$campaignData,
    references = session$userData$reactiveValues$referencesData,
    sites = session$userData$reactiveValues$sitesData,
    parameters = session$userData$reactiveValues$parametersData,
    compartments = session$userData$reactiveValues$compartmentsData,
    methods = session$userData$reactiveValues$methodsData,
    samples = session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
      session$userData$reactiveValues$sampleData
  )

  all(sapply(required_data, function(x) !is.null(x)))
}

#' Get validation status for each module
#'
#' @description Returns a list containing validation status and record counts for each module
#'
#' @param session Shiny session object containing userData$reactiveValues
#'
#' @returns list with module name, status, and count for each module

get_module_status <- function(session) {
  modules <- list(
    Campaign = session$userData$reactiveValues$campaignData,
    References = session$userData$reactiveValues$referencesData,
    Sites = session$userData$reactiveValues$sitesData,
    Parameters = session$userData$reactiveValues$parametersData,
    Compartments = session$userData$reactiveValues$compartmentsData,
    Methods = session$userData$reactiveValues$methodsData,
    Samples = session$userData$reactiveValues$sampleData,
    Biota = session$userData$reactiveValues$biotaValidated
  )

  status_list <- lapply(names(modules), function(name) {
    data <- modules[[name]]
    if (name == "Biota") {
      # Special handling for biota validation flag
      status <- if (isTruthy(data)) "✓ Validated" else "⚠ No biota samples"
      count <- if (isTruthy(session$userData$reactiveValues$biotaData)) {
        nrow(session$userData$reactiveValues$biotaData)
      } else {
        "No biota samples"
      }
    } else {
      status <- if (isTruthy(data) && nrow(data) > 0) {
        "✓ Validated"
      } else {
        "⚠ Pending"
      }
      count <- if (isTruthy(data)) {
        nrow(data)
      } else {
        0
      }
    }

    list(module = name, status = status, count = count)
  })

  return(status_list)
}

#' Create sample-parameter combinations for measurement entry
#'
#' @description Creates cartesian product of validated samples and parameters for measurement data entry
#'
#' @param session Shiny session object containing userData$reactiveValues
#'
#' @returns data.frame with sample-parameter combinations and empty measurement fields
#'
#' @importFrom dplyr cross_join mutate select

# ! FORMAT-BASED
create_measurement_combinations <- function(session) {
  # Get all validated data
  samples_data <- session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
    session$userData$reactiveValues$sampleData
  parameters_data <- session$userData$reactiveValues$parametersData
  campaign_data <- session$userData$reactiveValues$campaignData
  reference_data <- session$userData$reactiveValues$referencesData

  if (is.null(samples_data) || is.null(parameters_data)) {
    return(data.frame())
  }

  # Create cartesian product of samples and parameters
  combinations <- cross_join(
    samples_data |>
      select(
        SAMPLE_ID,
        SITE_CODE,
        SAMPLING_DATE,
        ENVIRON_COMPARTMENT,
        ENVIRON_COMPARTMENT_SUB,
        REP
      ),
    parameters_data |> select(PARAMETER_NAME, PARAMETER_TYPE, MEASURED_TYPE)
  )

  # Add campaign and reference info
  combinations <- combinations |>
    mutate(
      REFERENCE_ID = reference_data$REFERENCE_TYPE, # Simplified for now

      # Add measurement fields with empty defaults
      MEASURED_FLAG = "",
      MEASURED_VALUE = NA,
      MEASURED_SD = NA,
      MEASURED_UNIT = "mg/L", # Default unit
      LOQ_VALUE = NA,
      LOQ_UNIT = "mg/L", # Should match MEASURED_UNIT
      LOD_VALUE = NA,
      LOD_UNIT = "mg/L", # Should match MEASURED_UNIT

      # Add method info (simplified)
      SAMPLING_PROTOCOL = "Not reported",
      FRACTIONATION_PROTOCOL = "Not reported",
      EXTRACTION_PROTOCOL = "Not reported",
      ANALYTICAL_PROTOCOL = "Not reported"
    )

  return(combinations)
}

#' Initialize empty measurement data frame
#'
#' @description Creates an empty tibble with the correct column structure for measurement data
#'
#' @returns tibble with zero rows but correct column structure for measurement data
#'
#' @importFrom tibble tibble

# ! FORMAT-BASED
init_measurement_df <- function() {
  tibble(
    SAMPLE_ID = character(0),
    SITE_CODE = character(0),
    PARAMETER_NAME = character(0),
    SAMPLING_DATE = character(0),
    ENVIRON_COMPARTMENT = character(0),
    ENVIRON_COMPARTMENT_SUB = character(0),
    REP = integer(0),
    MEASURED_FLAG = character(0),
    MEASURED_VALUE = numeric(0),
    MEASURED_SD = numeric(0),
    MEASURED_UNIT = character(0),
    LOQ_VALUE = numeric(0),
    LOQ_UNIT = character(0),
    LOD_VALUE = numeric(0),
    LOD_UNIT = character(0),
    SAMPLING_PROTOCOL = character(0),
    EXTRACTION_PROTOCOL = character(0),
    FRACTIONATION_PROTOCOL = character(0),
    ANALYTICAL_PROTOCOL = character(0)
  )
}
