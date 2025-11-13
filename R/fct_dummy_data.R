# Dummy Data Creation Functions ----
# Functions for creating test/demonstration data

#' Create dummy data for testing and demonstration
#'
#' @description Creates a complete set of dummy environmental exposure data
#' that can be used for testing the application or demonstration purposes.
#' This includes campaign info, references, sites, parameters, compartments,
#' biota, and methods data. This returns a list because that's the format we
#' expect to get structured data back from the LLM in, even though we prefer
#' tibbles
#'
#' @param uppercase_columns Logical. If TRUE, converts data frame column names
#'   to uppercase for app data structures. If FALSE, keeps lowercase for LLM extraction.
#'
#' @importFrom tibble tibble is_tibble
#' @return List containing all dummy data structures
#' @export
create_dummy_data <- function(uppercase_columns = FALSE) {
  dummy_data <- list(
    campaign = tibble(
      # TODO: Actually, should this be a tibble? Need to coordinate with other stuff.
      campaign_name = "Dummy campaign, 1997",
      campaign_name_short = "DummyCampaign1997",
      campaign_start_date = "1997-01-01",
      campaign_end_date = "1997-03-31",
      organisation = "NIVA",
      campaign_comment = "A madeup NIVA study."
    ),
    references = tibble(
      author = "Welch, S.",
      title = "Study madeup",
      year = 1998L,
      periodical_journal = "Journal of NIVA",
      volume = 43L,
      issue = 2L,
      publisher = "NIVA Library",
      doi = NA_character_
    ),
    sites = tibble(
      site_code = "NIVA-001",
      site_name = "NIVA Office",
      latitude = 59.924634,
      longitude = 10.792297,
      country = "Norway",
      site_geographic_feature = "Coastal, fjord",
      site_geographic_feature_sub = "Water column"
    ),
    parameters = tibble(
      parameter_name = c("Silver"),
      parameter_type = rep("Stressor", 1),
      cas_rn = c(
        "7440-22-4"
      )
    ),
    compartments = tibble(
      environ_compartment = c("Aquatic", "Biota"),
      environ_compartment_sub = c("Marine/Salt Water", "Biota, Aquatic"),
      measured_category = c("External", "Internal")
    ),
    biota = tibble(
      sample_id = NA_character_,
      species_group = "Crustaceans",
      sample_species = "Daphnia magna",
      sample_tissue = "Whole body",
      sample_species_lifestage = "Adult",
      sample_species_gender = "Female"
    ),
    methods = tibble(
      protocol_category = c(
        "Sampling Protocol",
        "Analytical Protocol",
        "Extraction Protocol"
      ),
      protocol_name = c(
        "Grab sampling",
        "Other",
        "Not reported"
      ),
      protocol_comment = c(
        "Adult copepods collected between January and March 1997, acclimated to 15°C for 12 days",
        "Radioactivity measured with NaI(Tl) gamma detectors at specific energy levels for each isotope",
        ""
      )
    ),
    samples = tibble(
      sampling_date = c("2025-09-29", "2023-02-12")
    )
  )

  # Convert data frame column names to uppercase if requested
  if (uppercase_columns) {
    tibble_elements <- c(
      "sites",
      "parameters",
      "compartments",
      "biota",
      "methods"
    )
    for (element in tibble_elements) {
      if (!is.null(dummy_data[[element]]) && is_tibble(dummy_data[[element]])) {
        names(dummy_data[[element]]) <- toupper(names(dummy_data[[element]]))
      }
    }
  }

  return(dummy_data)
}

#' Populate session data directly with dummy data
#'
#' @description Stores dummy data directly into session reactiveValues.
#' This bypasses the LLM extraction process and populates all module
#' data objects immediately.
#'
#' @param session Shiny session object
#' @param navigate_to Optional tab to navigate to after loading data
#' @param parent_session Parent session for navigation (if different from session)
#'
#' @importFrom shiny showNotification updateNavbarPage
#' @importFrom golem print_dev
#' @export
populate_session_with_dummy_data <- function(
  session,
  navigate_to = NULL,
  parent_session = NULL
) {
  # Create dummy data with uppercase columns for app data structures
  dummy_data <- create_dummy_data(uppercase_columns = TRUE)

  # Store directly in session userData for immediate use
  # Campaign data
  if (!is.null(dummy_data$campaign)) {
    session$userData$reactiveValues$campaignData <- dummy_data$campaign
    print_dev("Populated campaign data with dummy data")
  }

  # References data
  if (!is.null(dummy_data$references)) {
    session$userData$reactiveValues$referenceData <- dummy_data$references
    print_dev("Populated references data with dummy data")
  }

  # Sites data
  if (!is.null(dummy_data$sites)) {
    session$userData$reactiveValues$sitesData <- dummy_data$sites
    print_dev("Populated sites data with dummy data")
  }

  # Parameters data
  if (!is.null(dummy_data$parameters)) {
    session$userData$reactiveValues$parametersData <- dummy_data$parameters
    print_dev("Populated parameters data with dummy data")
  }

  # Compartments data
  if (!is.null(dummy_data$compartments)) {
    session$userData$reactiveValues$compartmentsData <- dummy_data$compartments
    print_dev("Populated compartments data with dummy data")
  }

  # Biota data
  if (!is.null(dummy_data$biota)) {
    session$userData$reactiveValues$biotaData <- dummy_data$biota
    print_dev("Populated biota data with dummy data")
  }

  # Methods data
  if (!is.null(dummy_data$methods)) {
    session$userData$reactiveValues$methodsData <- dummy_data$methods
    print_dev("Populated methods data with dummy data")
  }

  # Set status flags
  session$userData$reactiveValues$dummyDataLoaded <- TRUE

  showNotification(
    "Dummy data loaded successfully! All modules now contain test data.",
    type = "default"
  )

  # Navigate if requested
  if (!is.null(navigate_to) && !is.null(parent_session)) {
    updateNavbarPage(
      session = parent_session,
      inputId = "main-page",
      selected = navigate_to
    )
  }

  print_dev("Dummy data population complete")
}

# Setup: Create dummy data matching actual structure ----
create_dummy_module_data <- function() {
  list(
    references = tibble(
      AUTHOR = "Smith, J.; Jones, A.",
      YEAR = 2023,
      TITLE = "Heavy metal contamination in coastal sediments",
      PERIODICAL_JOURNAL = "Environmental Science & Technology",
      PUBLISHER = NA_character_,
      DOI = "10.1234/est.2023.001"
    ),

    parameters = tibble(
      PARAMETER_NAME = c("Copper", "Lead", "Zinc", "Cadmium")
    ),

    compartments = tibble(
      ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Aquatic", "Terrestrial"),
      ENVIRON_COMPARTMENT_SUB = c(
        "Aquatic Sediment",
        "Porewater",
        "Wastewater",
        "Biological Residue"
      )
    ),

    sites = tibble(
      SITE_ID = c("S1", "S2", "S3"),
      COUNTRY = c("Norway", "Norway", "Sweden"),
      AREA = c("Oslo", "Bergen", "Stockholm"),
      SITE_GEOGRAPHIC_FEATURE = c("Fjord", "Fjord", "Lake"),
      SITE_GEOGRAPHIC_FEATURE_SUB = c("Inner", "Outer", "Shallow"),
      LATITUDE = c(59.9139, 60.3913, 59.3293),
      LONGITUDE = c(10.7522, 5.3221, 18.0686)
    ),

    samples = tibble(
      SAMPLING_DATE = as.Date(c(
        "2023-01-15",
        "2023-03-20",
        "2023-06-10",
        "2023-09-05"
      ))
    ),

    measurements = tibble(
      PARAMETER_NAME = c("Copper", "Lead", "Copper", "Zinc"),
      MEASURED_UNIT = c("mg/kg", "mg/kg", "µg/L", "mg/kg"),
      MEASURED_VALUE = c(15.3, 8.45, 2.1, 12.678),
      LOQ_VALUE = c(0.5, 1.0, 0.1, 0.8),
      LOQ_UNIT = c("mg/kg", "mg/kg", "µg/L", "mg/kg"),
      LOD_VALUE = c(0.2, 0.4, 0.05, 0.3),
      LOD_UNIT = c("mg/kg", "mg/kg", "µg/L", "mg/kg"),
      UNCERTAINTY_TYPE = c("SD", "SD", "CI", "Range"),
      MEASUREMENT_COMMENT = c(
        "High quality measurement",
        "Baseline sample",
        NA_character_,
        "Good precision"
      )
    ),

    methods = tibble(
      PROTOCOL_CATEGORY = c(
        "Sampling Protocol",
        "Sampling Protocol",
        "Analytical Protocol",
        "Analytical Protocol",
        "Fractionation Protocol",
        "Extraction Protocol"
      ),
      PROTOCOL_NAME = c(
        "Surface sediment grab",
        "Water column sampling",
        "ICP-MS analysis",
        "AAS analysis",
        "Sequential extraction",
        "Washing"
      ),
      PROTOCOL_COMMENT = c(
        "I picked up sand with my bare hands.",
        "Mouth pipetting.",
        "Threw it at the wall to see what happens.",
        "Set it on fire to see what happens.",
        "Multi-step acid digestion process",
        "Ran under the tap for ten minutes. Used non-organic dishsoap."
      )
    )
  )
}

dummy_session_data <- function() {
  dummy <- create_dummy_module_data()
  list(
    referenceData = dummy$references,
    parametersData = dummy$parameters,
    compartmentsData = dummy$compartments,
    sitesData = dummy$sites,
    samplesData = dummy$samples,
    measurementsData = dummy$measurements,
    methodsData = dummy$methods
  )
}
