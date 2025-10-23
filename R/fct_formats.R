# Initialise tibbles for each "table" of data used across the modules.
# Doing this in one place makes it easier (but not easy) to keep things consistent.
# The plan is that eventually all functions that are dependent on table format will
# be based in some way on these functions. That may prove to be impractical. But it's a start.
# eData DRF Version - Not Tracked
# Last Updated 2025.10.23

#' Initialize Campaign Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for campaign data.
#' Campaigns represent sampling projects or studies with metadata about timing,
#' organization, data quality evaluation, and confidentiality.
#'
#' @return A tibble with 0 rows and standardised campaign columns
#' @importFrom tibble tibble
#' @export
initialise_campaign_tibble <- function() {
  # TODO: Not yet implemented.
  tibble(
    CAMPAIGN_NAME_SHORT = character(),
    CAMPAIGN_NAME = character(),
    CAMPAIGN_START_DATE = as.Date(character()),
    CAMPAIGN_END_DATE = as.Date(character()),
    RELIABILITY_SCORE = character(),
    RELIABILITY_EVAL_SYS = character(),
    CONFIDENTIALITY_EXPIRY_DATE = as.Date(character()),
    ORGANISATION = character(),
    ENTERED_BY = character(),
    ENTERED_DATE = as.Date(character()),
    CAMPAIGN_COMMENT = character()
  )
}

#' Initialize Biota Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for biota data.
#' Biota data extends sample information with species-specific details including
#' taxonomic classification, tissue type, life stage, and gender information.
#'
#' @return A tibble with 0 rows and standardised biota columns
#' @importFrom tibble tibble
#' @export
initialise_biota_tibble <- function() {
  # TODO: Implemented.
  tibble(
    SAMPLE_ID = character(),
    SITE_CODE = character(),
    PARAMETER_NAME = character(),
    ENVIRON_COMPARTMENT = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    MEASURED_CATEGORY = character(),
    SAMPLING_DATE = character(),
    REP = integer(),
    SPECIES_GROUP = character(),
    SAMPLE_SPECIES = character(),
    SAMPLE_TISSUE = character(),
    SAMPLE_SPECIES_LIFESTAGE = character(),
    SAMPLE_SPECIES_GENDER = character()
  )
}

#' Initialize Compartments Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for environmental
#' compartment data. Compartments define the environmental matrix and measurement
#' context for sampling activities.
#'
#' @return A tibble with 0 rows and standardised compartment columns
#' @importFrom tibble tibble
#' @export
initialise_compartments_tibble <- function() {
  # TODO: Implemented.
  tibble(
    ENVIRON_COMPARTMENT = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    MEASURED_CATEGORY = character()
  )
}

#' Initialize Methods Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for analytical
#' methods data. Methods describe the protocols used for sampling, extraction,
#' fractionation, and analysis procedures.
#'
#' @return A tibble with 0 rows and standardised methods columns
#' @importFrom tibble tibble
#' @export
initialise_methods_tibble <- function() {
  # TODO: Implemented.
  tibble(
    PROTOCOL_ID = character(),
    CAMPAIGN_NAME = character(),
    PROTOCOL_CATEGORY = character(),
    PROTOCOL_NAME = character(),
    PROTOCOL_COMMENT = character()
  )
}

#' Initialize Parameters Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for parameter data.
#' Parameters define chemical substances, physical properties, or biological markers
#' being measured, including classification and chemical identifiers.
#'
#' @return A tibble with 0 rows and standardised parameter columns
#' @importFrom tibble tibble
#' @export
initialise_parameters_tibble <- function() {
  # TODO: Implemented.
  tibble(
    PARAMETER_TYPE = character(),
    PARAMETER_TYPE_SUB = character(),
    MEASURED_TYPE = character(),
    PARAMETER_NAME = character(),
    PARAMETER_NAME_SUB = character(),
    INCHIKEY_SD = character(),
    PUBCHEM_CID = character(),
    CAS_RN = character(),
    ENTERED_BY = character()
  )
}

#' Initialize References Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for reference data.
#' References provide bibliographic information for data sources including journals,
#' reports, datasets, and other published materials.
#'
#' @return A tibble with 0 rows and standardised reference columns
#' @importFrom tibble tibble
#' @export
initialise_references_tibble <- function() {
  # TODO: Not yet implemented.
  tibble(
    REFERENCE_ID = character(),
    REFERENCE_TYPE = character(),
    DATA_SOURCE = character(),
    AUTHOR = character(),
    TITLE = character(),
    YEAR = integer(),
    ACCESS_DATE = as.Date(character()),
    PERIODICAL_JOURNAL = character(),
    VOLUME = integer(),
    ISSUE = integer(),
    PUBLISHER = character(),
    INSTITUTION = character(),
    DOI = character(),
    URL = character(),
    ISBN_ISSN = character(),
    EDITION = character(),
    DOCUMENT_NUMBER = character(),
    REF_COMMENT = character()
  )
}

#' Initialize Samples Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for sample data.
#' Samples represent individual collections from sites with temporal, spatial,
#' and methodological information linking sites, parameters, and compartments.
#'
#' @return A tibble with 0 rows and standardised sample columns
#' @importFrom tibble tibble
#' @export
initialise_samples_tibble <- function() {
  # TODO: Implemented.

  tibble(
    SITE_CODE = character(),
    SITE_NAME = character(),
    PARAMETER_NAME = character(),
    PARAMETER_TYPE = character(),
    ENVIRON_COMPARTMENT = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    MEASURED_CATEGORY = character(),
    SAMPLING_DATE = character(),
    REP = numeric(),
    REPLICATE_ID = character(),
    SAMPLE_ID = character()
  )
}

#' Initialize Sites Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for site data.
#' Sites represent sampling locations with geographic coordinates, administrative
#' boundaries, and descriptive metadata about the sampling location.
#'
#' @return A tibble with 0 rows and standardised site columns
#' @importFrom tibble tibble
#' @export
initialise_sites_tibble <- function() {
  tibble(
    SITE_CODE = character(),
    SITE_NAME = character(),
    SITE_GEOGRAPHIC_FEATURE = character(),
    SITE_GEOGRAPHIC_FEATURE_SUB = character(),
    COUNTRY = character(),
    AREA = character(),
    LATITUDE = numeric(),
    LONGITUDE = numeric(),
    SITE_COORDINATE_SYSTEM = character(),
    ALTITUDE_VALUE = numeric(),
    ALTITUDE_UNIT = character(),
    ENTERED_BY = character(),
    ENTERED_DATE = character(),
    SITE_COMMENT = character()
  )
}

#' Geographic Features Controlled Vocabulary
#'
#' Returns controlled vocabulary options for geographic features.
#'
#' @return A character vector of geographic feature options
#' @export
#' @import ISOcodes
geographic_features_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "River, stream, canal",
    "Lake, pond, pool, reservoir",
    "Ocean, sea, territorial waters",
    "Coastal, fjord",
    "Estuary",
    "Drainage, sewer, artificial water",
    "Swamp, wetland",
    "Groundwater, aquifer",
    "WWTP",
    "Artificial Land/Urban Areas",
    "Landfills",
    "Cropland",
    "Woodland, forest",
    "Shrubland",
    "Grassland",
    "Bare land and lichen/moss",
    "Glacier",
    "Other"
  )
}

#' Geographic Features Sub Controlled Vocabulary
#'
#' Returns controlled vocabulary options for geographic feature subcategories.
#'
#' @return A character vector of geographic feature subcategory options
#' @export
geographic_features_sub_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "Water surface",
    "Water column, pelagic zone",
    "Water benthos",
    "Other"
  )
}

#' Coordinate Systems Controlled Vocabulary
#'
#' Returns controlled vocabulary options for coordinate systems.
#'
#' @return A character vector of coordinate system options
#' @export
coordinate_systems_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "WGS 84",
    "UTM 32",
    "UTM 33",
    "UTM 34",
    "UTM 35",
    "ETRS89",
    "Other"
  )
}

#' Countries Controlled Vocabulary
#'
#' Returns controlled vocabulary options for countries.
#'
#' @return A character vector of country options
#' @export
#' @import ISOcodes
countries_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "Other/Not a Country",
    ISOcodes::ISO_3166_1$Name
  )
}

#' Areas Controlled Vocabulary
#'
#' Returns controlled vocabulary options for areas.
#'
#' @return A character vector of area options
#' @export
#' @importFrom dplyr pull
areas_vocabulary <- function() {
  IHO_oceans <- readRDS("inst/data/clean/IHO_oceans.rds") |> pull(NAME)

  c(
    "Not relevant",
    "Not reported",
    "Other",
    IHO_oceans
  )
}

#' Altitude Units Controlled Vocabulary
#'
#' Returns controlled vocabulary options for altitude units.
#'
#' @return A character vector of altitude unit options
#' @export
altitude_units_vocabulary <- function() {
  c("km", "m", "cm", "mm")
}
