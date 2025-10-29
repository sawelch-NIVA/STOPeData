# Initialise tibbles for each "table" of data used across the modules.
# Doing this in one place makes it easier (but not easy) to keep things consistent.
# The plan is that eventually all functions that are dependent on table format will
# be based in some way on these functions. That may prove to be impractical. But it's a start.
# eData DRF Version - Not Tracked
# Last Updated 2025.10.23
# TODO: Implement caching for functions that call big datasets, if there turns
# out to be performance issues
# TODO: Update mod_llm_fct_populate to integrate with these functions, if poss.

# -----------------------
# ---- TABLE FORMATS ----
# -----------------------

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
  # TODO: Implemented.
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
    SAMPLE_SPECIES_GENDER = character(),
    BIOTA_COMMENT = character()
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
  # TODO: Implemented partially - not yet used in create_method_entry()
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
    ENTERED_BY = character(),
    PARAMETER_COMMENT = character()
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
  # TODO: Implemented

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

#' Initialize Measurements Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for measurements data.
#'
#' @return A tibble with 0 rows and standardised measurement columns
#' @importFrom tibble tibble
#' @export
## Initialize measurement combinations data frame ----
# Fix: Not Implemented.

initialise_measurements_tibble <- function() {
  tibble(
    SITE_CODE = character(),
    PARAMETER_NAME = character(),
    SAMPLING_DATE = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    REP = integer(),
    MEASURED_FLAG = character(),
    MEASURED_VALUE = numeric(),
    MEASURED_SD = numeric(),
    MEASURED_UNIT = character(),
    LOQ_VALUE = numeric(),
    LOQ_UNIT = character(),
    LOD_VALUE = numeric(),
    LOD_UNIT = character(),
    SAMPLING_PROTOCOL = character(),
    EXTRACTION_PROTOCOL = character(),
    FRACTIONATION_PROTOCOL = character(),
    ANALYTICAL_PROTOCOL = character(),
    REFERENCE_ID = character(),
    SAMPLE_ID = character(),
    ENVIRON_COMPARTMENT = character()
  )
}

# ------------------------
# ------ VOCABULARY ------
# ------------------------

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

#' Dummy Parameters Data
#'
#' Returns dummy parameter data combining quality parameters and chemical parameters.
#'
#' @return A data frame of dummy parameter data
#' @export
#' @import dplyr
#' @importFrom arrow read_parquet
# TODO: We call this "dummy" data for the simple reason that I've never looked for or made my own comprehensive list of quality parameters.
dummy_parameters_vocabulary <- function() {
  # Read dummy_parameters ----
  dummy_quality_params <- read_parquet(
    file = "inst/data/clean/dummy_quality_parameters.parquet"
  ) |>
    mutate(ENTERED_BY = "saw@niva.no")

  # Read and prepare chemical_parameters ----
  chemical_parameters <- read_parquet(
    file = "inst/data/clean/ClassyFire_Taxonomy_2025_02.parquet"
  ) |>
    mutate(
      MEASURED_TYPE = "Concentration",
      ENTERED_BY = "saw@niva.no"
    ) |>
    arrange(PARAMETER_NAME) |>
    mutate(
      PARAMETER_TYPE_SUB = case_when(
        PARAMETER_NAME == "Carbon" ~ "Carbon",
        TRUE ~ PARAMETER_TYPE_SUB
      ),
      PARAMETER_NAME_SUB = ""
    )

  # Merge datasets ----
  bind_rows(dummy_quality_params, chemical_parameters)
}

#' Parameter Types Controlled Vocabulary
#'
#' Returns controlled vocabulary options for parameter types.
#'
#' @return A character vector of parameter type options
#' @export
parameter_types_vocabulary <- function() {
  c(
    "Not relevant",
    "Stressor",
    "Quality parameter",
    "Normalization",
    "Background",
    "Ecological Indicator",
    "Other"
  )
}

#' Parameter Types Sub Controlled Vocabulary
#'
#' Returns controlled vocabulary options for parameter type subcategories.
#'
#' @return A character vector of parameter type subcategory options
#' @export
#' @import dplyr
#' @importFrom arrow read_parquet
parameter_types_sub_vocabulary <- function() {
  dummy_parameters <- dummy_parameters_vocabulary()

  dummy_parameters |>
    select(PARAMETER_TYPE_SUB) |>
    distinct() |>
    arrange(PARAMETER_TYPE_SUB) |>
    pull(PARAMETER_TYPE_SUB) |>
    append(c("Mixture", "Not reported"))
}

#' Measured Types Controlled Vocabulary
#'
#' Returns controlled vocabulary options for measured types.
#'
#' @return A character vector of measured type options
#' @export
measured_types_vocabulary <- function() {
  c(
    "Concentration",
    "Dose rate",
    "Dose",
    "Physical parameter",
    "Amount",
    "Volume",
    "Fraction of total",
    "Percent",
    "Irradiance",
    "Response",
    "Ecological Indicator",
    "Not relevant",
    "Other"
  )
}

#' Sub-compartment Options Mapping
#'
#' Returns controlled vocabulary mapping for environmental sub-compartments organized by main compartment.
#'
#' @return A named list of character vectors with sub-compartment options for each main compartment
#' @export
sub_compartment_options_vocabulary <- function() {
  list(
    "Aquatic" = c(
      "Freshwater" = "Freshwater",
      "Marine/Salt Water" = "Marine/Salt Water",
      "Brackish/Transitional Water" = "Brackish/Transitional Water",
      "Groundwater" = "Groundwater",
      "Wastewater" = "Wastewater",
      "Liquid Growth Medium" = "Liquid Growth Medium",
      "Rainwater" = "Rainwater",
      "Stormwater" = "Stormwater",
      "Leachate" = "Leachate"
    ),
    "Atmospheric" = c(
      "Indoor Air" = "Indoor Air",
      "Outdoor Air" = "Outdoor Air"
    ),
    "Terrestrial" = c(
      "Terrestrial Biological Residue" = "Terrestrial Biological Residue",
      "Soil H Horizon (Peat)" = "Soil H Horizon (Peat)",
      "Soil O Horizon (Organic)" = "Soil O Horizon (Organic)",
      "Soil A Horizon (Topsoil)" = "Soil A Horizon (Topsoil)",
      "Soil E Horizon (Mineral)" = "Soil E Horizon (Mineral)",
      "Soil S Horizon (Mineral)" = "Soil S Horizon (Mineral)",
      "Soil C Horizon (Parent Material)" = "Soil C Horizon (Parent Material)",
      "Soil R Horizon (Bedrock)" = "Soil R Horizon (Bedrock)"
    ),
    "Biota" = c(
      "Biota, Terrestrial" = "Biota, Terrestrial",
      "Biota, Aquatic" = "Biota, Aquatic",
      "Biota, Atmospheric" = "Biota, Atmospheric",
      "Biota, Other" = "Biota, Other"
    )
  )
}

#' Environmental Compartments Controlled Vocabulary
#'
#' Returns controlled vocabulary options for environmental compartments.
#'
#' @return A character vector of environmental compartment options
#' @export
environ_compartments_vocabulary <- function() {
  c(
    "Aquatic",
    "Atmospheric",
    "Terrestrial",
    "Biota",
    "Not relevant",
    "Not reported",
    "Other"
  )
}

#' Environmental Compartment Subs Controlled Vocabulary
#'
#' Returns controlled vocabulary options for environmental compartment subcategories.
#'
#' @return A character vector of environmental compartment subcategory options
#' @export
environ_compartment_subs_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "Freshwater",
    "Marine/Salt Water",
    "Brackish/Transitional Water",
    "Groundwater",
    "Wastewater",
    "Liquid Growth Medium",
    "Rainwater",
    "Stormwater",
    "Leachate",
    "Aquatic Sediment",
    "Indoor Air",
    "Outdoor Air",
    "Terrestrial Biological Residue",
    "Soil H Horizon (Peat)",
    "Soil O Horizon (Organic)",
    "Soil A Horizon (Topsoil)",
    "Soil E Horizon (Mineral)",
    "Soil S Horizon (Mineral)",
    "Soil C Horizon (Parent Material)",
    "Soil R Horizon (Bedrock)",
    "Biota, Terrestrial",
    "Biota, Aquatic",
    "Biota, Atmospheric",
    "Biota, Other",
    "Other"
  )
}

#' Measured Categories Controlled Vocabulary
#'
#' Returns controlled vocabulary options for measured categories.
#'
#' @return A character vector of measured category options
#' @export
measured_categories_vocabulary <- function() {
  c(
    "External" = "External Media",
    "Internal" = "Internal to Organism",
    "Surface" = "Surface of Organism"
  )
}


#' Initialize Tissue Types Controlled Vocabulary
#'
#' Returns controlled vocabulary options for sample tissue types.
#'
#' @return A character vector of tissue type options
#' @export
tissue_types_vocabulary <- function() {
  c(
    "Not reported",
    "Not relevant",
    "Whole body",
    "Muscle",
    "Liver",
    "Kidney",
    "Fat/Adipose",
    "Skin",
    "Bone",
    "Brain",
    "Heart",
    "Lung",
    "Gill",
    "Shell",
    "Carapace",
    "Blood",
    "Egg",
    "Larva",
    "Leaf",
    "Root",
    "Stem",
    "Fruit",
    "Seed",
    "Other"
  )
}

#' Initialize Life Stages Controlled Vocabulary
#'
#' Returns controlled vocabulary options for sample species life stages.
#'
#' @return A character vector of life stage options
#' @export
lifestage_vocabulary <- function() {
  c(
    "Not reported",
    "Not relevant",
    "Adult",
    "Juvenile",
    "Larva",
    "Embryo",
    "Egg",
    "Seedling",
    "Mature",
    "Young",
    "Mixed",
    "Other"
  )
}

#' Initialize Gender Controlled Vocabulary
#'
#' Returns controlled vocabulary options for sample species gender.
#'
#' @return A character vector of gender options
#' @export
gender_vocabulary <- function() {
  c(
    "Not reported",
    "Not relevant",
    "Male",
    "Female",
    "Mixed",
    "Hermaphrodite",
    "Other"
  )
}

#' Species Groups Controlled Vocabulary
#'
#' Returns controlled vocabulary options for species groups. Taken from EPA ECOTOX db.
#'
#' @return A character vector of species group options
#' @export
species_groups_vocabulary <- function() {
  c(
    "All",
    "Algae",
    "Amphibians",
    "Bacteria",
    "Birds",
    "Crustaceans",
    "Ecosystem",
    "Fish",
    "Fungi",
    "Insects/Spiders",
    "Invertebrates",
    "Mammals",
    "Molluscs",
    "Moss/Hornworts",
    "Plants",
    "Reptiles",
    "Worms",
    "Other"
  )
}

# ------------------------
# --- CHARACTER LIMITS ---
# ------------------------

### Character limit validations for optional fields ----
#' reference_character_limits
#'
#' @returns a list of character limits for fields in mod_references
#'
#' @export
reference_character_limits <- function() {
  list(
    # ACCESSION_NUMBER = 200,  # COMMENTED OUT
    # DB_NAME = 200,           # COMMENTED OUT
    # DB_PROVIDER = 200,       # COMMENTED OUT
    DOCUMENT_NUMBER = 200,
    DOI = 200,
    EDITION = 200,
    INSTITUTION = 200,
    ISBN_ISSN = 200,
    # NUMBER_OF_PAGES = 50,    # COMMENTED OUT
    # NUMBER_OF_VOLUMES = 100, # COMMENTED OUT
    # PAGES = 200,             # COMMENTED OUT
    PERIODICAL_JOURNAL = 200,
    # PMCID = 200,             # COMMENTED OUT
    # PUBLISHED_PLACE = 200,   # COMMENTED OUT
    PUBLISHER = 200,
    REF_COMMENT = 1000,
    # SERIES_EDITOR = 200,     # COMMENTED OUT
    # SERIES_TITLE = 200,      # COMMENTED OUT
    URL = 200
  )
}

#' Protocol Options Data
#'
#' Returns protocol options data from parquet file.
#'
#' @return A data frame of protocol options
#' @export
#' @importFrom arrow read_parquet
protocol_options_vocabulary <- function() {
  read_parquet("inst/data/clean/inst-claude-2025-08-05-methods.parquet")
}

#' Protocol Categories Controlled Vocabulary
#'
#' Returns controlled vocabulary options for protocol categories.
#'
#' @return A character vector of protocol category options
#' @export
protocol_categories_vocabulary <- function() {
  c(
    "Sampling Protocol",
    "Fractionation Protocol",
    "Extraction Protocol",
    "Analytical Protocol"
  )
}

#' Read in ecotoxicological units and conversion factors from csv
#'
#' @param select_column name of column to pull ("MEASURED_UNIT", "BASE_SI_UNIT", "CONVERSION_FACTOR", "UNIT_COMMENTS")
#'
#' @returns a dataframe or a character vector
#'
#' @export
#' @importFrom readr read_csv
parameter_unit_vocabulary <- function(select_column = NULL) {
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


#' Measurement Flags Controlled Vocabulary
#'
#' Returns measurement flag options.
#'
#' @return A character vector of measurement flag options
#' @export
measured_flags_vocabulary <- function() {
  c("", "< LOQ", "< LOD")
}
