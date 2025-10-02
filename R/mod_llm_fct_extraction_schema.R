#' Create campaign schema
#' @noRd
create_campaign_schema <- function() {
  type_object(
    .description = "Basic study/campaign information",
    campaign_name = type_string(
      description = "Short identifier for the study/campaign (max 100 chars)",
      required = FALSE
    ),
    campaign_start_date = type_string(
      description = "Study start date in YYYY-MM-DD format",
      required = FALSE
    ),
    campaign_end_date = type_string(
      description = "Study end date in YYYY-MM-DD format",
      required = FALSE
    ),
    organisation = type_string(
      description = "Organization that conducted the study (max 50 chars)",
      required = FALSE
    ),
    campaign_comment = type_string(
      description = "Additional study details or notes (max 1000 chars)",
      required = FALSE
    )
  )
}

#' Create references schema
#' @noRd
create_references_schema <- function() {
  type_object(
    .description = "Bibliographic information about this document",
    author = type_string(
      description = "Authors in format: Last1, First1; Last2, First2 (max 1000 chars)",
      required = FALSE
    ),
    title = type_string(
      description = "Document title (max 1000 chars)",
      required = FALSE
    ),
    year = type_integer(
      description = "Publication year (1800-2026)",
      required = FALSE
    ),
    periodical_journal = type_string(
      description = "Journal name for articles",
      required = FALSE
    ),
    volume = type_integer(
      description = "Journal volume number",
      required = FALSE
    ),
    issue = type_integer(
      description = "Journal issue number",
      required = FALSE
    ),
    publisher = type_string(
      description = "Publisher name",
      required = FALSE
    ),
    doi = type_string(
      description = "Digital Object Identifier",
      required = FALSE
    )
  )
}

#' Create sites schema
#' @noRd
create_sites_schema <- function() {
  type_array(
    type_object(
      .description = "Information about a sampling site",
      site_code = type_string(
        description = "Short site identifier/code",
        required = FALSE
      ),
      site_name = type_string(
        description = "Descriptive site name. If many sampling sites are reported without specific coordinates, note in the name.",
        required = FALSE
      ),
      latitude = type_number(
        description = "Latitude in decimal degrees (-90 to 90) - ONLY if explicitly stated in document",
        required = FALSE
      ),
      longitude = type_number(
        description = "Longitude in decimal degrees (-180 to 180) - ONLY if explicitly stated in document",
        required = FALSE
      ),
      country = type_string(
        description = "Country where site is located - ONLY if explicitly stated",
        required = FALSE
      ),
      site_geographic_feature = type_string(
        description = "Geographic feature type from: River stream canal, Lake pond pool reservoir, Ocean sea territorial waters, Coastal fjord, Estuary, Drainage sewer artificial water, Swamp wetland, Groundwater aquifer, WWTP, Artificial Land/Urban Areas, Landfills, Cropland, Woodland forest, Shrubland, Grassland, Bare land and lichen/moss, Other",
        required = FALSE
      ),
      site_comment = type_string(
        description = "Any additional details about the site not captured in the previous variables.",
        required = FALSE
      )
    )
  )
}

#' Create parameters schema
#' @noRd
create_parameters_schema <- function() {
  type_array(
    type_object(
      .description = "A measured parameter/stressor",
      parameter_name = type_string(
        description = "Name of the parameter/chemical/stressor measured",
        required = FALSE
      ),
      parameter_type = type_string(
        description = "Type: Stressor, Quality parameter, Normalization, or Background",
        required = FALSE
      ),
      cas_rn = type_string(
        description = "CAS Registry Number if chemical",
        required = FALSE
      )
    )
  )
}

#' Create compartments schema
#' @noRd
create_compartments_schema <- function() {
  type_array(
    type_object(
      .description = "An environmental compartment sampled",
      environ_compartment = type_string(
        description = "Main compartment: Aquatic, Atmospheric, Terrestrial, or Biota",
        required = FALSE
      ),
      environ_compartment_sub = type_string(
        description = "Sub-compartment: Freshwater, Marine/Salt Water, Brackish/Transitional Water, Groundwater, Wastewater, Aquatic Sediment, Indoor Air, Outdoor Air, Soil A Horizon (Topsoil), Soil O Horizon (Organic), Biota Terrestrial, Biota Aquatic",
        required = FALSE
      ),
      measured_category = type_string(
        description = "Measurement category: External, Internal, or Surface",
        required = FALSE
      )
    )
  )
}

#' Create biota schema
#' @noRd
create_biota_schema <- function() {
  type_array(
    type_object(
      .description = "Biological sampling information",
      sample_id = type_string(
        description = "Sample identifier",
        required = FALSE
      ),
      species_group = type_string(
        description = "Taxonomic group: Worms, Insects/Spiders, Molluscs, Fungi, Crustaceans, Mammals, Amphibians, Moss/Hornworts, Birds, Fish, Plants, Algae, Invertebrates, Reptiles, Bacteria, Ecosystem, Other",
        required = FALSE
      ),
      sample_species = type_string(
        description = "Species name (scientific if reported otherwise common)",
        required = FALSE
      ),
      sample_tissue = type_string(
        description = "Tissue type: Whole organism, Muscle, Liver, Kidney, Brain, Heart, Lung, Gill, Shell, Carapace, Blood, Egg, Larva, Leaf, Root, Stem, Fruit, Seed, Other",
        required = FALSE
      ),
      sample_species_lifestage = type_string(
        description = "Life stage: Adult, Juvenile, Larva, Embryo, Egg, Seedling, Mature, Young, Mixed, Not applicable, Other",
        required = FALSE
      ),
      sample_species_gender = type_string(
        description = "Gender: Male, Female, Mixed, Hermaphrodite, Not applicable, Not determined, Other",
        required = FALSE
      )
    )
  )
}

#' Create methods schema
#' @noRd
create_methods_schema <- function() {
  type_array(
    type_object(
      .description = "Analytical and sampling methods used",
      protocol_category = type_string(
        description = "Protocol type: Sampling Protocol, Fractionation Protocol, Extraction Protocol, Analytical Protocol",
        required = FALSE
      ),
      protocol_name = type_string(
        description = "Specific method name - must match exactly one of these options: 
          Not relevant, Not reported, 
        Point, Composite, Trawl, Grab, Core, Seine net, Electrofishing, Plankton net, 
        Pooled, Filtration, Size Fractionation, PLE, MAE, USE, SPE, QuEChERS, LLE, SLE, Classical Extraction, Soxhlet, 
        SBSE, HS-SPME, SPME, GC-MS, LC-MS, GC-ECD, GC-NPD, GC-FID, LC-UV, LC-FLD, ICP-MS, ICP-AES, AAS, FAAS, GFAAS, HGAAS, CVAAS, XRF, XRD, FTIR, UV-VIS, Enzyme assay, ELISA, RIA, Bioassay, Chemical analysis, Physical analysis, Microscopy, Histology. If technique does not match these options, report as Other",
        required = FALSE
      ),
      protocol_comment = type_string(
        description = "Additional details about the method, including a more specific description of the method (applicance name, reagents, etc)., ideally transcribed from source without modification.",
        required = FALSE
      )
    )
  )
}

#' Create samples schema
#' @noRd
create_samples_schema <- function() {
  type_array(
    type_object(
      .description = "Information on the overall sampling strategy of the paper, a (potentially assymetrical) combination of sites, dates, compartments/biota, and measured parameters. Some of these may be replicated multiple times.",
      sampling_date = type_string(
        description = "Dates (YYYY-MM-DD) when samples were taken",
        required = TRUE
      )
    ),
    description = "Information on the overall sampling strategy of the paper"
  )
}

#' Create extraction schema with correct ellmer syntax
#' @noRd
create_extraction_schema <- function() {
  type_object(
    .description = "Extract environmental exposure study data from this document",
    campaign = create_campaign_schema(),
    references = create_references_schema(),
    sites = create_sites_schema(),
    parameters = create_parameters_schema(),
    compartments = create_compartments_schema(),
    biota = create_biota_schema(),
    methods = create_methods_schema(),
    samples = create_samples_schema()
  )
}

get_schema_display <- function() {
  tryCatch(
    {
      # Create the actual schema and capture its structure
      schema <- create_extraction_schema()

      # Convert to a readable format showing the actual ellmer object structure
      schema_str <- capture.output({
        print(schema, width = 80)
      })

      # Join the output lines
      paste(schema_str, collapse = "\n")
    },
    error = function(e) {
      paste("Error displaying schema:", e$message)
    }
  )
}
