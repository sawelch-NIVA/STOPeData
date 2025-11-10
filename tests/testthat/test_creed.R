# test-creed-functions.R ----

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
      COUNTRY = c("Norway", "Norway", "Sweden", "Norway"),
      AREA = c("Oslo Fjord", "Oslo Fjord", "Skagerrak", "Bergen"),
      SITE_GEOGRAPHIC_FEATURE = c("Fjord", "Fjord", "Coastal", "Harbor")
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
        "Fractionation Protocol"
      ),
      PROTOCOL_NAME = c(
        "Surface sediment grab",
        "Water column sampling",
        "ICP-MS analysis",
        "AAS analysis",
        "Sequential extraction"
      ),
      PROTOCOL_COMMENT = c(
        "I picked up sand with my bare hands.",
        "Mouth pipetting.",
        "Threw it at the wall to see what happens.",
        "Set it on fire to see what happens.",
        "Multi-step acid digestion process"
      )
    )
  )
}

create_session_data <- function() {
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

# Tests: Helper functions ----

test_that("create_bibliography_reference works with complete data", {
  ref_data <- tibble(
    AUTHOR = "Smith, J.",
    YEAR = 2023,
    TITLE = "Test Article",
    PERIODICAL_JOURNAL = "Test Journal",
    PUBLISHER = NA_character_,
    DOI = "10.1234/test"
  )

  result <- create_bibliography_reference(ref_data)

  expect_type(result, "character")
  expect_true(grepl("Smith, J.", result))
  expect_true(grepl("2023", result))
  expect_true(grepl("Test Article", result))
  expect_true(grepl("Test Journal", result))
  expect_true(grepl("10.1234/test", result))
})

test_that("create_bibliography_reference handles missing fields", {
  ref_data <- tibble(
    AUTHOR = "Smith, J.",
    YEAR = NA_integer_,
    TITLE = NA_character_,
    PERIODICAL_JOURNAL = NA_character_,
    PUBLISHER = NA_character_,
    DOI = NA_character_
  )

  result <- create_bibliography_reference(ref_data)

  expect_type(result, "character")
  expect_true(grepl("Smith, J.", result))
})

test_that("create_bibliography_reference handles NULL or empty data", {
  expect_equal(create_bibliography_reference(NULL), "Relevant data not found")
  expect_equal(
    create_bibliography_reference(tibble()),
    "Relevant data not found"
  )
})

test_that("summarize_multiple works with basic input", {
  values <- c("A", "B", "C", "A", "D")
  result <- summarize_multiple(values)

  expect_type(result, "character")
  expect_true(grepl("A", result))
  expect_true(grepl("B", result))
  expect_true(grepl("C", result))
  expect_true(grepl("D", result))
  expect_false(grepl("A.*A", result)) # Should deduplicate
})

test_that("summarize_multiple adds prefix correctly", {
  values <- c("A", "B", "C")
  result <- summarize_multiple(values, prefix = "Items")

  expect_true(grepl("Items \\(3\\):", result))
})

test_that("summarize_multiple truncates long lists", {
  values <- letters[1:15]
  result <- summarize_multiple(values, max_display = 5)

  expect_true(grepl("and 10 more", result))
  expect_true(grepl("a", result))
  expect_false(grepl("f", result)) # Should not show 6th item
})

test_that("summarize_multiple handles NULL and empty values", {
  expect_equal(summarize_multiple(NULL), "Relevant data not found")
  expect_equal(summarize_multiple(character(0)), "Relevant data not found")
  expect_equal(summarize_multiple(c(NA, "")), "Relevant data not found")
})

test_that("calculate_date_range works with date range", {
  dates <- as.Date(c("2023-01-15", "2023-06-20", "2023-03-10"))
  result <- calculate_date_range(dates)

  expect_type(result, "character")
  expect_true(grepl("2023-01-15", result))
  expect_true(grepl("2023-06-20", result))
  expect_true(grepl("to", result))
})

test_that("calculate_date_range handles single date", {
  dates <- as.Date("2023-01-15")
  result <- calculate_date_range(dates)

  expect_equal(result, "2023-01-15")
  expect_false(grepl("to", result))
})

test_that("calculate_date_range handles NULL and NA", {
  expect_equal(calculate_date_range(NULL), "Relevant data not found")
  expect_equal(calculate_date_range(as.Date(NA)), "Relevant data not found")
})

test_that("generate_units_summary works with valid data", {
  measurement_data <- tibble(
    PARAMETER_NAME = c("Copper", "Lead", "Copper"),
    MEASURED_UNIT = c("mg/kg", "mg/kg", "µg/L")
  )

  parameters_data <- tibble(
    PARAMETER_NAME = c("Copper", "Lead")
  )

  result <- generate_units_summary(measurement_data, parameters_data)

  expect_type(result, "character")
  expect_true(grepl("Copper", result))
  expect_true(grepl("mg/kg", result))
  expect_true(grepl("µg/L", result))
})

test_that("generate_units_summary handles NULL data", {
  expect_equal(
    generate_units_summary(NULL, NULL),
    "Relevant data not found"
  )
})

# Tests: Gateway functions ----

test_that("get_gateway_summaries returns all expected fields", {
  module_data <- create_dummy_module_data()
  result <- get_gateway_summaries(module_data)

  expect_type(result, "list")
  expect_named(
    result,
    c("medium", "analyte", "location", "year", "units", "citation")
  )

  expect_true(grepl("Compartments", result$medium))
  expect_true(grepl("Parameters", result$analyte))
  expect_true(grepl("Countries", result$location))
  expect_true(grepl("2023", result$year))
  expect_type(result$citation, "character")
})

test_that("get_gateway_summaries handles missing data gracefully", {
  module_data <- list(
    references = NULL,
    parameters = NULL,
    compartments = NULL,
    sites = NULL,
    samples = NULL,
    measurements = NULL
  )

  result <- get_gateway_summaries(module_data)

  expect_true(all(result == "Relevant data not found"))
})

test_that("check_gateway_availability detects available data", {
  module_data <- create_dummy_module_data()
  result <- check_gateway_availability(module_data)

  expect_type(result, "list")
  expect_named(
    result,
    c("medium", "analyte", "location", "year", "units", "citation")
  )

  expect_true(result$medium)
  expect_true(result$analyte)
  expect_true(result$location)
  expect_true(result$year)
  expect_true(result$units)
  expect_true(result$citation)
})

test_that("check_gateway_availability detects missing data", {
  module_data <- list(
    references = NULL,
    parameters = NULL,
    compartments = NULL,
    sites = NULL,
    samples = NULL,
    measurements = NULL
  )

  result <- check_gateway_availability(module_data)

  expect_false(result$medium)
  expect_false(result$analyte)
  expect_false(result$location)
  expect_false(result$year)
  expect_false(result$units)
  expect_false(result$citation)
})

test_that("check_gateway_availability handles empty strings and NAs", {
  module_data <- list(
    compartments = tibble(
      ENVIRON_COMPARTMENT = c(NA, "")
    ),
    parameters = tibble(
      PARAMETER_NAME = c("Copper")
    )
  )

  result <- check_gateway_availability(module_data)

  expect_false(result$medium) # All NA or empty
  expect_true(result$analyte) # Has valid value
})

# Tests: Main summary function ----

test_that("summarise_CREED_details returns tibble with correct structure", {
  session_data <- create_session_data()
  result <- summarise_CREED_details(session_data)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("field", "value"))
  expect_equal(nrow(result), 11)

  expected_fields <- c(
    "source",
    "analytes",
    "medium",
    "study_area",
    "num_sites",
    "site_types",
    "num_samples",
    "sampling_period",
    "sampling_methods",
    "analytical_methods",
    "loq_info"
  )
  expect_equal(result$field, expected_fields)
})

test_that("summarise_CREED_details correctly summarizes reference data", {
  session_data <- create_session_data()
  result <- summarise_CREED_details(session_data)

  source_value <- result$value[result$field == "source"]
  expect_true(grepl("Smith, J.", source_value))
  expect_true(grepl("2023", source_value))
})

test_that("summarise_CREED_details correctly counts sites and samples", {
  session_data <- create_session_data()
  result <- summarise_CREED_details(session_data)

  num_sites <- result$value[result$field == "num_sites"]
  num_samples <- result$value[result$field == "num_samples"]

  expect_equal(num_sites, "4")
  expect_equal(num_samples, "4")
})

test_that("summarise_CREED_details separates sampling and analytical methods", {
  session_data <- create_session_data()
  result <- summarise_CREED_details(session_data)

  sampling_methods <- result$value[result$field == "sampling_methods"]
  analytical_methods <- result$value[result$field == "analytical_methods"]

  expect_true(grepl("Sampling Protocols", sampling_methods))
  expect_true(grepl("grab", sampling_methods))
  expect_true(grepl("Analytical Protocols", analytical_methods))
  expect_true(grepl("ICP-MS", analytical_methods))
})

test_that("summarise_CREED_details handles missing data", {
  session_data <- list(
    referenceData = NULL,
    parametersData = NULL,
    compartmentsData = NULL,
    sitesData = NULL,
    samplesData = NULL,
    measurementsData = NULL,
    methodsData = NULL
  )

  result <- summarise_CREED_details(session_data)

  expect_true(all(result$value == "Relevant data not found"))
})

test_that("summarise_CREED_details calculates LOQ/LOD info correctly", {
  session_data <- create_session_data()
  result <- summarise_CREED_details(session_data)

  loq_info <- result$value[result$field == "loq_info"]

  expect_true(grepl("LOQ:", loq_info))
  expect_true(grepl("LOD:", loq_info))
  expect_true(grepl("0.1 to 1", loq_info)) # Range of LOQ values
})

test_that("summarise_CREED_details handles NA dates correctly", {
  session_data <- create_session_data()
  session_data$samplesData$SAMPLING_DATE <- as.Date(NA)

  result <- summarise_CREED_details(session_data)
  sampling_period <- result$value[result$field == "sampling_period"]

  expect_equal(sampling_period, "Relevant data not found")
})

test_that("summarise_CREED_details deduplicates values correctly", {
  session_data <- create_session_data()
  # Add duplicate compartment
  session_data$compartmentsData <- add_row(
    session_data$compartmentsData,
    tibble(
      ENVIRON_COMPARTMENT = "Aquatic"
    )
  )

  result <- summarise_CREED_details(session_data)
  medium <- result$value[result$field == "medium"]

  # Should not have "Sediment" repeated multiple times in output
  sediment_count <- length(gregexpr("Sediment", medium)[[1]])
  expect_lte(sediment_count, 2) # Once in the value, once in potential plural
})

# test-creed-reliability-functions.R ----

test_that("summarise_CREED_reliability correctly summarizes compartments and protocols for RB1", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  rb1_value <- result$value[result$field == "RB1"]

  # Should contain compartments
  expect_true(grepl("compartment", rb1_value))
  expect_true(grepl("Sediment", rb1_value))

  # Should contain protocol information
  expect_true(grepl("protocol", rb1_value))
})

test_that("summarise_CREED_reliability correctly summarizes sites for RB4", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  rb4_value <- result$value[result$field == "RB4"]

  expect_true(grepl("4 sites", rb4_value))
  expect_true(grepl("Norway", rb4_value))
  expect_true(grepl("Oslo Fjord", rb4_value))
})

test_that("summarise_CREED_reliability correctly calculates date range for RB5", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  rb5_value <- result$value[result$field == "RB5"]

  expect_true(grepl("2023-01-15", rb5_value))
  expect_true(grepl("2023-09-05", rb5_value))
  expect_true(grepl("to", rb5_value))
})

test_that("summarise_CREED_reliability reuses sampling protocol summary for RB2, RB3, RB13", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  rb2_value <- result$value[result$field == "RB2"]
  rb3_value <- result$value[result$field == "RB3"]
  rb13_value <- result$value[result$field == "RB13"]

  # All three should be identical
  expect_equal(rb2_value, rb3_value)
  expect_equal(rb2_value, rb13_value)

  # Should contain sampling protocol info
  expect_true(grepl("sampling protocol", rb2_value))
})

test_that("summarise_CREED_reliability reuses analytical protocol summary for RB6, RB10, RB11, RB12", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  rb6_value <- result$value[result$field == "RB6"]
  rb10_value <- result$value[result$field == "RB10"]
  rb11_value <- result$value[result$field == "RB11"]
  rb12_value <- result$value[result$field == "RB12"]

  # All four should be identical
  expect_equal(rb6_value, rb10_value)
  expect_equal(rb6_value, rb11_value)
  expect_equal(rb6_value, rb12_value)

  # Should contain analytical protocol info
  expect_true(grepl("analytical protocol", rb6_value))
})

test_that("summarise_CREED_reliability calculates LOD/LOQ for RB7", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  rb7_value <- result$value[result$field == "RB7"]

  expect_true(grepl("LOQ:", rb7_value))
  expect_true(grepl("LOD:", rb7_value))
  expect_true(grepl("0.1 to 1", rb7_value)) # LOQ range
})

test_that("summarise_CREED_reliability calculates significant figures for RB15", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  rb15_value <- result$value[result$field == "RB15"]

  expect_true(grepl("significant figure", rb15_value))
})

test_that("summarise_CREED_reliability leaves blank fields empty for RB8, RB16, RB17, RB19", {
  session_data <- create_session_data()
  result <- summarise_CREED_reliability(session_data)

  expect_equal(result$value[result$field == "RB8"], "")
  expect_equal(result$value[result$field == "RB16"], "")
  expect_equal(result$value[result$field == "RB17"], "")
  expect_equal(result$value[result$field == "RB19"], "")
})

test_that("summarise_CREED_reliability handles missing data gracefully", {
  session_data <- list(
    compartmentsData = NULL,
    methodsData = NULL,
    sitesData = NULL,
    samplesData = NULL,
    measurementsData = NULL
  )

  result <- summarise_CREED_reliability(session_data)

  # Should still return 19 rows
  expect_equal(nrow(result), 19)

  # Non-empty fields should say "Relevant data not found"
  non_empty_fields <- c(
    "RB1",
    "RB2",
    "RB3",
    "RB4",
    "RB5",
    "RB6",
    "RB7",
    "RB9",
    "RB10",
    "RB11",
    "RB12",
    "RB13",
    "RB14",
    "RB15",
    "RB18"
  )
  for (field in non_empty_fields) {
    expect_equal(
      result$value[result$field == field],
      "Relevant data not found"
    )
  }
})

test_that("summarise_CREED_reliability handles uncertainty types for RB14 and RB18", {
  session_data <- create_session_data()
  # Add uncertainty data
  session_data$measurementsData$UNCERTAINTY_TYPE <- c("SD", "SD", "CI", "SD")
  session_data$measurementsData$MEASUREMENT_COMMENT <- c(
    "Good",
    "Fair",
    NA,
    "Good"
  )

  result <- summarise_CREED_reliability(session_data)

  rb14_value <- result$value[result$field == "RB14"]
  rb18_value <- result$value[result$field == "RB18"]

  # RB14 should have both uncertainty types and comments
  expect_true(grepl("Uncertainty types", rb14_value))
  expect_true(grepl("Measurement comments", rb14_value))

  # RB18 should have only uncertainty types
  expect_true(grepl("Uncertainty types", rb18_value))
  expect_true(grepl("SD", rb18_value))
  expect_true(grepl("CI", rb18_value))
})
