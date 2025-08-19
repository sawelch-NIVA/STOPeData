# Tests for Sample Helper Functions ----

# Test data setup ----
setup_test_data <- function() {
  list(
    sites = data.frame(
      SITE_CODE = c("SITE_001", "SITE_002", "SITE_003"),
      SITE_NAME = c("River Site A", "Lake Site B", "Coastal Site C"),
      stringsAsFactors = FALSE
    ),
    parameters = data.frame(
      STRESSOR_NAME = c("Copper", "Lead", "pH", "Dissolved oxygen"),
      STRESSOR_TYPE = c(
        "Stressor",
        "Stressor",
        "Quality parameter",
        "Quality parameter"
      ),
      stringsAsFactors = FALSE
    ),
    compartments = data.frame(
      ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Terrestrial"),
      ENVIRON_COMPARTMENT_SUB = c(
        "Freshwater",
        "Marine/Salt Water",
        "Soil A Horizon (Topsoil)"
      ),
      MEASURED_CATEGORY = c("External", "External", "External"),
      stringsAsFactors = FALSE
    ),
    existing_samples = data.frame(
      SITE_CODE = c("SITE_001", "SITE_002"),
      PARAMETER_NAME = c("Copper", "Lead"),
      ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic"),
      ENVIRON_COMPARTMENT_SUB = c("Freshwater", "Marine/Salt Water"),
      MEASURED_CATEGORY = c("External", "External"),
      SAMPLING_DATE = c("2024-01-15", "2024-01-15"),
      REPLICATE = c(1, 1),
      stringsAsFactors = FALSE
    )
  )
}

# Test parse_compartment_selections ----
test_that("parse_compartment_selections correctly parses merged selections", {
  test_data <- setup_test_data()
  compartments_data <- test_data$compartments

  # Test basic parsing
  selections <- c(
    "Aquatic | Freshwater",
    "Terrestrial | Soil A Horizon (Topsoil)"
  )
  result <- parse_compartment_selections(selections, compartments_data)

  expect_equal(nrow(result), 2)
  expect_equal(result$ENVIRON_COMPARTMENT[1], "Aquatic")
  expect_equal(result$ENVIRON_COMPARTMENT_SUB[1], "Freshwater")
  expect_equal(result$MEASURED_CATEGORY[1], "External")

  # Test empty selections
  result <- parse_compartment_selections(character(0), compartments_data)
  expect_equal(nrow(result), 0)
  expect_true(all(
    c(
      "ENVIRON_COMPARTMENT",
      "ENVIRON_COMPARTMENT_SUB",
      "MEASURED_CATEGORY"
    ) %in%
      names(result)
  ))

  # Test invalid selection
  expect_warning(
    parse_compartment_selections("Invalid | Selection", compartments_data),
    "Could not find compartment combination"
  )
})

# Test generate_sample_id_with_components ----
test_that("generate_sample_id_with_components creates valid sample IDs", {
  # Test basic functionality
  result <- generate_sample_id_with_components(
    "SITE_001",
    "Copper",
    "Aquatic",
    "Freshwater",
    "2024-01-15",
    1
  )
  expect_true(grepl("SITE_001", result))
  expect_true(grepl("Copper", result))
  expect_true(grepl("Aquati", result))
  expect_true(grepl("20240115", result))

  # Test with replicate
  result <- generate_sample_id_with_components(
    "SITE_001",
    "Copper",
    "Aquatic",
    "Freshwater",
    "2024-01-15",
    2
  )
  expect_true(grepl("_R02", result))

  # Test special character handling
  result <- generate_sample_id_with_components(
    "SITE-001",
    "Lead (II)",
    "Aquatic/Marine",
    "Fresh-water",
    "2024-12-31",
    1
  )
  expect_false(grepl("[^A-Za-z0-9_//-]", result)) # Should contain only alphanumeric and underscores
})

# Test combination_exists_with_components ----
test_that("combination_exists_with_components correctly identifies existing combinations", {
  test_data <- setup_test_data()
  existing <- test_data$existing_samples

  # Test existing combination
  result <- combination_exists_with_components(
    existing,
    "SITE_001",
    "Copper",
    "Aquatic",
    "Freshwater",
    "External",
    "2024-01-15",
    1
  )
  expect_true(result)

  # Test non-existing combination
  result <- combination_exists_with_components(
    existing,
    "SITE_003",
    "pH",
    "Terrestrial",
    "Soil A Horizon (Topsoil)",
    "External",
    "2024-01-15",
    1
  )
  expect_false(result)

  # Test with different replicate
  result <- combination_exists_with_components(
    existing,
    "SITE_001",
    "Copper",
    "Aquatic",
    "Freshwater",
    "External",
    "2024-01-15",
    2
  )
  expect_false(result)

  # Test with empty data frame
  result <- combination_exists_with_components(
    data.frame(),
    "SITE_001",
    "Copper",
    "Aquatic",
    "Freshwater",
    "External",
    "2024-01-15",
    1
  )
  expect_false(result)
})

# Test create_sample_combinations ----
test_that("create_sample_combinations generates correct combinations", {
  test_data <- setup_test_data()

  # Test basic combination generation
  result <- create_sample_combinations(
    sites = c("SITE_001", "SITE_002"),
    parameters = c("pH", "Dissolved oxygen"),
    compartment_selections = c("Aquatic | Freshwater"),
    dates = c("2024-01-15"),
    replicates = 1,
    existing_data = data.frame(),
    available_compartments = test_data$compartments,
    available_sites = test_data$sites,
    available_parameters = test_data$parameters
  )

  expect_equal(nrow(result$combinations), 4) # 2 sites × 2 params × 1 compartment × 1 date
  expect_equal(result$skipped, 0)

  # Check column structure
  expected_cols <- c(
    "SITE_CODE",
    "SITE_NAME",
    "PARAMETER_NAME",
    "PARAMETER_TYPE",
    "ENVIRON_COMPARTMENT",
    "ENVIRON_COMPARTMENT_SUB",
    "MEASURED_CATEGORY",
    "SAMPLING_DATE",
    "REPLICATE",
    "REPLICATE_ID",
    "SAMPLE_ID"
  )
  expect_true(all(expected_cols %in% names(result$combinations)))

  # Test with replicates
  result <- create_sample_combinations(
    sites = c("SITE_001"),
    parameters = c("pH"),
    compartment_selections = c("Aquatic | Freshwater"),
    dates = c("2024-01-15"),
    replicates = 3,
    existing_data = data.frame(),
    available_compartments = test_data$compartments,
    available_sites = test_data$sites,
    available_parameters = test_data$parameters
  )

  expect_equal(nrow(result$combinations), 3) # 1 combination × 3 replicates
  expect_equal(result$skipped, 0)
  expect_equal(unique(result$combinations$REPLICATE), c(1, 2, 3))

  # Test duplicate filtering
  existing <- test_data$existing_samples
  result <- create_sample_combinations(
    sites = c("SITE_001", "SITE_002"),
    parameters = c("Copper", "Lead"),
    compartment_selections = c(
      "Aquatic | Freshwater",
      "Aquatic | Marine/Salt Water"
    ),
    dates = c("2024-01-15"),
    replicates = 1,
    existing_data = existing,
    available_compartments = test_data$compartments,
    available_sites = test_data$sites,
    available_parameters = test_data$parameters
  )

  expect_equal(nrow(result$combinations), 6) # 8 total - 2 existing
  expect_equal(result$skipped, 2)
})

# Test update_combination_preview ----
test_that("update_combination_preview generates correct preview text", {
  # Test basic preview
  result <- update_combination_preview(2, 3, 1, 2, 1)
  expect_s3_class(result, "shiny.tag")

  # Extract text content for testing
  result_html <- as.character(result)
  expect_true(grepl("2 sites", result_html))
  expect_true(grepl("3 parameters", result_html))
  expect_true(grepl("1 compartments", result_html))
  expect_true(grepl("2 dates", result_html))
  expect_true(grepl("1 replicates", result_html))
  expect_true(grepl("12 total samples", result_html))

  # Test with replicates
  result <- update_combination_preview(1, 1, 1, 1, 3)
  result_html <- as.character(result)
  expect_true(grepl("3 total samples", result_html))

  # Test zero values
  result <- update_combination_preview(0, 0, 0, 0, 1)
  result_html <- as.character(result)
  expect_true(grepl("0 total samples", result_html))
})

# Test init_samples_df ----
test_that("init_samples_df creates correct empty data frame", {
  result <- init_samples_df()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 11)

  expected_cols <- c(
    "SITE_CODE",
    "SITE_NAME",
    "PARAMETER_NAME",
    "PARAMETER_TYPE",
    "ENVIRON_COMPARTMENT",
    "ENVIRON_COMPARTMENT_SUB",
    "MEASURED_CATEGORY",
    "SAMPLING_DATE",
    "REPLICATE",
    "REPLICATE_ID",
    "SAMPLE_ID"
  )
  expect_equal(names(result), expected_cols)

  # Test that most columns are character type
  char_cols <- c(
    "SITE_CODE",
    "SITE_NAME",
    "PARAMETER_NAME",
    "PARAMETER_TYPE",
    "ENVIRON_COMPARTMENT",
    "ENVIRON_COMPARTMENT_SUB",
    "MEASURED_CATEGORY",
    "SAMPLING_DATE",
    "REPLICATE_ID",
    "SAMPLE_ID"
  )
  expect_true(all(sapply(result[char_cols], is.character)))
  expect_true(is.numeric(result$REPLICATE))
})

# Integration tests ----
test_that("functions work together correctly", {
  test_data <- setup_test_data()

  # Generate combinations
  result <- create_sample_combinations(
    sites = test_data$sites$SITE_CODE[1:2],
    parameters = test_data$parameters$STRESSOR_NAME[1:2],
    compartment_selections = c("Aquatic | Freshwater"),
    dates = c("2024-01-15"),
    replicates = 1,
    existing_data = data.frame(),
    available_compartments = test_data$compartments,
    available_sites = test_data$sites,
    available_parameters = test_data$parameters
  )

  combinations <- result$combinations

  # Test that sample IDs are unique
  expect_equal(
    length(combinations$SAMPLE_ID),
    length(unique(combinations$SAMPLE_ID))
  )

  # Test that compartment parsing worked correctly
  expect_true(all(combinations$ENVIRON_COMPARTMENT == "Aquatic"))
  expect_true(all(combinations$ENVIRON_COMPARTMENT_SUB == "Freshwater"))
  expect_true(all(combinations$MEASURED_CATEGORY == "External"))

  # Test that combinations can be checked for existence
  first_combo <- combinations[1, ]
  exists_result <- combination_exists_with_components(
    combinations,
    first_combo$SITE_CODE,
    first_combo$PARAMETER_NAME,
    first_combo$ENVIRON_COMPARTMENT,
    first_combo$ENVIRON_COMPARTMENT_SUB,
    first_combo$MEASURED_CATEGORY,
    first_combo$SAMPLING_DATE,
    first_combo$REPLICATE
  )
  expect_true(exists_result)
})
