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
      PARAMETER_NAME = c("Copper", "Lead", "pH", "Dissolved oxygen"),
      STRESSOR_TYPE = c("Stressor", "Stressor", "Quality parameter", "Quality parameter"),
      stringsAsFactors = FALSE
    ),
    compartments = data.frame(
      ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Terrestrial"),
      ENVIRON_COMPARTMENT_SUB = c("Freshwater", "Marine/Salt Water", "Soil A Horizon (Topsoil)"),
      MEASURED_CATEGORY = c("External", "External", "External"),
      stringsAsFactors = FALSE
    ),
    existing_samples = data.frame(
      SITE_CODE = c("SITE_001", "SITE_002"),
      PARAMETER_NAME = c("Copper", "Lead"),
      COMPARTMENT = c("Aquatic | Freshwater", "Aquatic | Marine/Salt Water"),
      SAMPLING_DATE = c("2024-01-15", "2024-01-15"),
      stringsAsFactors = FALSE
    )
  )
}

# Test generate_sample_id ----
test_that("generate_sample_id creates valid sample IDs", {
  # Test basic functionality
  result <- generate_sample_id("SITE_001", "Copper", "Aquatic | Freshwater", "2024-01-15")
  expected <- "SITE_001_Copper_AquaticFre_20240115"
  expect_equal(result, expected)

  # Test special character handling
  result <- generate_sample_id("SITE-001", "Lead (II)", "Aquatic/Marine", "2024-12-31")
  expected <- "SITE-001_LeadII_AquaticMar_20241231"
  expect_equal(result, expected)

  # Test long parameter name truncation
  result <- generate_sample_id("SITE_001", "Very Long Parameter Name That Should Be Truncated", "Short", "2024-01-01")
  expected <- "SITE_001_VeryLongPa_Short_20240101"
  expect_equal(result, expected)

  # Test empty inputs
  result <- generate_sample_id("", "", "", "2024-01-01")
  expected <- "___20240101"
  expect_equal(result, expected)
})

# Test combination_exists ----
test_that("combination_exists correctly identifies existing combinations", {
  test_data <- setup_test_data()
  existing <- test_data$existing_samples

  # Test existing combination
  result <- combination_exists(existing, "SITE_001", "Copper", "Aquatic | Freshwater", "2024-01-15")
  expect_true(result)

  # Test non-existing combination
  result <- combination_exists(existing, "SITE_003", "pH", "Terrestrial", "2024-01-15")
  expect_false(result)

  # Test with different date
  result <- combination_exists(existing, "SITE_001", "Copper", "Aquatic | Freshwater", "2024-01-16")
  expect_false(result)

  # Test with empty data frame
  result <- combination_exists(data.frame(), "SITE_001", "Copper", "Compartment", "2024-01-15")
  expect_false(result)
})

# Test create_sample_combinations ----
test_that("create_sample_combinations generates correct combinations", {
  test_data <- setup_test_data()
  existing <- test_data$existing_samples

  # Test basic combination generation
  result <- create_sample_combinations(
    sites = c("SITE_001", "SITE_002"),
    parameters = c("pH", "Dissolved oxygen"),
    compartments = c("Aquatic | Freshwater"),
    dates = c("2024-01-15"),
    existing_data = data.frame()
  )

  expect_equal(nrow(result$combinations), 4) # 2 sites × 2 params × 1 compartment × 1 date
  expect_equal(result$skipped, 0)
  expect_true(all(c("SITE_CODE", "PARAMETER_NAME", "COMPARTMENT", "SAMPLE_ID") %in% names(result$combinations)))

  # Test duplicate filtering
  result <- create_sample_combinations(
    sites = c("SITE_001", "SITE_002"),
    parameters = c("Copper", "Lead"),
    compartments = c("Aquatic | Freshwater", "Aquatic | Marine/Salt Water"),
    dates = c("2024-01-15"),
    existing_data = existing
  )

  expect_equal(nrow(result$combinations), 6) # 6 total = (2 sites * 2 parameters * 2 compartments) - 2 existing
  expect_equal(result$skipped, 2)

  # Test multiple dates
  result <- create_sample_combinations(
    sites = c("SITE_001"),
    parameters = c("pH"),
    compartments = c("Aquatic | Freshwater"),
    dates = c("2024-01-15", "2024-01-16"),
    existing_data = data.frame()
  )

  expect_equal(nrow(result$combinations), 2) # 1 site × 1 param × 1 compartment × 2 dates
  expect_equal(result$skipped, 0)

  # Test empty inputs
  empty_combination <- function() create_sample_combinations(
    sites = character(0),
    parameters = c("pH"),
    compartments = c("Aquatic"),
    dates = c("2024-01-15"),
    existing_data = data.frame()
  )

  # Should error out
  expect_error(
    empty_combination(),
    regexp = "is not TRUE"
    )

})

# Test update_combination_preview ----
test_that("update_combination_preview generates correct preview text", {
  # Test basic preview
  result <- update_combination_preview(2, 3, 1, 2)
  expect_s3_class(result, "shiny.tag")

  # Extract text content for testing
  result_html <- as.character(result)
  expect_true(grepl("2 sites", result_html))
  expect_true(grepl("3 parameters", result_html))
  expect_true(grepl("1 compartments", result_html))
  expect_true(grepl("2 dates", result_html))
  expect_true(grepl("12 total combinations", result_html))

  # Test zero values
  result <- update_combination_preview(0, 0, 0, 0)
  result_html <- as.character(result)
  expect_true(grepl("0 total combinations", result_html))

  # Test single values
  result <- update_combination_preview(1, 1, 1, 1)
  result_html <- as.character(result)
  expect_true(grepl("1 total combinations", result_html))
})

# Test init_samples_df ----
test_that("init_samples_df creates correct empty data frame", {
  result <- init_samples_df()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 8)

  expected_cols <- c(
    "SITE_CODE", "SITE_NAME", "PARAMETER_NAME", "PARAMETER_TYPE",
    "COMPARTMENT", "COMPARTMENT_SUB", "SAMPLING_DATE", "SAMPLE_ID"
  )
  expect_equal(names(result), expected_cols)

  # Test column types
  expect_true(all(sapply(result, is.character)))
})

# Integration tests ----
test_that("functions work together correctly", {
  test_data <- setup_test_data()

  # Generate combinations
  result <- create_sample_combinations(
    sites = test_data$sites$SITE_CODE[1:2],
    parameters = test_data$parameters$PARAMETER_NAME[1:2],
    compartments = c("Aquatic | Freshwater"),
    dates = c("2024-01-15"),
    existing_data = data.frame()
  )

  combinations <- result$combinations

  # Test that sample IDs are unique
  expect_equal(length(combinations$SAMPLE_ID), length(unique(combinations$SAMPLE_ID)))

  # Test that combinations can be checked for existence
  first_combo <- combinations[1, ]
  exists_result <- combination_exists(
    combinations,
    first_combo$SITE_CODE,
    first_combo$PARAMETER_NAME,
    first_combo$COMPARTMENT,
    first_combo$SAMPLING_DATE
  )
  expect_true(exists_result)
})
