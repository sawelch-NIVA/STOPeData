# Test: fct_dummy_data.R ----
# Basic tests for dummy data creation functions
# These tests verify that dummy data functions execute without error
# and produce tibbles with the expected structure

# =========================================================================
# DUMMY TIBBLE FUNCTION TESTS
# =========================================================================
# Each test verifies:
# 1. Function executes without error or warning
# 2. Returns a tibble
# 3. Has at least one row
# 4. Column names match the parent initialise_* function
# =========================================================================

test_that("dummy_campaign_tibble creates valid tibble", {
  expect_no_error(result <- dummy_campaign_tibble())
  expect_no_warning(result <- dummy_campaign_tibble())
  expect_s3_class(
    result,
    "tbl_df"
  )
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_campaign_tibble()))
})

test_that("dummy_references_tibble creates valid tibble", {
  expect_no_error(result <- dummy_references_tibble())
  expect_no_warning(result <- dummy_references_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_references_tibble()))
})

test_that("dummy_sites_tibble creates valid tibble", {
  expect_no_error(result <- dummy_sites_tibble())
  expect_no_warning(result <- dummy_sites_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_sites_tibble()))
})

test_that("dummy_parameters_tibble creates valid tibble", {
  expect_no_error(result <- dummy_parameters_tibble())
  expect_no_warning(result <- dummy_parameters_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_parameters_tibble()))
})

test_that("dummy_compartments_tibble creates valid tibble", {
  expect_no_error(result <- dummy_compartments_tibble())
  expect_no_warning(result <- dummy_compartments_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_compartments_tibble()))
})

test_that("dummy_methods_tibble creates valid tibble", {
  expect_no_error(result <- dummy_methods_tibble())
  expect_no_warning(result <- dummy_methods_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_methods_tibble()))
})

test_that("dummy_samples_tibble creates valid tibble", {
  expect_no_error(result <- dummy_samples_tibble())
  expect_no_warning(result <- dummy_samples_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_samples_tibble()))
})

test_that("dummy_biota_tibble creates valid tibble", {
  expect_no_error(result <- dummy_biota_tibble())
  expect_no_warning(result <- dummy_biota_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_biota_tibble()))
})

test_that("dummy_measurements_tibble creates valid tibble", {
  expect_no_error(result <- dummy_measurements_tibble())
  expect_no_warning(result <- dummy_measurements_tibble())
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_equal(names(result), names(initialise_measurements_tibble()))
})


# =========================================================================
# SESSION DATA CREATION TESTS
# =========================================================================

test_that("create_dummy_session_data creates valid userData structure", {
  expect_no_error(result <- create_dummy_session_data())
  expect_no_warning(result <- create_dummy_session_data())
  expect_type(result, "list")

  # Check all expected data tibbles are present and populated
  expect_s3_class(result$campaignData, "tbl_df")
  expect_gt(nrow(result$campaignData), 0)

  expect_s3_class(result$referenceData, "tbl_df")
  expect_gt(nrow(result$referenceData), 0)

  expect_s3_class(result$sitesData, "tbl_df")
  expect_gt(nrow(result$sitesData), 0)

  expect_s3_class(result$parametersData, "tbl_df")
  expect_gt(nrow(result$parametersData), 0)

  expect_s3_class(result$compartmentsData, "tbl_df")
  expect_gt(nrow(result$compartmentsData), 0)

  expect_s3_class(result$methodsData, "tbl_df")
  expect_gt(nrow(result$methodsData), 0)

  expect_s3_class(result$samplesData, "tbl_df")
  expect_gt(nrow(result$samplesData), 0)

  expect_s3_class(result$biotaData, "tbl_df")
  expect_gt(nrow(result$biotaData), 0)

  expect_s3_class(result$measurementsData, "tbl_df")
  expect_gt(nrow(result$measurementsData), 0)
})

test_that("create_dummy_session_data sets validity flags to TRUE", {
  result <- create_dummy_session_data()

  expect_true(result$campaignDataValid)
  expect_true(result$referenceDataValid)
  expect_true(result$sitesDataValid)
  expect_true(result$parametersDataValid)
  expect_true(result$compartmentsDataValid)
  expect_true(result$methodsDataValid)
  expect_true(result$samplesDataValid)
  expect_true(result$biotaDataValid)
  expect_true(result$measurementsDataValid)
})

test_that("create_dummy_session_data column structures match initialise functions", {
  result <- create_dummy_session_data()

  expect_equal(names(result$campaignData), names(initialise_campaign_tibble()))
  expect_equal(
    names(result$referenceData),
    names(initialise_references_tibble())
  )
  expect_equal(names(result$sitesData), names(initialise_sites_tibble()))
  expect_equal(
    names(result$parametersData),
    names(initialise_parameters_tibble())
  )
  expect_equal(
    names(result$compartmentsData),
    names(initialise_compartments_tibble())
  )
  expect_equal(names(result$methodsData), names(initialise_methods_tibble()))
  expect_equal(names(result$samplesData), names(initialise_samples_tibble()))
  expect_equal(names(result$biotaData), names(initialise_biota_tibble()))
  expect_equal(
    names(result$measurementsData),
    names(initialise_measurements_tibble())
  )
})
