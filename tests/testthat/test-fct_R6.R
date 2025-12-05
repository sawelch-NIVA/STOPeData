# tests/testthat/test-campaign-data-r6.R

# Test: Can create new instance ----
test_that("CampaignData R6 generator creates valid instance", {
  campaign <- CampaignData_Table_R6_Generator$new()

  expect_s3_class(campaign, "CampaignData_Table_R6_Generator")
  expect_s3_class(campaign$data, "tbl_df")
  expect_equal(nrow(campaign$data), 0)
})

# Test: Schema structure ----
test_that("CampaignData schema has required columns", {
  campaign <- CampaignData_Table_R6_Generator$new()
  metadata <- campaign$metadata

  expect_true("column_name" %in% names(metadata))
  expect_true("data_type" %in% names(metadata))
  expect_true("mandatory" %in% names(metadata))
  expect_true("description" %in% names(metadata))
})

# Test: Empty tibble has correct structure ----
test_that("CampaignData creates correct empty tibble structure", {
  campaign <- CampaignData_Table_R6_Generator$new()

  expected_cols <- c(
    "CAMPAIGN_NAME_SHORT",
    "CAMPAIGN_NAME",
    "CAMPAIGN_START_DATE",
    "CAMPAIGN_END_DATE",
    "RELIABILITY_SCORE",
    "RELIABILITY_EVAL_SYS",
    "CONFIDENTIALITY_EXPIRY_DATE",
    "ORGANISATION",
    "ENTERED_BY",
    "ENTERED_DATE",
    "CAMPAIGN_COMMENT"
  )

  expect_equal(names(campaign$data), expected_cols)
  expect_equal(nrow(campaign$data), 0)
})

# Test: Column types match schema ----
test_that("CampaignData tibble has correct column types", {
  campaign <- CampaignData_Table_R6_Generator$new()

  expect_type(campaign$data$CAMPAIGN_NAME_SHORT, "character")
  expect_type(campaign$data$CAMPAIGN_NAME, "character")
  expect_s3_class(campaign$data$CAMPAIGN_START_DATE, "Date")
  expect_s3_class(campaign$data$CAMPAIGN_END_DATE, "Date")
  expect_type(campaign$data$ORGANISATION, "character")
})

# Test: Active bindings work ----
test_that("CampaignData active bindings return correct values", {
  campaign <- CampaignData_Table_R6_Generator$new()

  # Version
  expect_equal(campaign$version, "1.0.0")

  # Mandatory fields
  mandatory <- campaign$mandatory_fields
  expect_true("CAMPAIGN_NAME" %in% mandatory)
  expect_true("CAMPAIGN_START_DATE" %in% mandatory)
  expect_true("ORGANISATION" %in% mandatory)
  expect_false("CAMPAIGN_END_DATE" %in% mandatory)

  # Optional fields
  optional <- campaign$optional_fields
  expect_true("CAMPAIGN_END_DATE" %in% optional)
  expect_false("CAMPAIGN_NAME" %in% optional)

  # Row count
  expect_equal(campaign$n_rows, 0)
})

# Test: Can add valid rows ----
test_that("CampaignData accepts valid rows", {
  campaign <- CampaignData_Table_R6_Generator$new()

  # Using dummy function
  valid_row <- dummy_campaign_tibble()

  campaign$add_rows(valid_row)

  expect_equal(campaign$n_rows, 1)
  expect_equal(campaign$data$CAMPAIGN_NAME[1], valid_row$CAMPAIGN_NAME[1])
})

# Test: Rejects rows missing mandatory fields ----
test_that("CampaignData rejects rows missing mandatory columns", {
  campaign <- CampaignData_Table_R6_Generator$new()

  invalid_row <- tibble(
    CAMPAIGN_NAME_SHORT = "Test",
    CAMPAIGN_NAME = "Test Campaign"
    # Missing other mandatory fields
  )

  expect_error(
    campaign$add_rows(invalid_row),
    "Missing mandatory columns"
  )
})

# Test: Rejects rows with wrong types ----
test_that("CampaignData rejects rows with incorrect column types", {
  campaign <- CampaignData_Table_R6_Generator$new()

  invalid_row <- tibble(
    CAMPAIGN_NAME_SHORT = "Test",
    CAMPAIGN_NAME = "Test Campaign",
    CAMPAIGN_START_DATE = "2025-01-01", # Should be Date, not character
    ORGANISATION = "NIVA",
    ENTERED_BY = "Sam",
    ENTERED_DATE = Sys.Date()
  )

  expect_error(
    campaign$add_rows(invalid_row),
    "Type mismatch"
  )
})

# Test: validate_structure works ----
test_that("CampaignData structure validation works", {
  campaign <- CampaignData_Table_R6_Generator$new()

  expect_true(campaign$validate_structure())

  # Mess up the structure
  campaign$data <- campaign$data |> select(-CAMPAIGN_NAME)

  expect_error(
    campaign$validate_structure(),
    "Column names don't match schema"
  )
})

# Test: Can add multiple rows ----
test_that("CampaignData can add multiple rows sequentially", {
  campaign <- CampaignData_Table_R6_Generator$new()

  row1 <- dummy_campaign_tibble()
  row2 <- dummy_campaign_tibble() |>
    mutate(CAMPAIGN_NAME_SHORT = "Different")

  campaign$add_rows(row1)
  campaign$add_rows(row2)

  expect_equal(campaign$n_rows, 2)
})
