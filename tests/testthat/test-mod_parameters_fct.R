# Test data setup ----
test_dummy_parameters <- data.frame(
  PARAMETER_TYPE = c("Quality parameter", "Quality parameter", "Chemical"),
  PARAMETER_NAME = c("pH", "Dissolved oxygen", "Benzene"),
  PARAMETER_TYPE_SUB = c("pH", "Dissolved oxygen", "dunno"),
  MEASURED_TYPE = c("Physical parameter", "Concentration", "dunno"),
  CAS_RN = c(NA, "7782-44-7", "71-43-2"),
  PUBCHEM_CID = c(NA, "977", "241"),
  INCHIKEY_SD = c(NA, "MYMOFIZGZYHOMD-UHFFFAOYSA-N", "UHOVQNZJYSORNB-UHFFFAOYSA-N"),
  stringsAsFactors = FALSE
)

# Tests ----
test_that("get_parameters_of_types returns correct parameter names", {
  result <- get_parameters_of_types("Quality parameter", test_dummy_parameters)

  expect_type(result, "character")
  expect_equal(result[1], "-- New Parameter --")
  expect_true("pH" %in% result)
  expect_true("Dissolved oxygen" %in% result)
  expect_false("Benzene" %in% result)  # Should not include chemicals
})

test_that("create_existing_parameter returns correct parameter data", {
  result <- create_existing_parameter("Quality parameter", "pH", test_dummy_parameters)

  expect_s3_class(result, "data.frame")
  expect_equal(result$PARAMETER_NAME, "pH")
  expect_equal(result$PARAMETER_TYPE, "Quality parameter")
  expect_equal(result$MEASURED_TYPE, "Physical parameter")
})

test_that("create_new_parameter returns blank template", {
  result <- create_new_parameter("Chemical")

  expect_s3_class(result, "data.frame")
  expect_equal(result$PARAMETER_TYPE, "Chemical")
  expect_equal(result$PARAMETER_NAME, "")
  expect_equal(result$MEASURED_TYPE, "Concentration")
  expect_equal(nrow(result), 1)
})
