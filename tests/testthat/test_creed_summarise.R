# test-creed-functions.R ----
# tests for all the little CREED summarise_* functions designed to provide a text summary of tables or individual variables

# Setup: Create dummy data matching actual structure ----
dummy <- create_dummy_module_data()

# Tests for calculate_coordinate_precision() ----
test_that("calculate_coordinate_precision finds minimum decimal places", {
  result <- calculate_coordinate_precision(
    dummy$sites$LATITUDE,
    dummy$sites$LONGITUDE
  )
  # Lat: 59.9139 (4), 60.3913 (4), 59.3293 (4)
  # Lon: 10.7522 (4), 5.3221 (4), 18.0686 (4)
  expect_equal(result, 4)
})

test_that("calculate_coordinate_precision handles integer coordinates", {
  lat <- c(60, 61, 62)
  lon <- c(10, 11, 12)
  result <- calculate_coordinate_precision(lat, lon)
  expect_equal(result, 0)
})

test_that("calculate_coordinate_precision returns N/A for all NA", {
  lat <- c(NA, NA, NA)
  lon <- c(NA, NA, NA)
  result <- calculate_coordinate_precision(lat, lon)
  expect_equal(result, "N/A")
})

# Tests for summarise_sites() ----
test_that("summarise_sites returns full summary with all parameters TRUE", {
  result <- summarise_sites(
    dummy$sites,
    COUNTRY = TRUE,
    AREA = TRUE,
    SITE_GEOGRAPHIC_FEATURE = TRUE,
    SITE_GEOGRAPHIC_FEATURE_SUB = TRUE,
    PRECISION = TRUE
  )
  expect_match(result, "3 sites")
  expect_match(result, "Norway, Sweden") # including brackets and escape characters messes with the test somehow
  expect_match(result, "Oslo, Bergen, Stockholm")
  expect_match(result, "Distinct features")
  expect_match(result, "Lowest coordinate precision: 4")
})

test_that("summarise_sites respects FALSE parameters", {
  result <- summarise_sites(
    dummy$sites,
    COUNTRY = FALSE,
    AREA = FALSE,
    PRECISION = FALSE
  )
  expect_match(result, "3 sites")
  expect_false(grepl("Countries", result))
  expect_false(grepl("Areas", result))
  expect_false(grepl("precision", result))
})

test_that("summarise_sites handles coordinate precision correctly", {
  # Create custom test data with varying decimal places
  test_coords <- tibble(
    SITE_ID = c("S1", "S2", "S3"),
    COUNTRY = c("Norway", "Norway", "Sweden"),
    AREA = c("Oslo", "Bergen", "Stockholm"),
    SITE_GEOGRAPHIC_FEATURE = c("Fjord", "Fjord", "Lake"),
    SITE_GEOGRAPHIC_FEATURE_SUB = c("Inner", "Outer", "Shallow"),
    LATITUDE = c(60.1, 60.12, 60.123),
    LONGITUDE = c(10.1234, 10.12, 10.1)
  )

  result <- summarise_sites(test_coords, PRECISION = TRUE)
  expect_match(result, "Lowest coordinate precision: 1")
})
