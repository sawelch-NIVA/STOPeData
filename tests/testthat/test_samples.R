test_that("generate_sample_id_with_components creates correct IDs", {
  # Test data
  site_codes <- c("SITE_001", "LAKE_042", "RIVER_003")
  parameter_names <- c("Total Phosphorus", "pH", "Dissolved Oxygen")
  environ_compartments <- c("Water", "Sediment", "Water")
  environ_compartment_subs <- c("Surface Water", "Top Layer", "Bottom Water")
  dates <- c("2024-01-15", "2024-03-22", "2024-06-30")
  subsamples <- c("1", "2", "3")

  # Generate IDs
  result <- generate_sample_id_with_components(
    site_code = site_codes,
    parameter_name = parameter_names,
    environ_compartment = environ_compartments,
    environ_compartment_sub = environ_compartment_subs,
    date = dates,
    subsample = subsamples
  )

  # Expected results
  expected <- c(
    "SITE_001-TotalPho-SurfaceWater-2024-01-15-R-1",
    "LAKE_042-pH-TopLayer-2024-03-22-R-2",
    "RIVER_003-Dissolve-BottomWater-2024-06-30-R-3"
  )

  # Tests
  expect_equal(result, expected)
  expect_length(result, 3)
  expect_type(result, "character")
})
