# tests/testthat/test-compartment_creation.r ----

test_that("create_compartment_combination matches initialise structure", {
  result <- create_compartment_combination("Water", "Surface", "Dissolved")
  template <- initialise_compartments_tibble()

  # Same column names and types
  expect_equal(names(result), names(template))
  expect_equal(sapply(result, class), sapply(template, class))

  # Has exactly one row
  expect_equal(nrow(result), 1)

  # Values are correctly populated
  expect_equal(result$ENVIRON_COMPARTMENT, "Water")
  expect_equal(result$ENVIRON_COMPARTMENT_SUB, "Surface")
  expect_equal(result$MEASURED_CATEGORY, "Dissolved")
})
