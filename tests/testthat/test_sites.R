# tests/testthat/test-compartment_creation.r ----

test_that("create_compartment_combination matches initialise structure", {
  result <- create_new_site(
    1,
    "Fake_Site_Code",
    "Tim the Tester"
  )
  template <- initialise_sites_tibble()

  # Same column names and types
  expect_equal(names(result), names(template))
  expect_equal(sapply(result, class), sapply(template, class))

  # Has exactly one row
  expect_equal(nrow(result), 1)

  # Values are correctly populated
  expect_equal(result$ENTERED_BY, "Tim the Tester")
})
