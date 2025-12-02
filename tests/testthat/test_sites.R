# tests/testthat/test-compartment_creation.r ----
testSession <- NULL
testSession$userData$reactiveValues <- create_dummy_session_data()


test_that("create_compartment_combination matches initialise structure", {
  result <- create_new_site(
    1,
    base_code = "Fake_Site_Code",
    session = testSession
  )
  template <- initialise_sites_tibble()

  # Same column names and types
  expect_equal(names(result), names(template))
  expect_equal(sapply(result, class), sapply(template, class))

  # Has exactly one row
  expect_equal(nrow(result), 1)

  # Values are correctly populated
  expect_equal(result$ENTERED_BY, "test_user")
})
