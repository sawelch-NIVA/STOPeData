# tests/testthat/test-rprotocols.r ----

# Setup ----
test_that("generate_protocol_id handles a vector of protocols", {
  types <- c("Sampling Protocol", "Analytical Protocol")
  names <- c("Water Sample", "LC-MS Analysis")
  sequences <- c(1, 2)
  result <- generate_protocol_id(types, names, sequences, "Study2024")

  expect_equal(
    result,
    c("S01_WaterSample_Study2024", "A02_LcmsAnalysis_Study2024")
  )
})
