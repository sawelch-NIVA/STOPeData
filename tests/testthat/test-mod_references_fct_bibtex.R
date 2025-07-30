library(testthat)

# Test data ----
simple_bibtex <- "@article{testkey2023,
  title={A Test Article},
  author={Smith, John and Doe, Jane},
  journal={Test Journal},
  year={2023},
  volume={1},
  pages={1--10}
}"

test_that("bib_string2df_alt converts simple bibtex string correctly", {
  result <- bib_string2df_alt(simple_bibtex)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Check key fields are parsed correctly
  expect_equal(result$BIBTEXKEY, "testkey2023")
  expect_equal(result$TITLE, "A Test Article")
  expect_equal(result$YEAR, 2023)
  expect_equal(result$CATEGORY, "ARTICLE")
})

test_that("bib_string2df_alt passes additional arguments to bib2df", {
  # Test that separate_names parameter gets passed through
  result_default <- bib_string2df_alt(simple_bibtex)
  result_separated <- bib_string2df_alt(simple_bibtex, separate_names = TRUE)

  expect_s3_class(result_default, "data.frame")
  expect_s3_class(result_separated, "data.frame")

  # Both should have same number of rows but potentially different structure
  expect_equal(nrow(result_default), nrow(result_separated))
})
