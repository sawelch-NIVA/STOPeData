# tests/testthat/test_utils.r ----

test_that("abbreviate_string works for all case styles", {
  input <- "dogs and cats are best friends"
  n <- 3L

  # Test lower case
  expect_equal(
    abbreviate_string(input, n, "lower"),
    "dogsandcats"
  )

  # Test upper case
  expect_equal(
    abbreviate_string(input, n, "upper"),
    "DOGSANDCATS"
  )

  # Test sentence case
  expect_equal(
    abbreviate_string(input, n, "sentence"),
    "Dogsandcats"
  )

  # Test snake case
  expect_equal(
    abbreviate_string(input, n, "snake"),
    "dogs_and_cats"
  )

  # Test title case
  expect_equal(
    abbreviate_string(input, n, "title"),
    "DogsAndCats"
  )

  # Test screaming snake case
  expect_equal(
    abbreviate_string(input, n, "screamingsnake"),
    "DOGS_AND_CATS"
  )

  # Test camel case
  expect_equal(
    abbreviate_string(input, n, "camel"),
    "dogsAndCats"
  )
})

test_that("abbreviate_string handles special characters", {
  expect_equal(
    abbreviate_string("Total-N Concentration", n_words = 2L, "snake"),
    "total-n_concentration"
  )

  expect_equal(
    abbreviate_string("Water & Sediment Quality", n_words = 3L, "title"),
    "Water&Sediment"
  )
})

test_that("abbreviate_string respects n_words limit", {
  result <- abbreviate_string("one two three four five", n_words = 2L, "snake")
  expect_equal(result, "one_two")
  expect_equal(length(strsplit(result, "_")[[1]]), 2)
})
