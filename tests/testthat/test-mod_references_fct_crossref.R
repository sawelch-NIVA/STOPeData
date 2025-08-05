# Tests for Crossref Functions ----
# Simple tests for DOI/PMID validation and lookup functions

library(lubridate)
library(glue)
library(purrr)


# Helper function for offline testing
skip_if_offline <- function() {
  if (!has_internet()) {
    testthat::skip("No internet connection")
  }
}

# Simple internet connectivity check
has_internet <- function() {
  tryCatch(
    {
      httr::GET("https://httpbin.org/status/200", httr::timeout(5))
      TRUE
    },
    error = function(e) FALSE
  )
}


test_that("validate_doi_format works correctly", {
  # Valid DOIs
  expect_true(validate_doi_format("10.1038/nature12373"))
  expect_true(validate_doi_format("https://doi.org/10.1038/nature12373"))
  expect_true(validate_doi_format("http://dx.doi.org/10.1038/nature12373"))
  expect_true(validate_doi_format("10.1234/example.test-2023"))

  # Invalid DOIs
  expect_false(validate_doi_format("invalid"))
  expect_false(validate_doi_format("10.123/test")) # registrant too short
  expect_false(validate_doi_format("11.1234/test")) # wrong prefix
  expect_false(validate_doi_format(""))
  expect_false(validate_doi_format(NA))
  expect_false(validate_doi_format(123)) # not character
})

test_that("validate_pmid_format works correctly", {
  # Valid PMIDs
  expect_true(validate_pmid_format("12345678"))
  expect_true(validate_pmid_format("PMID:12345678"))
  expect_true(validate_pmid_format("PMID: 12345678"))
  expect_true(validate_pmid_format("pmid:123"))
  expect_true(validate_pmid_format("1"))

  # Invalid PMIDs
  expect_false(validate_pmid_format("invalid"))
  expect_false(validate_pmid_format("123456789")) # too long
  expect_false(validate_pmid_format(""))
  expect_false(validate_pmid_format(NA))
  expect_false(validate_pmid_format(123)) # not character
})

test_that("extract_clean_doi works correctly", {
  expect_equal(extract_clean_doi("10.1038/nature12373"), "10.1038/nature12373")
  expect_equal(
    extract_clean_doi("https://doi.org/10.1038/nature12373"),
    "10.1038/nature12373"
  )
  expect_equal(
    extract_clean_doi("http://dx.doi.org/10.1038/nature12373"),
    "10.1038/nature12373"
  )
  expect_equal(
    extract_clean_doi("  10.1038/nature12373  "),
    "10.1038/nature12373"
  )
})

test_that("extract_clean_pmid works correctly", {
  expect_equal(extract_clean_pmid("12345678"), "12345678")
  expect_equal(extract_clean_pmid("PMID:12345678"), "12345678")
  expect_equal(extract_clean_pmid("PMID: 12345678"), "12345678")
  expect_equal(extract_clean_pmid("pmid:12345678"), "12345678")
  expect_equal(extract_clean_pmid("  12345678  "), "12345678")
})

test_that("pmid_to_doi handles errors gracefully", {
  # Test with invalid PMID (should return error structure)
  result <- pmid_to_doi("invalid")
  expect_false(result$success)
  expect_null(result$doi)
  expect_true(is.character(result$message))

  # Test with non-existent PMID
  result <- pmid_to_doi("99999999")
  expect_false(result$success)
  expect_null(result$doi)
  expect_true(is.character(result$message))
})

test_that("lookup_crossref_doi handles errors gracefully", {
  # Test with invalid DOI (should return error structure)
  result <- lookup_crossref_doi("10.1234/nonexistent")
  expect_false(result$success)
  expect_null(result$data)
  expect_true(is.character(result$message))

  # Test with malformed DOI
  result <- lookup_crossref_doi("invalid.doi")
  expect_false(result$success)
  expect_null(result$data)
  expect_true(is.character(result$message))
})

test_that("map_crossref_to_reference_fields handles empty data", {
  # Test error handling with invalid input
  expect_error(map_crossref_to_reference_fields(data.frame()))
  expect_error(map_crossref_to_reference_fields(NULL))

  # Test with minimal valid data frame
  minimal_df <- data.frame(
    title = "Test Title",
    type = "journal-article",
    stringsAsFactors = FALSE
  )

  result <- map_crossref_to_reference_fields(minimal_df)
  expect_true(is.list(result))
  expect_equal(result$REFERENCE_TYPE, "journal")
  expect_equal(result$TITLE, "Test Title")
  expect_true(is.na(result$AUTHOR))
})

test_that("validate_and_lookup_identifier handles input validation", {
  # Test empty input
  result <- validate_and_lookup_identifier("")
  expect_false(result$success)
  expect_true(grepl("enter", result$message, ignore.case = TRUE))

  # Test invalid input type
  result <- validate_and_lookup_identifier(123)
  expect_false(result$success)
  expect_true(grepl("character string", result$message))

  # Test invalid format
  result <- validate_and_lookup_identifier("invalid")
  expect_false(result$success)
  expect_true(grepl(
    "DOI or PMID format not recognised.",
    result$message
  ))
  expect_true(is.na(result$identifier_type))
})

# Integration Tests for Crossref Functions ----
# Tests that require internet connectivity and test actual API calls

test_that("pmid_to_doi works with real PMID", {
  skip_if_offline()

  # Test with a real PMID (this is a well-known paper)
  result <- pmid_to_doi("23193287") # A Nature paper from 2012

  expect_true(result$success)
  expect_true(is.character(result$doi))
  expect_true(validate_doi_format(result$doi))
  expect_true(grepl("successfully converted", result$message))
})

test_that("lookup_crossref_doi works with real DOI", {
  skip_if_offline()

  # Test with a real DOI
  result <- lookup_crossref_doi("10.1038/nature11247") # A Nature paper

  expect_true(result$success)
  expect_true(is.data.frame(result$data))
  expect_true(nrow(result$data) > 0)
  expect_true("title" %in% names(result$data))
  expect_true(grepl("successfully", result$message))
})

test_that("map_crossref_to_reference_fields works with real Crossref data", {
  skip_if_offline()

  # Get real Crossref data
  lookup_result <- lookup_crossref_doi("10.1038/nature11247")
  skip_if(!lookup_result$success, "Crossref lookup failed")

  # Map the data
  mapped_fields <- map_crossref_to_reference_fields(lookup_result$data)

  expect_true(is.list(mapped_fields))
  expect_equal(mapped_fields$REFERENCE_TYPE, "journal")
  expect_true(is.character(mapped_fields$TITLE))
  expect_true(nchar(mapped_fields$TITLE) > 0)
  expect_true(is.character(mapped_fields$DOI))
  expect_equal(mapped_fields$DOI, "10.1038/nature11247")
  expect_true(is.Date(mapped_fields$ACCESS_DATE))
})

test_that("validate_and_lookup_identifier works end-to-end with DOI", {
  skip_if_offline()

  # Test complete workflow with DOI
  result <- validate_and_lookup_identifier("10.1038/nature11247")

  expect_true(result$success)
  expect_equal(result$identifier_type, "doi")
  expect_true(is.list(result$data))
  expect_equal(result$data$REFERENCE_TYPE, "journal")
  expect_true(is.character(result$data$TITLE))
  expect_true(nchar(result$data$TITLE) > 0)
})

test_that("validate_and_lookup_identifier works end-to-end with PMID", {
  skip_if_offline()

  # Test complete workflow with PMID
  result <- validate_and_lookup_identifier("23193287")

  expect_true(result$success)
  expect_equal(result$identifier_type, "pmid")
  expect_true(is.list(result$data))
  expect_equal(result$data$REFERENCE_TYPE, "journal")
  expect_true(is.character(result$data$TITLE))
  expect_true(nchar(result$data$TITLE) > 0)
})

test_that("validate_and_lookup_identifier works with URL-formatted DOI", {
  skip_if_offline()

  # Test with full DOI URL
  result <- validate_and_lookup_identifier(
    "https://doi.org/10.1038/nature11247"
  )

  expect_true(result$success)
  expect_equal(result$identifier_type, "doi")
  expect_true(is.list(result$data))
  expect_equal(result$data$DOI, "10.1038/nature11247")
})

test_that("validate_and_lookup_identifier works with PMID prefix", {
  skip_if_offline()

  # Test with PMID: prefix
  result <- validate_and_lookup_identifier("PMID:23193287")

  expect_true(result$success)
  expect_equal(result$identifier_type, "pmid")
  expect_true(is.list(result$data))
  expect_true(is.character(result$data$TITLE))
})

test_that("validate_and_lookup_identifier works with diverse DOIs", {
  skip_if_offline()

  # Collection of 20 valid DOIs from various publishers and domains
  test_dois <- c(
    "10.1038/nature11247",
    "10.3390/ijms23063346",
    "10.1126/science.1234567",
    "https://doi.org/10.1093/etojnl/vgaf178",
    "10.1016/j.cell.2019.01.001",
    "10.1016/S0140-6736(20)30183-5",
    "10.1002/anie.202409143",
    "doi.org/10.1111/j.1469-8137.1912.tb05611.x",
    "10.1371/journal.pone.0123456",
    "10.1371/journal.pbio.1002345",
    "https://doi.org/10.1103/PhysRevLett.35.1442",
    "10.1002/andp.19053220607",
    "10.1093/bioinformatics/bty123",
    "10.1093/nar/gkz456",
    "https://doi.org/10.1525/aa.1929.31.3.02a00240",
    "10.1080/12345678.2019.1234567",
    "10.1109/TPAMI.2019.1234567",
    "10.1021/jacs.9b01234",
    "10.1038/nprot.2013.143",
    "https://doi.org/10.48550/arXiv.2508.02453"
  )

  # Test each DOI
  results <- list()
  successful_lookups <- 0
  failed_lookups <- character(0)

  for (i in seq_along(test_dois)) {
    doi <- test_dois[i]

    # Add some delay to be respectful to APIs
    if (i > 1) Sys.sleep(0.01) # limit as of 2025 is 50/sec

    result <- validate_and_lookup_identifier(doi)
    results[[i]] <- result

    message(glue("DOI query {i}: {doi}; result: {result$success}"))

    if (result$success) {
      successful_lookups <- successful_lookups + 1

      # Basic checks for successful lookups
      expect_equal(result$identifier_type, "doi")
      expect_true(is.list(result$data))
      expect_true(is.character(result$data$TITLE) || is.na(result$data$TITLE))
      expect_true(is.Date(result$data$ACCESS_DATE))

      # Check that DOI is preserved correctly
      if (!is_empty(result$data$DOI)) {
        expect_equal(
          extract_clean_doi(result$data$DOI) |> tolower(),
          extract_clean_doi(doi) |> tolower()
        )
      }
    } else {
      failed_lookups <- c(failed_lookups, doi)
    }
  }

  # We expect at least 70% success rate (some DOIs might be invalid or not in Crossref)
  success_rate <- successful_lookups / length(test_dois)
  expect_gte(
    success_rate,
    0.7,
    label = paste(
      "Success rate:",
      round(success_rate * 100, 1),
      "%. Failed DOIs:",
      paste(failed_lookups, collapse = ", ")
    )
  )

  # Print summary for debugging
  cat("\nDOI Lookup Test Summary:\n")
  cat(
    "Successful lookups:",
    successful_lookups,
    "/",
    length(test_dois),
    "(",
    round(success_rate * 100, 1),
    "%)\n"
  )
  if (length(failed_lookups) > 0) {
    cat("Failed DOIs:", paste(failed_lookups, collapse = ", "), "\n")
  }
})

test_that("validate_and_lookup_identifier handles different DOI formats consistently", {
  skip_if_offline()

  # Test the same DOI in different formats
  base_doi <- "10.1038/nature11247"
  doi_formats <- c(
    base_doi,
    paste0("https://doi.org/", base_doi),
    paste0("http://dx.doi.org/", base_doi),
    paste0("DOI:", base_doi),
    paste0("doi:", base_doi)
  )

  results <- list()
  for (format in doi_formats) {
    result <- validate_and_lookup_identifier(format)
    results[[format]] <- result

    if (result$success) {
      expect_equal(result$identifier_type, "doi")
      expect_equal(result$data$DOI, base_doi) # Should normalize to clean DOI
    }

    # Small delay between requests
    Sys.sleep(0.2)
  }

  # Check that we got consistent results (allowing for some API failures)
  successful_results <- results[sapply(results, function(x) x$success)]
  if (length(successful_results) > 1) {
    # All successful results should have the same title
    titles <- sapply(successful_results, function(x) x$data$TITLE)
    expect_true(
      all(titles == titles[1]),
      info = "Different DOI formats should return the same publication data"
    )
  }
})
