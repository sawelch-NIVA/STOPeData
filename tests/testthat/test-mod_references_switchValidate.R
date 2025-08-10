library(shinytest2)

journal_test_data <- tibble::tibble(
  REFERENCE_TYPE = "journal",
  AUTHOR = "A",
  TITLE = "A",
  YEAR = 2025L,
  # ACCESS_DATE = as.Date("2025-07-30"),
  PERIODICAL_JOURNAL = "A",
  VOLUME = 1L,
  ISSUE = 2L,
  PUBLISHER = NA,
  INSTITUTION = NA,
  DB_NAME = NA,
  DB_PROVIDER = NA,
  DOI = NA,
  URL = NA,
  PAGES = NA,
  ISBN_ISSN = NA,
  EDITION = NA,
  PUBLISHED_PLACE = NA,
  DOCUMENT_NUMBER = NA,
  ACCESSION_NUMBER = NA,
  PMCID = NA,
  SERIES_TITLE = NA,
  SERIES_EDITOR = NA,
  SERIES_VOLUME = NA,
  NUMBER_OF_PAGES = NA,
  NUMBER_OF_VOLUMES = NA,
  REF_COMMENT = NA,
)

book_test_data <- tibble::tibble(
  REFERENCE_TYPE = "book",
  AUTHOR = "A",
  TITLE = "A",
  YEAR = 2025L,
  # ACCESS_DATE = as.Date("2025-07-30"),
  PERIODICAL_JOURNAL = NA,
  VOLUME = NA,
  ISSUE = NA,
  PUBLISHER = "A",
  INSTITUTION = NA,
  DB_NAME = NA,
  DB_PROVIDER = NA,
  DOI = NA,
  URL = NA,
  PAGES = NA,
  ISBN_ISSN = NA,
  EDITION = NA,
  PUBLISHED_PLACE = NA,
  DOCUMENT_NUMBER = NA,
  ACCESSION_NUMBER = NA,
  PMCID = NA,
  SERIES_TITLE = NA,
  SERIES_EDITOR = NA,
  SERIES_VOLUME = NA,
  NUMBER_OF_PAGES = NA,
  NUMBER_OF_VOLUMES = NA,
  REF_COMMENT = NA,
)

dataset_test_data <- tibble::tibble(
  REFERENCE_TYPE = "dataset",
  AUTHOR = "A",
  TITLE = "A",
  YEAR = 2025L,
  # ACCESS_DATE = as.Date("2025-07-30"),
  PERIODICAL_JOURNAL = NA,
  VOLUME = NA,
  ISSUE = NA,
  PUBLISHER = NA,
  INSTITUTION = NA,
  DB_NAME = "A",
  DB_PROVIDER = "A",
  DOI = NA,
  URL = NA,
  PAGES = NA,
  ISBN_ISSN = NA,
  EDITION = NA,
  PUBLISHED_PLACE = NA,
  DOCUMENT_NUMBER = NA,
  ACCESSION_NUMBER = NA,
  PMCID = NA,
  SERIES_TITLE = NA,
  SERIES_EDITOR = NA,
  SERIES_VOLUME = NA,
  NUMBER_OF_PAGES = NA,
  NUMBER_OF_VOLUMES = NA,
  REF_COMMENT = NA,
)

report_test_data <- tibble::tibble(
  REFERENCE_TYPE = "report",
  AUTHOR = "A",
  TITLE = "A",
  YEAR = 2025L,
  PERIODICAL_JOURNAL = NA,
  VOLUME = NA,
  ISSUE = NA,
  PUBLISHER = "A",
  INSTITUTION = "A",
  DB_NAME = NA,
  DB_PROVIDER = NA,
  DOI = NA,
  URL = NA,
  PAGES = NA,
  ISBN_ISSN = NA,
  EDITION = NA,
  PUBLISHED_PLACE = NA,
  DOCUMENT_NUMBER = NA,
  ACCESSION_NUMBER = NA,
  PMCID = NA,
  SERIES_TITLE = NA,
  SERIES_EDITOR = NA,
  SERIES_VOLUME = NA,
  NUMBER_OF_PAGES = NA,
  NUMBER_OF_VOLUMES = NA,
  REF_COMMENT = NA,
)

describe("{shinytest2} mod_references", {
  app <- AppDriver$new(
    name = "references_switchValidate"
  )
  it("returns valid = FALSE on initiation", {
    app$set_inputs(`main-page` = "02-references")
    valid <- app$get_value(export = "references-module_valid")
    expect_false(valid)
  })
  it("returns valid = TRUE and valid data when journal required fields are filled", {
    app$set_inputs(`references-AUTHOR` = "A", wait_ = FALSE)
    app$set_inputs(`references-TITLE` = "A", wait_ = FALSE)
    app$set_inputs(`references-ACCESS_DATE` = "2025-07-30", wait_ = FALSE)
    app$set_inputs(`references-PERIODICAL_JOURNAL` = "A", wait_ = FALSE)
    app$set_inputs(`references-VOLUME` = 1, wait_ = FALSE)
    app$set_inputs(`references-ISSUE` = 2, wait_ = FALSE)
    app$set_inputs(`references-ENTERED_BY` = "Ole Nordman", wait_ = FALSE)
    data <- app$get_value(export = "references-module_data") |>
      dplyr::select(-ACCESS_DATE)
    valid <- app$get_value(export = "references-module_valid")
    expect_true(valid)
    expect_equal(data, journal_test_data)
  })
  it("returns valid = TRUE and valid data when book required fields are filled", {
    app$set_inputs(`references-REFERENCE_TYPE` = "book", wait_ = FALSE)
    app$set_inputs(`references-PUBLISHER` = "A", wait_ = FALSE)
    data <- app$get_value(export = "references-module_data") |>
      dplyr::select(-ACCESS_DATE)
    valid <- app$get_value(export = "references-module_valid")
    expect_true(valid)
    expect_equal(data, book_test_data)
  })
  it("returns valid = TRUE and valid data when report required fields are filled", {
    app$set_inputs(`references-REFERENCE_TYPE` = "report", wait_ = FALSE)
    app$set_inputs(`references-INSTITUTION` = "A", wait_ = FALSE)
    data <- app$get_value(export = "references-module_data") |>
      dplyr::select(-ACCESS_DATE)
    valid <- app$get_value(export = "references-module_valid")
    expect_true(valid)
    expect_equal(data, report_test_data)
  })
  it("returns valid = TRUE and valid data when dataset required fields are filled", {
    app$set_inputs(`references-REFERENCE_TYPE` = "dataset", wait_ = FALSE)
    app$set_inputs(`references-DB_NAME` = "A", wait_ = FALSE)
    app$set_inputs(`references-DB_PROVIDER` = "A", wait_ = FALSE)
    data <- app$get_value(export = "references-module_data") |>
      dplyr::select(-ACCESS_DATE)
    valid <- app$get_value(export = "references-module_valid")
    expect_true(valid)
    expect_equal(data, dataset_test_data)
  })
})
