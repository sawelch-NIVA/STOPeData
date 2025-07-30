library(shinytest2)

test_that("{shinytest2} does mod_references switch reference types properly, and validate them?", {
  app <- AppDriver$new(
    name = "references_switchType",
    height = 1353,
    width = 2259
  )
  app$expect_values()
  app$set_inputs(`references-AUTHOR` = "A", wait_ = FALSE)
  app$set_inputs(`references-TITLE` = "A", wait_ = FALSE)
  app$set_inputs(`references-ACCESS_DATE` = "2025-07-30", wait_ = FALSE)
  app$set_inputs(`references-PERIODICAL_JOURNAL` = "A", wait_ = FALSE)
  app$set_inputs(`references-VOLUME` = 1, wait_ = FALSE)
  app$set_inputs(`references-ISSUE` = 2, wait_ = FALSE)
  app$expect_values()
  app$set_inputs(`references-REFERENCE_TYPE` = "book", wait_ = FALSE)
  app$set_inputs(`references-PUBLISHER` = "A", wait_ = FALSE)
  app$expect_values()
  app$set_inputs(`references-REFERENCE_TYPE` = "report", wait_ = FALSE)
  app$set_inputs(`references-INSTITUTION` = "A", wait_ = FALSE)
  app$expect_values()
  app$set_inputs(`references-REFERENCE_TYPE` = "dataset", wait_ = FALSE)
  app$set_inputs(`references-DB_NAME` = "A", wait_ = FALSE)
  app$set_inputs(`references-DB_PROVIDER` = "A", wait_ = FALSE)
  app$expect_values()
})
