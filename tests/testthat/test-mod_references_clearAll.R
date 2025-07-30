library(shinytest2)

test_that("{shinytest2} Does mod_references clearAll button work?", {
  app <- AppDriver$new(
    name = "references_clearAll",
    height = 1353,
    width = 2259
  )
  app$set_inputs(`references-AUTHOR` = "Hello!", wait_ = FALSE)
  app$set_inputs(`references-TITLE` = "Hi!", wait_ = FALSE)
  app$set_inputs(`references-ACCESS_DATE` = "2025-07-15")
  app$set_inputs(
    `references-PERIODICAL_JOURNAL` = "Big Book of Test Text",
    wait_ = FALSE
  )
  app$set_inputs(`references-VOLUME` = 1234, wait_ = FALSE)
  app$set_inputs(`references-ISSUE` = 1234, wait_ = FALSE)
  app$click("references-clear")
  app$expect_values()
})
