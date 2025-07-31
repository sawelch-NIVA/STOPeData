library(shinytest2)

test_that("{shinytest2} does mod_campaign correctly validate dummy data?", {
  app <- AppDriver$new(name = "STOPeData", height = 1351, width = 2259)
  app$set_inputs(`campaign-CAMPAIGN_NAME` = "Sam Campaign", wait_ = FALSE)
  app$set_inputs(`campaign-CAMPAIGN_START_DATE` = "2025-07-18", wait_ = FALSE)
  app$set_inputs(`campaign-CAMPAIGN_END_DATE` = "2025-07-31", wait_ = FALSE)
  app$set_inputs(`campaign-RELIABILITY_EVAL_SYS` = "CREED", wait_ = FALSE)
  app$set_inputs(`campaign-RELIABILITY_SCORE` = "11", wait_ = FALSE)
  app$set_inputs(
    `campaign-CONFIDENTIALITY_EXPIRY_DATE` = "2025-07-23",
    wait_ = FALSE
  )
  app$set_inputs(`campaign-ORGANISATION` = "Sam Himself", wait_ = FALSE)
  app$set_inputs(`campaign-ENTERED_BY` = "Sam Welch", wait_ = FALSE)
  app$expect_values(output = TRUE)
})
