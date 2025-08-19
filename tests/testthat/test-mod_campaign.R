library(shinytest2)

testServer(
  mod_campaign_server,
  # Add here your module params
  args = list(),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
  }
)

test_that("module ui works", {
  ui <- mod_campaign_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_campaign_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})


test_campaign_data <- tibble::tibble(
  CAMPAIGN_NAME = "Campaign",
  CAMPAIGN_START_DATE = as.Date("2025-07-18"),
  CAMPAIGN_END_DATE = as.Date("2025-07-31"),
  RELIABILITY_SCORE = "11",
  RELIABILITY_EVAL_SYS = "CREED",
  CONFIDENTIALITY_EXPIRY_DATE = as.Date("2025-07-23"),
  ORGANISATION = "NIVA",
  ENTERED_BY = "Sam Welch",
  # ENTERED_DATE = as.Date("2025-08-10"),
  CAMPAIGN_COMMENT = NA,
)

test_that("{shinytest2} does mod_campaign correctly validate inputs?", {
  app <- AppDriver$new(name = "STOPeData")
  app$set_inputs(`campaign-CAMPAIGN_NAME` = "Campaign", wait_ = FALSE)
  app$set_inputs(`campaign-CAMPAIGN_START_DATE` = "2025-07-18", wait_ = FALSE)
  app$set_inputs(`campaign-CAMPAIGN_END_DATE` = "2025-07-31", wait_ = FALSE)
  app$set_inputs(`campaign-RELIABILITY_EVAL_SYS` = "CREED", wait_ = FALSE)
  app$set_inputs(`campaign-RELIABILITY_SCORE` = "11", wait_ = FALSE)
  app$set_inputs(
    `campaign-CONFIDENTIALITY_EXPIRY_DATE` = "2025-07-23",
    wait_ = FALSE
  )
  app$set_inputs(`campaign-ORGANISATION` = "NIVA", wait_ = FALSE)
  app$set_inputs(`campaign-ENTERED_BY` = "Sam Welch", wait_ = FALSE)
  data <- app$get_values(
    export = "campaign-module_data"
  )$export$`campaign-module_data` |>
    select(-ENTERED_DATE)
  valid <- app$get_values(
    export = "campaign-module_valid"
  )$export$`campaign-module_valid`
  expect_true(valid)
  expect_equal(data, test_campaign_data)
})
