library(shinytest2)

test_that("{shinytest2} recording: ENTERED_BY propogates to sites and references module", {
  app <- AppDriver$new(name = "ENTERED_BY_propogates")
  app$set_inputs(`campaign-ENTERED_BY` = "Test McTesterson")
  app$set_inputs(`main-page` = "02-references")
  app$expect_values(input = TRUE)
  app$set_inputs(`main-page` = "03-sites")
  app$set_inputs(`sites-add_site` = "click")
  app$expect_values(output = "sites-sites_table")
})
