library(shinytest2)

test_that("{shinytest2} add 50 blank sites to the table in mod_sites", {
  app <- AppDriver$new(name = "add50Sites")
  app$set_inputs(`main-page` = "03-sites")

  for (n in 1:50) {
    app$set_inputs(`sites-add_site` = "click")
    # give the server time to catch up
    Sys.sleep(1)
  }

  app$expect_values(output = "sites-sites_table")
})
