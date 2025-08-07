library(shinytest2)

test_that("{shinytest2} add 50 blank sites to the table in mod_sites", {
  app <- AppDriver$new(name = "add50Sites")
  app$set_inputs(`main-page` = "03-sites")

  for (n in 1:5) {
    app$set_inputs(`sites-add_site` = "click")
    # give the server time to catch up
    Sys.sleep(1)
  }

  app$expect_values(output = "sites-sites_table")
})

# Failed to scrub dates so far
#
# scrub_hot_dates <- function(hot) {
#   hot_list <- hot |> rjson::fromJSON()
#   hot_tibble <- hot_list$x$data |> bind_rows() |> mutate(ENTERED_DATE = "")
#   return(hot_tibble)
# }
#
# print_and_return <- function(input) {
#   output <<- input
#   return(input)
# }
#
