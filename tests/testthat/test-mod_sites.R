library(shinytest2)

testServer(
  mod_sites_server,
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
  ui <- mod_sites_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_sites_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

describe("{shinytest2} mod_sites bulk addition", {
  app <- AppDriver$new(name = "add50Sites")

  it("successfully adds 50 blank sites to the table", {
    # Navigate to sites module ----
    app$set_inputs(`main-page` = "03-sites")

    app$set_inputs(`sites-num_sites` = "50")
    app$set_inputs(`sites-add_site` = "click")

    # Check that table exists and has 50 rows ----
    sites_table_data <- app$get_value(output = "sites-sites_table")
    expect_true(!is.null(sites_table_data))

    # Verify the table has exactly 50 rows ----
    # Rather crudely for now
    expect_match(sites_table_data, regexp = "SITE_050")
  })
})
