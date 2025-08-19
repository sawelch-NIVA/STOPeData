library(shinytest2)

describe("{shinytest2} ENTERED_BY field propagation", {
  app <- AppDriver$new(name = "ENTERED_BY_propagates")

  it("propagates ENTERED_BY from campaign to references module", {
    # Set ENTERED_BY in campaign module ----
    app$set_inputs(`campaign-ENTERED_BY` = "Test McTesterson")

    # Navigate to references module ----
    app$set_inputs(`main-page` = "02-references")

    # Check that ENTERED_BY value is propagated ----
    references_entered_by <- app$get_value(input = "references-ENTERED_BY")
    expect_equal(references_entered_by, "Test McTesterson")
  })

  it("propagates ENTERED_BY from campaign to sites module", {
    # Navigate to sites module ----
    app$set_inputs(`main-page` = "03-sites")

    # Add a new site ----
    app$set_inputs(`sites-add_site` = "click")
    app$wait_for_idle()

    # Check that ENTERED_BY appears in the sites table ----
    sites_table_data <- app$get_value(output = "sites-sites_table")

    # Verify ENTERED_BY is present in the table data ----
    # (This assumes the table contains the ENTERED_BY field)
    expect_true(any(grepl(
      "Test McTesterson",
      sites_table_data,
      ignore.case = TRUE
    )))
  })
})
