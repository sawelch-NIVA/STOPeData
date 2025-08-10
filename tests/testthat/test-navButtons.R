library(shinytest2)

describe("{shinytest2} section navigation buttons functionality", {
  app <- AppDriver$new(name = "navButtons")

  it("has previous section button disabled on first module", {
    # Check initial state ----
    previous_btn_html <- app$get_html(selector = "#previous_section")
    expect_true(grepl("disabled", previous_btn_html))
  })

  it("changes input value when next section button is pressed", {
    # Get initial page value ----
    initial_page <- app$get_value(input = "main-page")

    # Click next section button ----
    app$set_inputs(next_section = "click", wait = TRUE)
    app$wait_for_idle()

    # Check that page value changed ----
    new_page <- app$get_value(input = "main-page")
    expect_false(identical(initial_page, new_page))
  })

  it("disables next section button after pressing it 20 times", {
    # Click next section button 20 times ----
    for (i in 1:20) {
      app$set_inputs(next_section = "click", wait = TRUE)
      app$wait_for_idle()
    }

    # Check that next section button is disabled ----
    next_btn_html <- app$get_html(selector = "#next_section")
    expect_true(grepl("disabled", next_btn_html))
  })
})
