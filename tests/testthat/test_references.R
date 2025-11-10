test_that("generate_reference_id() correctly generates a reference ID", {
  input <- list(
    date = 1997,
    author = "Testname, T.",
    title = "Some Initial Observations on Felid-Canid Social Dynamics"
  )

  result <- generate_reference_id(
    date = input$date,
    author = input$author,
    title = input$title
  )

  expect_equal(result, "1997TestnameSomeInitialObservations")
})
