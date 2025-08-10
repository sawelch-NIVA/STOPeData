describe("parameter_units() loads expected data", {
  withr::local_dir(new = "../") # set wd to root
  it(description = "loads a dataframe with the right four columns", {
    units <- parameter_units()
    testthat::expect_equal(
      object = names(units),
      expected = c(
        "MEASURED_UNIT",
        "BASE_SI_UNIT",
        "CONVERSION_FACTOR",
        "UNIT_COMMENTS"
      )
    )
  })
  it(description = "selected by named column", {
    units_selected <- parameter_units("BASE_SI_UNIT")

    testthat::expect_vector(
      object = units_selected,
      ptype = character()
    )
  })
})
