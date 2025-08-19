test_that("module server loads correctly", {
  # Mock both file reading functions
  local_mocked_bindings(
    read_parquet = function(file) {
      if (file == "inst/data/clean/dummy_quality_parameters.parquet") {
        tibble(
          PARAMETER_TYPE = "Quality Parameter",
          PARAMETER_TYPE_SUB = "Not Reported",
          MEASURED_TYPE = "pH",
          PARAMETER_NAME = "pH",
          PARAMETER_NAME_SUB = NA,
          INCHIKEY_SD = NA,
          PUBCHEM_CID = NA,
          CAS_RN = NA
        )
      } else if (
        file == "inst/data/clean/ClassyFire_Taxonomy_2025_02.parquet"
      ) {
        tibble(
          PARAMETER_TYPE = "Stressor",
          PARAMETER_TYPE_SUB = "Homogeneous metal compounds",
          MEASURED_TYPE = "Concentration",
          PARAMETER_NAME = "Copper",
          PARAMETER_NAME_SUB = NA,
          INCHIKEY_SD = NA,
          PUBCHEM_CID = NA,
          CAS_RN = NA
        )
      } else {
        errorCondition(message = "read_parquet didn't recognise mocked file")
      }
    }
  )

  testServer(
    mod_parameters_server,
    args = list(),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))
    }
  )
})


test_that("module ui works", {
  ui <- mod_parameters_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_parameters_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
