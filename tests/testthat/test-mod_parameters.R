test_that("module server loads correctly", {
  # Mock both file reading functions
  local_mocked_bindings(
    read_parquet = function(...) {
      tibble(
        PARAMETER_NAME = "Poisonium",
        CAS_RN = 12345
      )
    }
      )

      local_mocked_bindings(
        read.csv = function(...) {
          data.frame(
            PARAMETER_TYPE = "Stressor",
            PARAMETER_TYPE_SUB = "Metal",
            MEASURED_TYPE = "Concentration",
            PARAMETER_NAME = "Copper",
            PARAMETER_NAME_SUB = NA,
            INCHIKEY_SD = NA,
            PUBCHEM_CID = NA,
            CAS_RN = NA)
        }, .package = "utils"
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
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
