
test_that("module server loads correctly", {
 # Mock both file reading functions
  local_mocked_bindings(
    read_parquet = function(file) {
      if (file == "inst/data/clean/dummy_quality_parameters.parquet") {
        tibble(
          PARAMETER_TYPE = "Stressor",
          PARAMETER_TYPE_SUB = "Metal",
          MEASURED_TYPE = "Concentration",
          PARAMETER_NAME = "Copper",
          PARAMETER_NAME_SUB = NA,
          INCHIKEY_SD = NA,
          PUBCHEM_CID = NA,
          CAS_RN = NA
        )
      } else if (
        file == "inst/data/clean/ecotox_2025_06_12_chemicals.parquet"
      ) {
        tibble(
          PARAMETER_NAME = "Poisonium",
          CAS_RN = 12345
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



# Test ----
# describe("server", {
#   it("should add 5 stressor parameters using inputs and add_existing button", {
#
#     # Arrange
#     parameter_data <- tibble::tibble(
#       PARAMETER_NAME = c(
#         "1,2-Dibromo-4-(4-bromophenoxy)benzene",
#         "(11beta,16alpha)-21-(3,3-Dimethyl-1-oxobutoxy)-9-fluoro-11,17-dihydroxy-16-methylpregna-1,4-diene-3,20-dione",
#         "3-(2-Methoxyphenyl)-N,N,alpha,alpha,4-pentamethyl-1H-pyrazole-1-acetamide",
#         "2,5-Dibromophenol",
#         "2-Ethoxy-2,3-dihydro-3,3-dimethyl-5-benzofuranol 5-methanesulfonate"
#       ),
#       CAS_RN = c("147217-81-0", "24668-75-5", "63687-72-9", "28165-52-8", "26225-79-6"),
#       PARAMETER_TYPE = "Stressor",
#       PARAMETER_TYPE_SUB = "Not reported",
#       MEASURED_TYPE = "Concentration",
#       PUBCHEM_CID = NA,
#       INCHIKEY_SD = NA
#     )
#
#     local_mocked_bindings(
#       read_parquet = function(file) {
#         if (file == "inst/data/clean/dummy_quality_parameters.parquet") {
#           tibble(
#             PARAMETER_TYPE = "Stressor",
#             PARAMETER_TYPE_SUB = "Metal",
#             MEASURED_TYPE = "Concentration",
#             PARAMETER_NAME = "Copper",
#             PARAMETER_NAME_SUB = NA,
#             INCHIKEY_SD = NA,
#             PUBCHEM_CID = NA,
#             CAS_RN = NA
#           )
#         } else if (
#           file == "inst/data/clean/ecotox_2025_06_12_chemicals.parquet"
#         ) {
#           tibble(
#             PARAMETER_NAME = "Poisonium",
#             CAS_RN = 12345
#           )
#         } else {
#           errorCondition(message = "read_parquet didn't recognise mocked file")
#         }
#       }
#     )
#
#     # Act
#     testServer(app = mod_parameters_server, {
#
#       # Add each parameter from parameter_data
#       for (i in 1:nrow(parameter_data)) {
#         param_type <- parameter_data[[i, "PARAMETER_TYPE"]]
#         param_name <- parameter_data[[i, "PARAMETER_NAME"]]
#
#         # Set inputs and add existing parameter
#         session$setInputs(
#           parameter_type_select = param_type,
#           parameter_name_select = param_name,
#           wait_ = TRUE
#         )
#         expect_identical(input$parameter_type_select, param_type)
#         expect_identical(input$parameter_name_select, param_name)
#
#         # Add existing parameter
#         session$setInputs(add_existing = "click", wait_ = TRUE)
#       }
#
#       # Capture final outputs
#       final_table <<- output$parameters_table
#       final_reactive <<- moduleState$parameters_data
#       final_parameter_count <<- nrow(moduleState$parameters_data)
#
#     })
#
#     # Assert
#     expect_equal(final_parameter_count, 5)
#
#     # Extract and verify table data
#     extracted_data <- hot_table_data_extract(final_table)
#     expect_equal(nrow(extracted_data), 5)
#     expect_true(all(parameter_data$PARAMETER_NAME %in% extracted_data$PARAMETER_NAME))
#
#   })
# })

    parameter_data <- tibble::tibble(
      PARAMETER_NAME = c(
        "1,2-Dibromo-4-(4-bromophenoxy)benzene",
        "(11beta,16alpha)-21-(3,3-Dimethyl-1-oxobutoxy)-9-fluoro-11,17-dihydroxy-16-methylpregna-1,4-diene-3,20-dione",
        "3-(2-Methoxyphenyl)-N,N,alpha,alpha,4-pentamethyl-1H-pyrazole-1-acetamide",
        "2,5-Dibromophenol",
        "2-Ethoxy-2,3-dihydro-3,3-dimethyl-5-benzofuranol 5-methanesulfonate"
      ),
      CAS_RN = c("147217-81-0", "24668-75-5", "63687-72-9", "28165-52-8", "26225-79-6"),
      PARAMETER_TYPE = "Stressor",
      PARAMETER_TYPE_SUB = "Not reported",
      MEASURED_TYPE = "Concentration",
      PUBCHEM_CID = NA,
      INCHIKEY_SD = NA
    )

local_mocked_bindings(
  read_parquet = function(file) {
    if (file == "inst/data/clean/dummy_quality_parameters.parquet") {
      tibble(
        PARAMETER_TYPE = "Stressor",
        PARAMETER_TYPE_SUB = "Metal",
        MEASURED_TYPE = "Concentration",
        PARAMETER_NAME = "Copper",
        PARAMETER_NAME_SUB = NA,
        INCHIKEY_SD = NA,
        PUBCHEM_CID = NA,
        CAS_RN = NA
      )
    } else if (
      file == "inst/data/clean/ecotox_2025_06_12_chemicals.parquet"
    ) {
      tibble(
        PARAMETER_NAME = "Poisonium",
        CAS_RN = 12345
      )
    } else {
      errorCondition(message = "read_parquet didn't recognise mocked file")
    }
  }
)
# Neither of these work properly, even though I was able to get them working before... ?

# describe("server", {
#   it("should subset the data with selected variables", {
#     # Arrange
#     # Act
#     shiny::testServer(app = mod_parameters_server, {
#       for (i in 1:nrow(parameter_data)) {
#         session$setInputs(parameter_type_select = parameter_data[[i,3]], parameter_name_select = parameter_data[[i,1]] ,wait = TRUE)
#         # add existing stressor
#         session$setInputs(addexisting = "click", wait = TRUE)
#         expect_identical(input$parameter_type_select, parameter_data[[i,3]])
#         expect_identical(input$parameter_name_select, parameter_data[[i,1]])
#         Sys.sleep(1)
#       }
#
#       # check outputs
#       print(output$parameters_table)
#       temp_table <<- output$parameters_table
#     })
#     # Assert
#   })
# })

# describe("server", {
#   it("should subset the data with selected variables", {
#     # Arrange
#     # Act
#     shiny::testServer(app = mod_parameters_server, {
#       # stressor 1
#       session$setInputs(parameter_type_select = parameter_data[[1,3]], parameter_name_select = parameter_data[[1,1]] ,wait = TRUE)
#       expect_identical(input$parameter_type_select, parameter_data[[1,3]])
#       expect_identical(input$parameter_name_select, parameter_data[[1,1]])
#       # add existing stressor
#       session$setInputs(addexisting = "click", wait = TRUE)
#       # stressor 2
#       session$setInputs(parameter_type_select = parameter_data[[2,3]], parameter_name_select = parameter_data[[2,1]] ,wait = TRUE)
#       expect_identical(input$parameter_type_select, parameter_data[[2,3]])
#       expect_identical(input$parameter_name_select, parameter_data[[2,1]])
#       # add existing stressor
#       session$setInputs(addexisting = "click", wait = TRUE)
#       # check outputs
#       print(output$parameters_table)
#       temp_table <<- output$parameters_table
#       print(moduleState$parameters_data)
#     })
#     # Assert
#   })
# })
