# # Parameters Module Test ----
# # Extended test script for mod_parameters_server functionality
#
# # Setup ----
# library(testthat)
# library(tidyverse)
# library(arrow)
# library(constructive)
# library(rjson)
# library(shinyvalidate)
# # devtools::load_all()
#
# # Test data preparation ----
# chemical_parameters <- read_parquet(
#   file = "inst/data/clean/ecotox_2025_06_12_chemicals.parquet"
# ) |>
#   mutate(
#     PARAMETER_TYPE = "Stressor",
#     PARAMETER_TYPE_SUB = "Not reported",
#     MEASURED_TYPE = "Concentration",
#     PUBCHEM_CID = NA,
#     INCHIKEY_SD = NA
#   ) |>
#   arrange(PARAMETER_NAME) |>
#   tibble()
#
# # Expected test parameter data ----
# parameter_data <- tibble::tibble(
#   PARAMETER_NAME = c(
#     "1,2-Dibromo-4-(4-bromophenoxy)benzene",
#     "(11beta,16alpha)-21-(3,3-Dimethyl-1-oxobutoxy)-9-fluoro-11,17-dihydroxy-16-methylpregna-1,4-diene-3,20-dione",
#     "3-(2-Methoxyphenyl)-N,N,alpha,alpha,4-pentamethyl-1H-pyrazole-1-acetamide",
#     "2,5-Dibromophenol",
#     "2-Ethoxy-2,3-dihydro-3,3-dimethyl-5-benzofuranol 5-methanesulfonate"
#   ),
#   CAS_RN = c("147217-81-0", "24668-75-5", "63687-72-9", "28165-52-8", "26225-79-6"),
#   PARAMETER_TYPE = "Stressor",
#   PARAMETER_TYPE_SUB = "Not reported",
#   MEASURED_TYPE = "Concentration",
#   PUBCHEM_CID = NA,
#   INCHIKEY_SD = NA,
#   PARAMETER_NAME_SUB = ""
# )
#
# # Helper functions ----
# ## Extract data from rhandsontable JSON output ----
# hot_table_data_extract <- function(hot_json) {
#   stopifnot(is.character(hot_json), attributes(hot_json)$class == "json")
#   hot_list <- fromJSON(hot_json)
#   hot_list_data <- hot_list$x$data |> bind_rows()
#   return(hot_list_data)
# }
#
# ## Add single parameter helper ----
# add_parameter <- function(session, param_type, param_name, wait_time = TRUE) {
#   session$setInputs(
#     parameter_type_select = param_type,
#     parameter_name_select = param_name,
#     wait_ = wait_time
#   )
#   session$setInputs(add_existing = "click", wait_ = wait_time)
# }
#
# # Tests ----
# describe("mod_parameters_server", {
#
#   ## Test: Add all 5 parameters sequentially ----
#   it("should successfully add all 5 test parameters", {
#
#     # Arrange: Initialize test server
#     shiny::testServer(app = mod_parameters_server, {
#
#       # Act: Add each parameter from parameter_data
#       for (i in 1:nrow(parameter_data)) {
#         param_type <- parameter_data[[i, "PARAMETER_TYPE"]]
#         param_name <- parameter_data[[i, "PARAMETER_NAME"]]
#
#         # Add parameter
#         add_parameter(session, param_type, param_name)
#
#         # Verify inputs were set correctly
#         expect_identical(input$parameter_type_select, param_type)
#         expect_identical(input$parameter_name_select, param_name)
#       }
#
#       # Extract final table data
#       final_table_json <<- output$parameters_table
#       final_validation_status <<- moduleState$is_valid
#       final_parameter_count <<- nrow(moduleState$parameters_data)
#
#     })
#
#     # Assert: Verify final state
#     expect_true(final_validation_status)
#     expect_equal(final_parameter_count, 5)
#
#     # Extract and verify table contents
#     extracted_data <- hot_table_data_extract(final_table_json)
#     expect_equal(nrow(extracted_data), 5)
#
#     # Check that all parameter names are present
#     expect_true(all(parameter_data$PARAMETER_NAME %in% extracted_data$PARAMETER_NAME))
#
#     # Verify required fields are populated
#     expect_true(all(!is.na(extracted_data$PARAMETER_TYPE)))
#     expect_true(all(!is.na(extracted_data$PARAMETER_NAME)))
#     expect_true(all(!is.na(extracted_data$MEASURED_TYPE)))
#   })
#
#   ## Test: Add duplicate parameter (should fail) ----
#   it("should prevent adding duplicate parameters", {
#
#     shiny::testServer(app = mod_parameters_server, {
#
#       # Add first parameter
#       param_type <- parameter_data[[1, "PARAMETER_TYPE"]]
#       param_name <- parameter_data[[1, "PARAMETER_NAME"]]
#       add_parameter(session, param_type, param_name)
#
#       initial_count <- nrow(moduleState$parameters_data)
#
#       # Attempt to add same parameter again
#       add_parameter(session, param_type, param_name)
#
#       duplicate_attempt_count <<- nrow(moduleState$parameters_data)
#     })
#
#     # Should still have only 1 parameter
#     expect_equal(duplicate_attempt_count, initial_count)
#   })
#
#   ## Test: Module validation states ----
#   it("should properly validate parameter data", {
#
#     shiny::testServer(app = mod_parameters_server, {
#
#       # Initially should be invalid (empty)
#       expect_false(moduleState$is_valid)
#       expect_null(moduleState$validated_data)
#
#       # Add one valid parameter
#       add_parameter(
#         session,
#         parameter_data[[1, "PARAMETER_TYPE"]],
#         parameter_data[[1, "PARAMETER_NAME"]]
#       )
#
#       # Should now be valid
#       expect_true(moduleState$is_valid)
#       expect_false(is.null(moduleState$validated_data))
#       expect_equal(nrow(moduleState$validated_data), 1)
#
#       validation_output <<- output$validation_reporter
#     })
#
#     # Check validation UI output contains success message
#     expect_match(validation_output, "validated successfully")
#   })
#
#   ## Test: New parameter creation ----
#   it("should allow creation of new blank parameters", {
#
#     shiny::testServer(app = mod_parameters_server, {
#
#       # Select parameter type and add new blank parameter
#       session$setInputs(parameter_type_select = "Stressor", wait_ = TRUE)
#       session$setInputs(add_new = "click", wait_ = TRUE)
#
#       new_param_count <<- nrow(moduleState$parameters_data)
#       new_param_data <<- moduleState$parameters_data
#     })
#
#     # Should have 1 new blank parameter
#     expect_equal(new_param_count, 1)
#     expect_equal(new_param_data$PARAMETER_TYPE[1], "Stressor")
#     expect_true(is.na(new_param_data$PARAMETER_NAME[1]) || new_param_data$PARAMETER_NAME[1] == "")
#   })
# })
#
# # Optional: Interactive test runner ----
# if (interactive()) {
#   cat("Running parameters module tests...\n")
#   test_results <- test_file("path/to/this/test/file.R")
#   print(test_results)
# }
