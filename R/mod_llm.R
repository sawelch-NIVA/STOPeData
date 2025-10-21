# LLM Extraction Module ----
# A Shiny module for PDF upload and automated data extraction using Claude

#' LLM Extraction UI Function ----
#'
#' @description A shiny Module for PDF upload and Claude-powered data extraction.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput textInput actionButton downloadButton downloadHandler
#' @importFrom bslib card card_body accordion accordion_panel tooltip layout_column_wrap input_task_button accordion_panel_open
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs disabled
#' @export
mod_llm_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(
      ".btn-file {
    padding: 6px 20px !important;
}"
    ))),
    # Enable shinyjs
    useShinyjs(),

    # Main extraction card ----
    card(
      card_body(
        ## Info accordion ----
        info_accordion(
          content_file = "inst/app/www/md/intro_llm.md"
        ),

        ## Upload and API section ----
        layout_column_wrap(
          width = "400px",
          fill = FALSE,
          fillable = FALSE,

          ### PDF upload ----
          div(
            style = "display: flex; flex-direction: column;",
            fileInput(
              inputId = ns("pdf_file"),
              label = tooltip(
                list("Upload PDF", bs_icon("info-circle-fill")),
                "Upload a research paper or report (pdf) containing environmental exposure data"
              ),
              accept = ".pdf",
              width = "100%",
              buttonLabel = "Browse...",
            ),
            a(
              "Example PDF URL",
              href = "https://link.springer.com/content/pdf/10.1007/s10646-024-02740-4.pdf",
              target = "_blank",
              class = "text-muted",
              style = "font-size: 0.8rem; margin-top: -20px;"
            )
          ),

          ### API key input ----
          passwordInput(
            inputId = ns("api_key"),
            label = tooltip(
              list("Anthropic API Key", bs_icon("info-circle-fill")),
              "Your Anthropic API key for Claude access. Set ANTHROPIC_API_KEY environment variable to avoid entering this each time."
            ),
            value = Sys.getenv("ANTHROPIC_API_KEY", unset = ""),
            placeholder = "sk-ant-...",
            width = "100%"
          ),
          ### ENTERED_BY
          textInput(
            inputId = ns("ENTERED_BY"),
            label = tooltip(
              list("Entered By", bs_icon("info-circle-fill")),
              "Name/contact details."
            ),
            value = Sys.getenv("EDATA_USERNAME", unset = ""),
            placeholder = "Ole Nordman",
            width = "100%"
          ),
        ),

        ## Prompt and Schema Configuration ----
        accordion(
          id = ns("config_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Modify Prompt and Data Structure (Advanced)",
            icon = bs_icon("gear"),
            div(
              textAreaInput(
                inputId = ns("system_prompt"),
                label = "Extraction Instructions",
                value = create_extraction_prompt(),
                rows = 8,
                width = "100%"
              ),

              textAreaInput(
                inputId = ns("extraction_schema_display"),
                label = "Schema Definition",
                value = get_schema_display(),
                rows = 12,
                width = "100%"
              ),

              div(
                style = "margin-top: 10px;",
                actionButton(
                  ns("reset_defaults"),
                  "Reset to Defaults",
                  class = "btn-secondary btn-sm"
                )
              )
            )
          )
        ),

        ## Extract buttons ----
        layout_columns(
          fill = FALSE,
          div(
            style = "display: flex; flex-direction: column;",
            input_task_button(
              id = ns("extract_data"),
              label = HTML(paste(
                bsicons::bs_icon("cpu"),
                "Extract Data from PDF"
              )),
              class = "btn-info"
            ) |>
              disabled(),
            span(
              "Per extraction: ~$0.10, 30 seconds",
              class = "text-muted",
              style = "font-size: 0.8rem;"
            )
          ),

          div(
            style = "display: flex; flex-direction: column;",
            input_task_button(
              id = ns("load_dummy_data"),
              label = "Load Dummy Data",
              icon = icon("flask"),
              class = "btn-info"
            ),
            span(
              "For testing/demonstration purposes.",
              class = "text-muted",
              style = "font-size: 0.8rem;"
            )
          )
        ),

        ## Status and results ----
        div(
          uiOutput(ns("extraction_status"))
        ),

        ## Extraction results accordion ----
        accordion(
          id = ns("results_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Extraction Results",
            value = "extraction_results",
            icon = bs_icon("cpu"),
            div(
              verbatimTextOutput(ns("extraction_results")),
              # Add download button ----
              div(
                style = "margin-top: 10px;",
                downloadButton(
                  outputId = ns("download_extraction"),
                  label = "Download Results",
                  class = "btn-secondary btn-sm",
                  icon = icon("download")
                ) |>
                  disabled()
              )
            )
          )
        ),

        ## Action buttons for extracted data  ----
        layout_columns(
          fill = FALSE,
          input_task_button(
            id = ns("populate_forms"),
            label = "Populate Modules",
            icon = icon("download"),
            class = "btn-primary"
          ) |>
            disabled(),

          input_task_button(
            id = ns("clear_extraction"),
            label = "Clear Extraction",
            icon = icon("trash"),
            class = "btn-danger"
          ) |>
            disabled()
        )
      )
    )
  )
}

#' LLM Extraction Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateTextAreaInput
#' @importFrom shinyjs enable disable
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom ellmer chat_anthropic params content_pdf_file type_object type_string type_integer type_number type_array
#' @importFrom utils str
#' @export
mod_llm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      extraction_complete = FALSE,
      extraction_successful = FALSE,
      raw_extraction = NULL,
      structured_data = NULL,
      error_message = NULL
    )

    # 2. Observers and Reactives ----

    ## observe: Reset configuration to defaults ----
    observe({
      updateTextAreaInput(
        session,
        "system_prompt",
        value = create_extraction_prompt()
      )
      updateTextAreaInput(
        session,
        "extraction_schema_display",
        value = get_schema_display()
      )
      showNotification("Configuration reset to defaults", type = "message")
    }) |>
      bindEvent(input$reset_defaults)

    ## observe: Enable extract button when PDF and API key available ----
    # upstream: input$pdf_file, input$api_key
    # downstream: extract_data button state
    observe({
      if (
        !is.null(input$pdf_file) &&
          isTruthy(input$api_key) &&
          nchar(input$api_key) > 10
      ) {
        enable("extract_data")
      } else {
        disable("extract_data")
      }
    })

    ## observe: Load dummy data ----
    # upstream: user clicks input$load_dummy_data
    # downstream: moduleState$*, session$userData$reactiveValues$*DataLLM
    observe({
      # Create dummy data structure using external function (lowercase for LLM)
      dummy_data <- create_dummy_data(uppercase_columns = FALSE)

      # Store results in module state (for LLM-specific behavior)
      moduleState$extraction_complete <- TRUE
      moduleState$extraction_successful <- TRUE
      moduleState$structured_data <- dummy_data
      moduleState$raw_extraction <- dummy_data
      moduleState$error_message <- NULL

      # ! I believe this is redundant due to the Populate Forms
      # ! observer. Disabling to check.
      # # Store in session data with LLM suffix (for LLM workflow)
      # store_llm_data_in_session(session, dummy_data)

      showNotification(
        "Dummy data loaded successfully!",
        type = "default"
      )

      # Enable form population button (LLM-specific workflow)
      enable("populate_forms")
      enable("clear_extraction")
    }) |>
      bindEvent(input$load_dummy_data)

    ## observe: PDF data extraction ----
    # upstream: user clicks input$extract_data
    # downstream: moduleState$*, session$userData$reactiveValues$*DataLLM
    observe({
      req(input$pdf_file, input$api_key)

      withProgress(message = "Processing PDF extraction", value = 0, {
        # Step 1: Validate API key
        incProgress(0.1, detail = "Validating API key...")
        if (!grepl("^sk-ant-", input$api_key)) {
          showNotification(
            "API key should start with 'sk-ant-'. Please check your key.",
            type = "warning"
          )
          return()
        }

        tryCatch(
          {
            # Step 2: Set up environment and test connection
            incProgress(0.01, detail = "Opening API connection...")
            Sys.setenv(ANTHROPIC_API_KEY = input$api_key)

            # Step 3: Test API connectivity
            incProgress(0.02, detail = "Testing API connection...")
            test_chat <- NULL
            tryCatch(
              {
                test_chat <- chat_anthropic(
                  model = "claude-sonnet-4-20250514",
                  params = params(max_tokens = 50)
                )
                test_response <- test_chat$chat(
                  "Hello, please respond with 'API connection successful'"
                )
                print_dev("API test successful")
              },
              error = function(e) {
                showNotification(
                  paste("API connection failed:", e$message),
                  type = "error"
                )
                return()
              }
            )

            if (is.null(test_chat)) {
              return()
            }

            # Step 4: Prepare extraction components
            incProgress(0.03, detail = "Preparing extraction...")
            chat <- chat_anthropic(
              model = "claude-sonnet-4-20250514",
              params = params(max_tokens = 4000)
            )

            extraction_schema <- create_extraction_schema()
            pdf_content <- content_pdf_file(input$pdf_file$datapath)

            # Step 5: Set up prompts
            incProgress(0.04, detail = "Configuring extraction...")
            system_prompt <- if (isTruthy(input$system_prompt)) {
              input$system_prompt
            } else {
              create_extraction_prompt()
            }

            # Step 6: Extract data (this is the longest step)
            incProgress(0.05, detail = "Extracting data...")
            result <- chat$chat_structured(
              system_prompt,
              pdf_content,
              type = extraction_schema
            )

            # Step 7: Get API call metadata
            incProgress(0.8, detail = "Storing results...")

            # Capture cost information
            api_metadata <- NULL
            tryCatch(
              {
                cost_info <- chat$get_cost(include = "all")
                api_metadata <- list(
                  total_cost = cost_info$total_cost,
                  total_input_tokens = cost_info$total_input_tokens,
                  total_output_tokens = cost_info$total_output_tokens,
                  call_count = nrow(cost_info$calls)
                )
              },
              error = function(e) {
                print_dev(paste("Could not retrieve cost info:", e$message))
              }
            )

            # Store results
            moduleState$extraction_complete <- TRUE
            moduleState$extraction_successful <- TRUE
            moduleState$structured_data <- result
            moduleState$raw_extraction <- result
            moduleState$error_message <- NULL
            moduleState$api_metadata <- api_metadata

            # Step 8: Update session data
            incProgress(0.9, detail = "Updating data...")

            # Step 9: Enable UI elements
            incProgress(1.0, detail = "Finalising...")
            enable("populate_forms")
            enable("clear_extraction")

            # Final success notification
            showNotification(
              "PDF extraction completed successfully!",
              type = "message"
            )

            # Open extraction accordion for review
            accordion_panel_open(
              id = "results_accordion",
              values = "extraction_results"
            )
          },
          error = function(e) {
            moduleState$extraction_complete <- TRUE
            moduleState$extraction_successful <- FALSE
            moduleState$error_message <- e$message
            moduleState$structured_data <- NULL
            moduleState$api_metadata <- NULL

            showNotification(
              paste("Extraction failed:", e$message),
              type = "error"
            )
          }
        )
      })
    }) |>
      bindEvent(input$extract_data)

    ## observe: Enable download button when extraction is complete ----
    # upstream: moduleState$extraction_complete, moduleState$extraction_successful
    # downstream: download_extraction button state
    observe({
      if (
        moduleState$extraction_complete && moduleState$extraction_successful
      ) {
        enable("download_extraction")
      } else {
        disable("download_extraction")
      }
    }) |>
      bindEvent(
        moduleState$extraction_complete,
        moduleState$extraction_successful
      )

    ## observe: Populate forms with extracted data ----
    # upstream: user clicks input$populate_forms
    # downstream: trigger form population in other modules
    observe({
      req(moduleState$structured_data)
      tryCatch(
        {
          # Populate form fields directly
          # TODO: The code for campaign and references is rather messy
          # and could perhaps do with some rationalisation.
          if (!is.null(moduleState$structured_data$campaign)) {
            # campaign_data <- populate_campaign_from_llm(
            #   session,
            #   moduleState$structured_data$campaign
            # )
            session$userData$reactiveValues$campaignDataLLM <- moduleState$structured_data$campaign
          }

          if (!is.null(moduleState$structured_data$references)) {
            # reference_data <- populate_references_from_llm(
            #   session,
            #   moduleState$structured_data$references
            # )
            session$userData$reactiveValues$referenceDataLLM <- moduleState$structured_data$references
          }

          # Create structured data for table-based modules and store in session
          if (!is.null(moduleState$structured_data$sites)) {
            sites_data <- create_sites_from_llm(
              moduleState$structured_data$sites,
              moduleState$structured_data$campaign,
              session
            )
            session$userData$reactiveValues$sitesDataLLM <- sites_data
          }

          if (!is.null(moduleState$structured_data$parameters)) {
            parameters_data <- create_parameters_from_llm(
              moduleState$structured_data$parameters,
              session = session
            )
            session$userData$reactiveValues$parametersDataLLM <- parameters_data
          }

          if (!is.null(moduleState$structured_data$compartments)) {
            compartments_data <- create_compartments_from_llm(
              moduleState$structured_data$compartments
            )
            session$userData$reactiveValues$compartmentsDataLLM <- compartments_data
          }

          if (!is.null(moduleState$structured_data$biota)) {
            biota_data <- create_biota_from_llm(
              moduleState$structured_data$biota
            )
            session$userData$reactiveValues$biotaDataLLM <- biota_data
          }

          if (!is.null(moduleState$structured_data$methods)) {
            methods_data <- create_methods_from_llm(
              moduleState$structured_data$methods
            )
            session$userData$reactiveValues$methodsDataLLM <- methods_data
          }

          if (!is.null(moduleState$structured_data$samples)) {
            print_dev(paste0(
              "moduleState$structured_data$samples: ",
              moduleState$structured_data$samples
            ))
            samples_data <- create_samples_from_llm(
              moduleState$structured_data$samples
            )
            print_dev(paste0("samples_data: ", samples_data))
            session$userData$reactiveValues$samplesDataLLM <- samples_data
          }

          # Set extraction status flags
          session$userData$reactiveValues$llmExtractionComplete <- TRUE
          session$userData$reactiveValues$llmExtractionSuccessful <- TRUE

          showNotification(
            "Forms populated with extracted data! Review and correct in each module.",
            type = "default"
          )

          print_dev("All forms populated from LLM extraction")
        },
        error = function(e) {
          showNotification(
            paste("Error populating forms:", e$message),
            type = "error"
          )
          print_dev(glue("Form population error: {e$message}"))
        }
      )
    }) |>
      bindEvent(input$populate_forms)

    ## observe: Clear extraction ----
    # upstream: user clicks input$clear_extraction
    # downstream: reset module state and session data
    observe({
      # Clear module state
      moduleState$extraction_complete <- FALSE
      moduleState$extraction_successful <- FALSE
      moduleState$raw_extraction <- NULL
      moduleState$structured_data <- NULL
      moduleState$error_message <- NULL
      moduleState$api_metadata <- NULL

      # Clear session LLM data and status flags
      clear_llm_data_from_session(session)
      session$userData$reactiveValues$llmExtractionComplete <- FALSE
      session$userData$reactiveValues$llmExtractionSuccessful <- FALSE

      # Disable buttons
      disable("populate_forms")
      disable("clear_extraction")
      disable("download_extraction")

      showNotification("Extraction cleared", type = "message")
    }) |>
      bindEvent(input$clear_extraction)

    ## observe ~ bindEvent: Set session username from ENTERED_BY ----
    observe({
      req(input$ENTERED_BY)

      # only trigger if a username doesn't already exist in the session
      if (!isTruthy(session$userData$reactiveValues$ENTERED_BY)) {
        # Set the reactive value
        session$userData$reactiveValues$ENTERED_BY <- input$ENTERED_BY

        showNotification(
          glue("Saved your username {input$ENTERED_BY} to session data."),
          type = "message"
        )
      }
    }) |>
      bindEvent(input$ENTERED_BY, ignoreInit = FALSE)

    # 3. Outputs ----

    ## output: extraction_status ----
    # upstream: moduleState
    # downstream: UI status display
    output$extraction_status <- renderUI({
      if (!moduleState$extraction_complete) {
        div(
          bs_icon("info-circle"),
          "Upload a PDF and provide your API key to begin extraction, or use dummy data for testing.",
          class = "validation-status validation-info"
        )
      } else if (moduleState$extraction_successful) {
        # Build status message with API metadata
        status_text <- "Data extraction completed successfully. Review results and populate forms below."

        if (!is.null(moduleState$api_metadata)) {
          metadata_text <- paste0(
            " API usage: $",
            sprintf("%.4f", moduleState$api_metadata$total_cost),
            " (",
            moduleState$api_metadata$total_input_tokens,
            " input + ",
            moduleState$api_metadata$total_output_tokens,
            " output tokens, ",
            moduleState$api_metadata$call_count,
            " calls)"
          )
          status_text <- paste0(status_text, metadata_text)
        }

        div(
          bs_icon("check-circle"),
          status_text,
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          paste("Extraction failed:", moduleState$error_message),
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: extraction_results ----
    # upstream: moduleState$raw_extraction
    # downstream: UI results display
    output$extraction_results <- renderText({
      if (!is.null(moduleState$raw_extraction)) {
        # Format the extraction results for display
        if (is.list(moduleState$raw_extraction)) {
          # Pretty print the structured data
          capture.output(str(
            moduleState$raw_extraction,
            max.level = 6,
            vec.len = 10,
            nchar.max = 200
          )) |>
            paste(collapse = "\n")
        } else {
          as.character(moduleState$raw_extraction)
        }
      } else if (!is.null(moduleState$error_message)) {
        paste("Extraction Error:", moduleState$error_message)
      } else {
        "No extraction performed yet."
      }
    })

    ## output: download_extraction ----
    # upstream: moduleState$raw_extraction
    # downstream: file download
    output$download_extraction <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("llm_extraction_", timestamp, ".txt")
      },
      content = function(file) {
        # Create comprehensive output including metadata
        output_lines <- c()

        # Header information ----
        output_lines <- c(
          output_lines,
          "=== LLM PDF Data Extraction Results ===",
          paste("Generated:", Sys.time()),
          ""
        )

        # API metadata if available ----
        if (!is.null(moduleState$api_metadata)) {
          output_lines <- c(
            output_lines,
            "=== API Usage Statistics ===",
            paste(
              "Total Cost: $",
              sprintf("%.4f", moduleState$api_metadata$total_cost)
            ),
            paste("Input Tokens:", moduleState$api_metadata$total_input_tokens),
            paste(
              "Output Tokens:",
              moduleState$api_metadata$total_output_tokens
            ),
            paste("API Calls:", moduleState$api_metadata$call_count),
            ""
          )
        }

        # Structured data output ----
        if (!is.null(moduleState$raw_extraction)) {
          output_lines <- c(
            output_lines,
            "=== Extracted Structured Data ===",
            ""
          )

          # Convert structured data to readable format
          if (is.list(moduleState$raw_extraction)) {
            structured_output <- capture.output(
              str(
                moduleState$raw_extraction,
                max.level = 100,
                vec.len = 200,
                nchar.max = 5000
              )
            )
            output_lines <- c(output_lines, structured_output)
          } else {
            output_lines <- c(
              output_lines,
              as.character(moduleState$raw_extraction)
            )
          }
        }

        # Write to file
        writeLines(output_lines, file, useBytes = TRUE)
      }
    )
  })
}

# 4. Helper Functions ----

#' Create extraction prompt with controlled vocabulary
#' @description Creates the system prompt for Claude extraction
#' @importFrom readr read_file
#' @noRd
create_extraction_prompt <- function() {
  read_file("inst/app/www/md/extraction_prompt.md")
}

#' Clear LLM data from session reactiveValues
#' @param session Shiny session object
#' @noRd
clear_llm_data_from_session <- function(session) {
  session$userData$reactiveValues$campaignDataLLM <- NULL
  session$userData$reactiveValues$referenceDataLLM <- NULL
  session$userData$reactiveValues$sitesDataLLM <- NULL
  session$userData$reactiveValues$parametersDataLLM <- NULL
  session$userData$reactiveValues$compartmentsDataLLM <- NULL
  session$userData$reactiveValues$biotaDataLLM <- NULL
  session$userData$reactiveValues$methodsDataLLM <- NULL
  session$userData$reactiveValues$samplesDataLLM <- NULL
  showNotification(
    "Cleared all LLM extracted data from session",
    type = "message"
  )
}

## To be copied in the UI ----
# mod_llm_ui("llm_1")

## To be copied in the server ----
# mod_llm_server("llm_1")
