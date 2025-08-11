# PDF Metadata Extraction Shiny App ----
# Uses ellmer to extract structured data from uploaded PDFs via Claude

# Load required libraries ----
library(shiny)
library(shinyvalidate)
library(ellmer)

# UI ----
ui <- fluidPage(
  titlePanel("PDF Metadata Extraction"),

  sidebarLayout(
    sidebarPanel(
      # File upload ----
      fileInput("pdf_file", "Upload PDF", accept = ".pdf"),

      # API key input (temporary for demo) ----
      textInput(
        "api_key",
        "Claude API Key",
        placeholder = "Enter your Anthropic API key"
      ),

      # Extract button ----
      actionButton("extract", "Extract Metadata", class = "btn-primary"),

      br(),
      br(),

      # Status output ----
      verbatimTextOutput("status")
    ),

    mainPanel(
      # Input fields with validation ----
      h3("Extracted Metadata"),

      textInput(
        "author",
        "Author(s)",
        placeholder = "Will be populated after extraction"
      ),

      textInput(
        "title",
        "Title",
        placeholder = "Will be populated after extraction"
      ),

      numericInput(
        "year",
        "Published Year",
        value = NA,
        min = 1800,
        max = 2026,
        step = 1
      ),

      br(),

      # Data preview ----
      h4("Extracted Data Object"),
      verbatimTextOutput("data_preview")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Reactive values to store extracted data ----
  extracted_data <- reactiveVal(NULL)

  # Input validation setup ----
  iv <- InputValidator$new()

  iv$add_rule("author", sv_required())
  iv$add_rule(
    "author",
    ~ if (nchar(.) > 1000) "Author field too long (max 1000 characters)"
  )

  iv$add_rule("title", sv_required())
  iv$add_rule(
    "title",
    ~ if (nchar(.) > 1000) "Title field too long (max 1000 characters)"
  )

  iv$add_rule("year", sv_required())
  iv$add_rule(
    "year",
    sv_between(1800, 2026, message_fmt = "Year must be between 1800 and 2026")
  )

  iv$enable()

  # PDF extraction logic ----
  observeEvent(input$extract, {
    # Validation checks ----
    req(input$pdf_file)
    req(input$api_key)

    if (input$api_key == "") {
      output$status <- renderText("Please enter your Claude API key")
      return()
    }

    output$status <- renderText("Processing PDF...")

    tryCatch(
      {
        # Set up Claude chat with API key ----
        Sys.setenv(ANTHROPIC_API_KEY = input$api_key)
        chat <- chat_anthropic()

        # Define structured data type for extraction ----
        type_metadata <- type_object(
          "Extract bibliographic metadata from this PDF document",
          author = type_string(
            "Author(s) of the document. If multiple authors, separate with semicolons.",
            required = FALSE
          ),
          title = type_string(
            "Title of the document or paper",
            required = FALSE
          ),
          year = type_integer(
            "Publication year (4-digit year between 1800-2026)",
            required = FALSE
          )
        )

        # Create PDF content object ----
        pdf_content <- content_pdf_file(input$pdf_file$datapath)

        # Extract metadata using structured chat ----
        result <- chat$chat_structured(
          pdf_content,
          type = type_metadata
        )

        # Store the result ----
        extracted_data(result)

        # Update UI fields ----
        updateTextInput(
          session,
          "author",
          value = ifelse(
            is.na(result$author) || is.null(result$author),
            "",
            result$author
          )
        )

        updateTextInput(
          session,
          "title",
          value = ifelse(
            is.na(result$title) || is.null(result$title),
            "",
            result$title
          )
        )

        updateNumericInput(
          session,
          "year",
          value = ifelse(
            is.na(result$year) || is.null(result$year),
            NA,
            result$year
          )
        )

        output$status <- renderText("Extraction completed successfully!")
      },
      error = function(e) {
        output$status <- renderText(paste("Error:", e$message))
      }
    )
  })

  # Display extracted data object ----
  output$data_preview <- renderPrint({
    data <- extracted_data()
    if (!is.null(data)) {
      cat("Extracted data structure:\n")
      str(data)
      cat("\nData content:\n")
      print(data)
    } else {
      cat("No data extracted yet. Upload a PDF and click 'Extract Metadata'.")
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
