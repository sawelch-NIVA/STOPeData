# Bookmark Manager Module ----
# A Shiny module for managing session bookmarks using DT with centralized metadata

#' Bookmark Manager UI Function ----
#'
#' @description A shiny Module for bookmark management functionality using DT.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput hr h4 HTML
#' @importFrom bslib card card_body input_task_button layout_column_wrap
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @export
mod_bookmark_manager_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main bookmark management card ----
    card(
      fill = TRUE,
      card_body(
        ## Input section ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### Session name input ----
          # textInput(
          #   inputId = ns("bookmark_name"),
          #   label = "Session Name:",
          #   placeholder = "Enter a name for this session",
          #   value = "",
          #   width = "100%"
          # ),

          ### Save session button ----
          input_task_button(
            id = ns("save_bookmark"),
            label = HTML(paste(
              bsicons::bs_icon("floppy"),
              "Save Session"
            )),
            width = "100%",
            class = "btn-success"
          )
        ),

        hr(),

        ## Saved sessions table ----
        h4("Saved Sessions"),
        ## Action buttons ----
        layout_column_wrap(
          width = "200px",
          fill = FALSE,
          fillable = FALSE,

          ### Load selected button ----
          input_task_button(
            id = ns("load_selected"),
            label = HTML(paste(
              bsicons::bs_icon("folder"),
              "Load Selected"
            )),
            class = "btn-success",
            width = "100%"
          ),

          ### Delete selected button ----
          # input_task_button(
          #   id = ns("delete_selected"),
          #   label = HTML(paste(
          #     bsicons::bs_icon("trash"),
          #     "Delete Selected"
          #   )),
          #   class = "btn-danger",
          #   width = "100%"
          # )
        ),
        DTOutput(ns("bookmarks_table"))
      )
    )
  )
}

#' Bookmark Manager Server Functions ----#' Bookmark Manager Server Module
#'
#' @description
#' Server-side logic for managing Shiny application bookmarks stored in Google Drive.
#' This module handles bookmark creation, loading, deletion, and metadata management
#' through Google Drive integration.
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI observeEvent updateTextInput updateQueryString showNotification req isolate
#' @importFrom DT renderDT datatable formatDate
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc filter select
#' @importFrom glue glue
#' @importFrom jsonlite write_json read_json toJSON
#' @importFrom googledrive drive_api_key drive_get drive_mkdir drive_ls drive_download drive_upload drive_update as_id
#'
#' @export
mod_bookmark_manager_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Authenticate
    drive_api_key(Sys.getenv("GOOGLE_DRIVE_API_KEY"))

    # Locate bookmark folder
    bookmarks_folder_name <- "Saved Sessions"
    bookmarks_folder <- googledrive::drive_get(bookmarks_folder_name)

    if (nrow(bookmarks_folder) == 0) {
      # Create folder if it doesn't exist
      bookmarks_folder <- drive_mkdir(bookmarks_folder_name)
    }

    BOOKMARKS_FOLDER_ID <- as_id(bookmarks_folder$id)

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      selected_rows = integer(0)
    )

    ## Constants ----
    bookmark_dir <- "shiny_bookmarks"
    metadata_file <- file.path(bookmark_dir, "bookmark_metadata.json") # Changed to .json

    get_bookmark_metadata <- function() {
      metadata_file_name <- "bookmark_metadata.json"

      # Try to find the metadata file in the bookmarks folder
      metadata_file <- drive_ls(
        path = BOOKMARKS_FOLDER_ID,
        pattern = metadata_file_name
      )

      if (nrow(metadata_file) > 0) {
        # Download and read the metadata file
        temp_file <- tempfile(fileext = ".json")
        drive_download(
          as_id(metadata_file$id[1]),
          path = temp_file,
          overwrite = TRUE
        )

        metadata <- read_json(temp_file, simplifyVector = TRUE) |>
          tibble::as_tibble()

        # Convert date columns back to POSIXct
        metadata$created_date <- as.POSIXct(metadata$created_date)
        metadata$last_accessed <- as.POSIXct(metadata$last_accessed)

        unlink(temp_file)
      } else {
        # Create empty metadata structure
        metadata <- tibble(
          state_id = character(0),
          name = character(0),
          campaign_name_short = character(0),
          reference_id = character(0),
          username = character(0),
          created_date = as.POSIXct(character(0)),
          last_accessed = as.POSIXct(character(0)),
          total_size_mb = numeric(0),
          description = character(0)
        )
      }

      # Get list of bookmark folders in Google Drive
      bookmark_folders <- drive_ls(path = BOOKMARKS_FOLDER_ID, type = "folder")
      existing_state_ids <- bookmark_folders$name

      # Remove metadata for non-existent folders
      metadata <- metadata |>
        filter(state_id %in% existing_state_ids)

      # Add metadata for new folders without metadata
      new_state_ids <- setdiff(existing_state_ids, metadata$state_id)

      if (length(new_state_ids) > 0) {
        for (state_id in new_state_ids) {
          # Find the folder
          state_folder <- bookmark_folders |>
            filter(name == state_id)

          # Look for values.rds in this folder
          folder_files <- drive_ls(path = as_id(state_folder$id))
          values_file <- folder_files |> filter(name == "values.rds")

          if (nrow(values_file) > 0) {
            # Download and read values.rds
            temp_values <- tempfile(fileext = ".rds")
            drive_download(
              as_id(values_file$id[1]),
              path = temp_values,
              overwrite = TRUE
            )
            values <- readRDS(temp_values)
            unlink(temp_values)

            # Calculate total size from Drive
            total_size_bytes <- sum(
              folder_files$drive_resource |>
                lapply(function(x) as.numeric(x$size %||% 0)) |>
                unlist()
            )
            total_size_mb <- round(total_size_bytes / (1024^2), 2)

            new_row <- tibble(
              state_id = state_id,
              name = values$bookmarkName %||% "Unnamed Session",
              campaign_name_short = values$bookmarkCampaign %||%
                "Unknown Campaign",
              reference_id = values$bookmarkReference %||% "Unknown Reference",
              username = values$bookmarkUsername %||% "Unknown",
              created_date = values$bookmarkTimestamp %||% Sys.time(),
              last_accessed = values$bookmarkTimestamp %||% Sys.time(),
              total_size_mb = total_size_mb,
              description = ""
            )

            metadata <- rbind(metadata, new_row)
          }
        }
      }

      # Save updated metadata back to Google Drive
      if (nrow(metadata) > 0) {
        temp_metadata <- tempfile(fileext = ".json")
        write_json(
          metadata,
          temp_metadata,
          pretty = TRUE,
          auto_unbox = TRUE,
          POSIXt = "ISO8601"
        )

        # Check if metadata file exists, update or create
        if (nrow(metadata_file) > 0) {
          drive_update(as_id(metadata_file$id[1]), media = temp_metadata)
        } else {
          drive_upload(
            temp_metadata,
            path = BOOKMARKS_FOLDER_ID,
            name = metadata_file_name
          )
        }

        unlink(temp_metadata)
      }

      # Sort by creation date (most recent first)
      metadata |>
        arrange(desc(created_date))
    }

    save_bookmark_metadata <- function(metadata) {
      metadata_file_name <- "bookmark_metadata.json"

      # Write to temporary file
      temp_metadata <- tempfile(fileext = ".json")
      write_json(
        metadata,
        temp_metadata,
        pretty = TRUE,
        auto_unbox = TRUE,
        POSIXt = "ISO8601"
      )

      # Find existing metadata file
      metadata_file <- drive_ls(
        path = BOOKMARKS_FOLDER_ID,
        pattern = metadata_file_name
      )

      # Update or create
      if (nrow(metadata_file) > 0) {
        drive_update(as_id(metadata_file$id[1]), media = temp_metadata)
      } else {
        drive_upload(
          temp_metadata,
          path = BOOKMARKS_FOLDER_ID,
          name = metadata_file_name
        )
      }

      unlink(temp_metadata)

      session$userData$reactiveValues$bookmarkedSessions <- metadata
    }

    update_last_accessed <- function(state_id) {
      metadata <- session$userData$reactiveValues$bookmarkedSessions
      if (!is.null(metadata) && state_id %in% metadata$state_id) {
        metadata$last_accessed[metadata$state_id == state_id] <- Sys.time()
        save_bookmark_metadata(metadata)
      }
    }

    # 3. Observers and Reactives ----

    ## observe: update moduleState with current bookmarks ----
    # upstream: get_bookmark_metadata()
    # downstream: session$userData$reactiveValues$bookmarkedSessions
    observe({
      session$userData$reactiveValues$bookmarkedSessions <- get_bookmark_metadata()
    }) |>
      bindEvent()

    ## observe ~ bindEvent: save bookmark with custom name ----
    # upstream: user clicks input$save_bookmark
    # downstream: session bookmark creation
    observe({
      # Get bookmark name or create default
      name <- paste("Session", format(Sys.time(), "%Y-%m-%d %H:%M"))

      # Get username from session
      username <- session$userData$reactiveValues$ENTERED_BY %||% "Unknown User"

      # Store bookmark metadata for onBookmark callback
      session$userData$bookmarkName <- name
      session$userData$bookmarkUsername <- username
      session$userData$bookmarkTimestamp <- Sys.time()

      # Trigger bookmark creation
      session$doBookmark()

      # Show success notification
      showNotification(
        glue("Session '{name}' saved successfully."),
        type = "message"
      )
    }) |>
      bindEvent(input$save_bookmark)

    ## observe ~ bindEvent: load selected bookmark ----
    # upstream: user clicks input$load_selected
    # downstream: session navigation
    observe({
      req(input$bookmarks_table_rows_selected)

      selected_rows <- input$bookmarks_table_rows_selected
      if (length(selected_rows) != 1) {
        showNotification(
          "Please select exactly one session to load.",
          type = "warning"
        )
        return()
      }

      metadata <- session$userData$reactiveValues$bookmarkedSessions
      selected_bookmark <- metadata[selected_rows, ]

      # Update last accessed time
      update_last_accessed(selected_bookmark$state_id)

      # Construct the bookmark URL
      bookmark_url <- paste0(
        session$clientData$url_protocol,
        "//",
        session$clientData$url_hostname,
        ":",
        session$clientData$url_port,
        session$clientData$url_pathname,
        "?_state_id_=",
        selected_bookmark$state_id
      )

      # Redirect to bookmark URL
      updateQueryString(bookmark_url, mode = "replace", session = session)

      # Show loading notification
      showNotification(
        glue("Loading session '{selected_bookmark$name}'..."),
        type = "message"
      )
    }) |>
      bindEvent(input$load_selected)

    # 4. Outputs ----

    ## output: bookmarks_table ----
    #' @description
    #' Renders a DT datatable displaying bookmark metadata with formatting for dates
    #' and proper column alignment. Shows a message when no bookmarks exist.
    output$bookmarks_table <- renderDT({
      metadata <- session$userData$reactiveValues$bookmarkedSessions

      # Handle empty bookmark list
      if (is.null(metadata) || nrow(metadata) == 0) {
        return(
          datatable(
            tibble(Message = "No saved sessions found."),
            options = list(
              dom = "t",
              ordering = FALSE,
              searching = FALSE,
              paging = FALSE,
              info = FALSE
            ),
            selection = "none",
            rownames = FALSE
          )
        )
      }

      # Prepare display data with new columns
      display_data <- metadata |>
        select(
          UID = state_id,
          Name = name,
          Campaign = campaign_name_short,
          Reference = reference_id,
          Username = username,
          `Size (MB)` = total_size_mb,
          Created = created_date,
          `Last Accessed` = last_accessed
        )

      # Create DT table
      datatable(
        display_data,
        options = list(
          dom = "ftip",
          pageLength = 10,
          ordering = TRUE,
          searching = TRUE,
          select = list(style = "multi"),
          columnDefs = list(
            list(className = "dt-center", targets = c(1, 2, 3, 4, 5, 6)),
            list(className = "dt-right", targets = 5) # Right-align size column
          )
        ),
        selection = "multiple",
        rownames = FALSE,
        filter = "top"
      ) |>
        formatDate(
          columns = c("Created", "Last Accessed"),
          method = "toLocaleString"
        )
    })

    ## export: export variables for testing ----
    # exportTestValues(
    #   module_metadata = session$userData$reactiveValues$bookmarkedSessions,
    #   selected_rows = input$bookmarks_table_rows_selected
    # )
  })
}

#' @rdname mod_bookmark_manager_server
#' @section UI Function:
#' To use this module, include the following in your UI:
#' @eval "mod_bookmark_manager_ui('bookmark_manager_1')"
#'
#' @section Server Function:
#' To use this module, include the following in your server:
#' @eval "bookmark_manager_data <- mod_bookmark_manager_server('bookmark_manager_1')"
