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
          input_task_button(
            id = ns("delete_selected"),
            label = HTML(paste(
              bsicons::bs_icon("trash"),
              "Delete Selected"
            )),
            class = "btn-danger",
            width = "100%"
          )
        ),
        DTOutput(ns("bookmarks_table"))
      )
    )
  )
}

#' Bookmark Manager Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI observeEvent updateTextInput updateQueryString showNotification req isolate
#' @importFrom DT renderDT datatable formatDate
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc filter
#' @importFrom glue glue
#' @export
mod_bookmark_manager_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      bookmarks_metadata = NULL,
      selected_rows = integer(0)
    )

    ## Constants ----
    bookmark_dir <- "shiny_bookmarks"
    metadata_file <- file.path(bookmark_dir, "bookmark_metadata.rds")

    # 2. Helper functions ----
    ## get_bookmark_metadata: load or create metadata file ----
    get_bookmark_metadata <- reactive({
      # Create bookmark directory if it doesn't exist
      if (!dir.exists(bookmark_dir)) {
        dir.create(bookmark_dir, recursive = TRUE)
      }

      browser()
      # Load existing metadata or create new
      if (file.exists(metadata_file)) {
        metadata <- readRDS(metadata_file)
      } else {
        metadata <- tibble(
          state_id = character(0),
          name = character(0),
          campaign_name_short = character(0),
          reference_id = character(0),
          username = character(0),
          created_date = as.POSIXct(character(0)),
          last_accessed = as.POSIXct(character(0)),
          description = character(0)
        )
      }

      # Sync with actual bookmark directories
      existing_dirs <- list.dirs(
        bookmark_dir,
        full.names = FALSE,
        recursive = FALSE
      )
      existing_dirs <- existing_dirs[existing_dirs != ""] # Remove empty string

      # Remove metadata for non-existent directories
      metadata <- metadata |>
        filter(state_id %in% existing_dirs)

      # Add metadata for new directories without metadata
      new_dirs <- setdiff(existing_dirs, metadata$state_id)
      if (length(new_dirs) > 0) {
        for (dir_name in new_dirs) {
          dir_path <- file.path(bookmark_dir, dir_name)
          values_file <- file.path(dir_path, "values.rds")

          if (file.exists(values_file)) {
            values <- readRDS(values_file)

            new_row <- tibble(
              state_id = dir_name,
              name = values$bookmarkName %||% "Unnamed Session",
              campaign_name_short = values$bookmarkName %||% "Unknown Campaign",
              reference_id = values$bookmarkName %||% "Unnamed Reference",
              username = values$bookmarkUsername %||% "Unknown",
              created_date = values$bookmarkTimestamp %||%
                file.info(dir_path)$mtime,
              last_accessed = values$bookmarkTimestamp %||%
                file.info(dir_path)$mtime,
              description = ""
            )

            metadata <- rbind(metadata, new_row)
          }
        }
      }

      # Save updated metadata
      saveRDS(metadata, metadata_file)

      # Sort by creation date (most recent first)
      metadata |>
        arrange(desc(created_date))
    })

    ## save_bookmark_metadata: save metadata to file ----
    save_bookmark_metadata <- function(metadata) {
      saveRDS(metadata, metadata_file)
      moduleState$bookmarks_metadata <- metadata
    }

    ## update_last_accessed: update last accessed time for a bookmark ----
    update_last_accessed <- function(state_id) {
      metadata <- moduleState$bookmarks_metadata
      if (!is.null(metadata) && state_id %in% metadata$state_id) {
        metadata$last_accessed[metadata$state_id == state_id] <- Sys.time()
        save_bookmark_metadata(metadata)
      }
    }

    # 3. Observers and Reactives ----

    ## observe: update moduleState with current bookmarks ----
    # upstream: get_bookmark_metadata()
    # downstream: moduleState$bookmarks_metadata
    observe({
      moduleState$bookmarks_metadata <- get_bookmark_metadata()
    }) |>
      bindEvent(input$save_bookmark)

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

      metadata <- moduleState$bookmarks_metadata
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

    ## observe ~ bindEvent: delete selected bookmarks ----
    # upstream: user clicks input$delete_selected
    # downstream: bookmark file deletion
    observe({
      req(input$bookmarks_table_rows_selected)

      selected_rows <- input$bookmarks_table_rows_selected
      metadata <- moduleState$bookmarks_metadata
      selected_bookmarks <- metadata[selected_rows, ]

      if (nrow(selected_bookmarks) == 0) {
        showNotification(
          "No sessions selected for deletion.",
          type = "warning"
        )
        return()
      }

      # Delete bookmark directories and update metadata
      deleted_names <- character(0)
      remaining_metadata <- metadata[-selected_rows, ]

      for (i in seq_len(nrow(selected_bookmarks))) {
        bookmark <- selected_bookmarks[i, ]
        bookmark_path <- file.path(bookmark_dir, bookmark$state_id)

        if (dir.exists(bookmark_path)) {
          unlink(bookmark_path, recursive = TRUE)
          deleted_names <- c(deleted_names, bookmark$name)
        }
      }

      # Save updated metadata
      save_bookmark_metadata(remaining_metadata)

      # Show success notification
      if (length(deleted_names) > 0) {
        showNotification(
          glue("Deleted sessions: {paste(deleted_names, collapse = ', ')}"),
          type = "message"
        )
      }
    }) |>
      bindEvent(input$delete_selected)

    # 4. Outputs ----

    ## output: bookmarks_table ----
    # upstream: moduleState$bookmarks_metadata
    # downstream: DT table display
    output$bookmarks_table <- renderDT({
      metadata <- moduleState$bookmarks_metadata

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

      # Prepare display data
      display_data <- metadata |>
        select(
          UID = state_id,
          Name = name,
          Username = username,
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
            list(className = "dt-center", targets = c(1, 2, 3))
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
    exportTestValues(
      module_metadata = moduleState$bookmarks_metadata,
      selected_rows = input$bookmarks_table_rows_selected
    )
  })
}

## To be copied in the UI ----
# mod_bookmark_manager_ui("bookmark_manager_1")

## To be copied in the server ----
# bookmark_manager_data <- mod_bookmark_manager_server("bookmark_manager_1")
