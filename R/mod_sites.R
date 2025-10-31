# Sites Import Module ----
# A Shiny module for site data entry with table-based editing and map visualization

#' Sites UI Function ----
#'
#' @description A shiny Module for site data entry with editable table and map visualization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput numericInput textInput dateInput textAreaInput actionButton tags
#' @importFrom bslib css card card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom leaflet leafletOutput
#' @export
mod_sites_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(
      "
  "
    )),
    # Enable shinyjs
    useShinyjs(),

    # Main content layout ----
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "3fr 1fr"),
      fill = TRUE,
      fillable = TRUE,

      ## Left panel: Table and controls ----
      card(
        full_screen = TRUE,
        height = "30vh",
        fill = TRUE,
        fillable = TRUE,
        card_body(
          style = "min-height: 300px !important;",
          ### Info accordion ----
          info_accordion(content_file = "inst/app/www/md/intro_sites.md"),

          ### Table controls ----
          div(
            style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",

            # Number of sites input
            numericInput(
              inputId = ns("num_sites"),
              label = tooltip(
                list("n sites", bs_icon("info-circle-fill")),
                "Number of sites to add at once."
              ),
              value = 10,
              min = 1,
              max = 50,
              width = "80px"
            ),

            # Base site code input
            textInput(
              inputId = ns("base_site_code"),
              label = tooltip(
                list("Site Code Root", bs_icon("info-circle-fill")),
                "Project/Campaign-related short name to append to all site codes in format base_nnn"
              ),
              value = "",
              placeholder = "e.g., NIVA_AQUAMONITOR_",
              width = "200px"
            ),

            # Add button
            div(
              input_task_button(
                id = ns("add_site"),
                label = "Add Site(s)",
                icon = icon("plus"),
                class = "btn-success",
                width = "120px"
              )
            ),

            ### Validation status ----
            uiOutput(ns("validation_reporter"))
          ),
          ### Raw data accordion ----
          accordion(
            id = ns("data_accordion"),
            open = FALSE,
            accordion_panel(
              title = "Click to view raw validated data",
              icon = bs_icon("code"),
              verbatimTextOutput(ns("validated_data_display"))
            )
          )
        )
      ),
      ## Top right card: Map ----
      card(
        height = "30vh",
        full_screen = TRUE,
        card_body(
          ### Leaflet map ----
          leafletOutput(ns("sites_map"), height = "100%"),

          ### Map controls ----

          # Add point button
          div(
            style = "margin-top: auto;
                      margin-bottom: auto;
                      flex: 1 1 auto;
                      align-content: space-between;",

            tooltip(
              input_task_button(
                id = ns("add_map_point"),
                label = "Add",
                icon = icon("plus"),
                class = "btn-success btn-sm",
                style = "width: 80px; height: fit-content;",
                label_busy = "..."
              ),
              "Add a new table with the current coordinates",
              id = "add_map_point_tooltip",
              placement = "bottom"
            ),

            # Update selected button
            tooltip(
              input_task_button(
                id = ns("update_selected_coords"),
                label = "Update",
                icon = icon("map-pin"),
                class = "btn-primary btn-sm",
                style = "width: 80px; height: fit-content;",
                label_busy = "..."
              ),
              "Update the selected table row(s) with the current coordinates",

              id = "update_selected_coords_tooltip",
              placement = "bottom"
            ),

            # Hide labels button
            tooltip(
              input_task_button(
                id = ns("toggle_labels"),
                label = "Labels",
                icon = icon("tag"),
                class = "btn-warning btn-sm",
                style = "width: 80px; height: fit-content;",
                label_busy = "..."
              ),
              "Click to show/hide site labels on the map",
              id = "toggle_labels_tooltip",
              placement = "bottom",
            ),

            # Selected coordinates reporter
            div(
              style = "display: flex; flex-direction: column; font-size: 0.9em; font-family: ",
              textOutput(ns("selected_coords")),
              textOutput(ns("selected_rows_reporter"))
            )
          )
        )
      )
    ),

    ### Bottom card: Sites table ----
    card(
      full_screen = TRUE,
      div(
        rHandsontableOutput(ns("sites_table")),
        style = "margin-bottom: 10px;"
      )
    )
  )
}

#' Sites Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu hot_table hot_cell hot_validate_numeric hot_validate_character
#' @importFrom shinyjs enable disable
#' @importFrom tibble tibble
#' @importFrom leaflet renderLeaflet leaflet addTiles addMarkers mapOptions labelOptions clearMarkers setView leafletProxy addCircleMarkers clearGroup
#' @import ISOcodes
#' @importFrom dplyr pull
#' @importFrom bslib update_task_button
#' @export
mod_sites_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      sites_data = initialise_sites_tibble(),
      validated_data = initialise_sites_tibble(),
      is_valid = FALSE,
      selected_rows = NULL,
      next_site_id = 1,
      clicked_coords = NULL,
      show_labels = TRUE
    )

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    iv$add_rule("sites_table_validation", function(value) {
      if (nrow(moduleState$sites_data) == 0) {
        "At least one site must be added"
      } else {
        # Check if all required fields are filled for each row
        required_fields <- c(
          "SITE_CODE",
          "SITE_NAME",
          "SITE_GEOGRAPHIC_FEATURE",
          "LATITUDE",
          "LONGITUDE",
          "SITE_COORDINATE_SYSTEM",
          "ALTITUDE_VALUE",
          "ALTITUDE_UNIT",
          "ENTERED_BY",
          "ENTERED_DATE"
        )

        for (i in 1:nrow(moduleState$sites_data)) {
          for (field in required_fields) {
            value <- moduleState$sites_data[i, field]
            if (
              is.na(value) || value == "" || (is.numeric(value) && is.na(value))
            ) {
              return(paste("Row", i, "is missing required field:", field))
            }
          }

          # Validate coordinate ranges
          lat <- moduleState$sites_data[i, "LATITUDE"]
          lon <- moduleState$sites_data[i, "LONGITUDE"]
          alt <- moduleState$sites_data[i, "ALTITUDE_VALUE"]

          if (!is.na(lat) && (lat < -90 || lat > 90)) {
            return(paste("Row", i, "has invalid latitude (must be -90 to 90)"))
          }
          if (!is.na(lon) && (lon < -180 || lon > 180)) {
            return(paste(
              "Row",
              i,
              "has invalid longitude (must be -180 to 180)"
            ))
          }
          if (!is.na(alt) && (alt < -12000 || alt > 9000)) {
            return(paste(
              "Row",
              i,
              "has invalid altitude (must be -12000 to 9000)"
            ))
          }
        }
        NULL # All validations passed
      }
    })

    iv$enable()

    # 2. Helper functions ----

    ## Create new site row with defaults ----
    create_new_site <- function(site_number = 1, base_code = "", session) {
      # Generate site code
      if (base_code == "" || is.null(base_code)) {
        site_code <- paste0("SITE_", sprintf("%03d", site_number))
      } else {
        site_code <- paste0(base_code, sprintf("%03d", site_number))
      }

      initialise_sites_tibble() |>
        add_row(
          SITE_CODE = site_code,
          SITE_NAME = "",
          SITE_GEOGRAPHIC_FEATURE = "Not reported",
          SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
          SITE_COORDINATE_SYSTEM = "WGS 84",
          LATITUDE = NA,
          LONGITUDE = NA,
          COUNTRY = "",
          AREA = "",
          ALTITUDE_VALUE = NA,
          ALTITUDE_UNIT = "m",
          ENTERED_BY = session$userData$reactiveValues$ENTERED_BY %|truthy|% "",
          ENTERED_DATE = as.character(Sys.Date()),
          SITE_COMMENT = ""
        )
    }

    # 3. Observers and Reactives ----

    ## observe: Track table row selection ----
    # upstream: input$sites_table_select (from selectCallback)
    # downstream: moduleState$selected_rows
    observe({
      selection <- input$sites_table_select$select$rAll
      if (!is.null(selection)) {
        # Extract selected row indices (1-based)
        selected <- input$sites_table_select$select$rAll
        if (!is.null(selected) && length(selected) > 0) {
          moduleState$selected_rows <- selected
        } else {
          moduleState$selected_rows <- NULL
        }
      } else {
        moduleState$selected_rows <- NULL
      }
    })

    ## observe: Add new site(s) ----
    # upstream: user clicks input$add_site
    # downstream: moduleState$sites_data
    observe({
      num_sites <- input$num_sites
      base_code <- input$base_site_code

      # Create multiple sites
      new_sites_list <- lapply(1:num_sites, function(i) {
        create_new_site(
          site_number = i,
          base_code = base_code,
          username = session$userData$reactiveValues$ENTERED_BY
        )
      })

      # Combine into single data frame
      new_sites <- do.call(rbind, new_sites_list)

      # Add to existing data
      moduleState$sites_data <- rbind(moduleState$sites_data, new_sites)

      # Show notification
      if (num_sites == 1) {
        showNotification(
          "New site added. Edit fields directly in the table.",
          type = "message"
        )
      } else {
        showNotification(
          paste(num_sites, "sites added. Edit fields directly in the table."),
          type = "message"
        )
      }
    }) |>
      bindEvent(input$add_site)

    ## observe: Handle table changes ----
    # upstream: input$sites_table changes
    # downstream: moduleState$sites_data
    observe({
      if (!is.null(input$sites_table)) {
        # Update sites_data from the table
        moduleState$sites_data <- hot_to_r(input$sites_table)
      }
    }) |>
      bindEvent(autoInvalidate())

    ## observer: receive data from session$userData$reactiveValues$sitesData (import) ----
    ## and update module data
    observe({
      moduleState$sites_data <- session$userData$reactiveValues$sitesData
      print_dev("Assigned saved data to sites moduleData.")
    }) |>
      bindEvent(
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    ## observe: Capture map clicks ----
    # upstream: input$sites_map_click
    # downstream: moduleState$clicked_coords
    observe({
      click <- input$sites_map_click
      if (!is.null(click)) {
        moduleState$clicked_coords <- list(
          lat = click$lat,
          lng = click$lng
        )

        # Update map with clicked point marker
        leafletProxy("sites_map") |>
          clearGroup("clicked_point") |>
          addCircleMarkers(
            lng = click$lng,
            lat = click$lat,
            radius = 3,
            color = "black",
            fillColor = "red",
            fillOpacity = 0.8,
            stroke = TRUE,
            weight = 2,
            group = "clicked_point",
            popup = paste0(
              "<strong>Clicked Point</strong><br/>",
              "Lat: ",
              round(click$lat, 6),
              "<br/>",
              "Lng: ",
              round(click$lng, 6)
            )
          )
      }
    })

    ## observe: Add point from map ----
    # upstream: user clicks input$add_map_point
    # downstream: moduleState$sites_data
    observe({
      req(moduleState$clicked_coords)

      # Create new site with map coordinates
      new_site <- create_new_site(
        site_number = moduleState$next_site_id,
        base_code = input$base_site_code,
        username = session$userData$reactiveValues$ENTERED_BY
      )

      # Set coordinates from map click
      new_site$LATITUDE <- moduleState$clicked_coords$lat
      new_site$LONGITUDE <- moduleState$clicked_coords$lng

      # Add to existing data
      moduleState$sites_data <- rbind(moduleState$sites_data, new_site)

      # Increment next site ID
      moduleState$next_site_id <- moduleState$next_site_id + 1

      # Clear selected coordinates
      moduleState$clicked_coords <- NULL

      # Clear clicked point marker
      leafletProxy("sites_map") |>
        clearGroup("clicked_point")

      # Show notification
      showNotification(
        paste(
          "New site added, Lat:",
          round(new_site$LATITUDE, 6),
          ", Lng:",
          round(new_site$LONGITUDE, 6)
        ),
        type = "message"
      )
    }) |>
      bindEvent(input$add_map_point)

    ## observe: Update selected rows with map coordinates ----
    # upstream: user clicks input$update_selected_coords
    # downstream: moduleState$sites_data
    observe({
      req(moduleState$clicked_coords)
      req(moduleState$selected_rows)
      req(nrow(moduleState$sites_data) > 0)

      # Update coordinates for all selected rows
      for (row_idx in moduleState$selected_rows) {
        if (row_idx <= nrow(moduleState$sites_data)) {
          moduleState$sites_data[
            row_idx,
            "LATITUDE"
          ] <- moduleState$clicked_coords$lat
          moduleState$sites_data[
            row_idx,
            "LONGITUDE"
          ] <- moduleState$clicked_coords$lng
        }
      }

      # Clear selected coordinates and marker
      moduleState$clicked_coords <- NULL
      leafletProxy("sites_map") |>
        clearGroup("clicked_point")

      # Show notification
      showNotification(
        paste(
          "Updated coordinates for",
          length(moduleState$selected_rows),
          "selected row(s)"
        ),
        type = "message"
      )
    }) |>
      bindEvent(input$update_selected_coords)

    ## observe: Toggle map text label visibility ----
    # upstream: input$toggle_labels
    observe({
      moduleState$show_labels <- !moduleState$show_labels

      # Update button text and icon - this doesn't actually work!
      if (moduleState$show_labels) {
        update_task_button(
          # session,
          id = "toggle_labels",
          # label = "Hide Labels",
          # icon = icon("eye-slash"),
          # state = "ready"
        )
      } else {
        update_task_button(
          # session,
          id = "toggle_labels",
          # label = "Show Labels",
          # icon = icon("eye"),
          state = "ready"
        )
      }
    }) |>
      bindEvent(input$toggle_labels)

    ## observe: Check overall validation status ----
    # upstream: moduleState$sites_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data
    observe({
      # Trigger validation check
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$sites_data) > 0) {
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$sites_data

        session$userData$reactiveValues$sitesData <- moduleState$validated_data
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    }) |>
      bindEvent(autoInvalidate())

    ## observe: Load from LLM data when available ----
    # upstream: session$userData$reactiveValues$sitesDataLLM
    # downstream: moduleState$sites_data
    observe({
      print_dev("observe~bindEvent: load data from LLM module")
      llm_sites <- session$userData$reactiveValues$sitesDataLLM
      if (
        !is.null(llm_sites) &&
          nrow(llm_sites) > 0 &&
          session$userData$reactiveValues$llmExtractionComplete
      ) {
        # Replace current sites data with LLM data
        # This will trigger validation and may show warnings for invalid fields
        moduleState$sites_data <- llm_sites

        # Update next_site_id counter
        moduleState$next_site_id <- nrow(llm_sites) + 1

        showNotification(
          paste(
            "Loaded",
            nrow(llm_sites),
            "sites from LLM extraction."
          ),
          type = "message"
        )
      }
    }) |>
      bindEvent(
        label = "observe~bindEvent: load data from LLM module",
        session$userData$reactiveValues$sitesDataLLM,
        session$userData$reactiveValues$llmExtractionComplete,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    # 4. Outputs ----

    ## output: sites_table ----
    # upstream: moduleState$sites_data
    # downstream: UI table display
    output$sites_table <- renderRHandsontable({
      rhandsontable(
        moduleState$sites_data,
        stretchH = "all",
        height = 500,
        selectCallback = TRUE,
        width = NULL,
      ) |>
        hot_table(overflow = "visible", stretchH = "all", width = NULL) |>
        hot_col("SITE_CODE", renderer = mandatory_highlight_text()) |>
        hot_cell(
          1,
          1,
          comment = 'Site Code: A short, unique code identifying identify the site.'
        ) |>
        hot_col("SITE_NAME", renderer = mandatory_highlight_text()) |>
        hot_cell(
          1,
          2,
          comment = 'Site Name: A longer site name identifying the site.'
        ) |>
        hot_col(
          "SITE_GEOGRAPHIC_FEATURE",
          type = "dropdown",
          source = geographic_features_vocabulary(),
          strict = TRUE,
          renderer = mandatory_highlight_dropdown()
        ) |>
        hot_cell(
          1,
          3,
          comment = 'Site Geographical Feature: The geographical category of the site.'
        ) |>
        hot_col(
          "SITE_GEOGRAPHIC_FEATURE_SUB",
          type = "dropdown",
          source = geographic_features_sub_vocabulary(),
          strict = TRUE,
          renderer = mandatory_highlight_dropdown()
        ) |>
        hot_cell(
          1,
          4,
          comment = 'Site Geographical Sub-Feature: The geographical sub-category of the site.'
        ) |>
        hot_col(
          "SITE_COORDINATE_SYSTEM",
          type = "dropdown",
          source = coordinate_systems_vocabulary(),
          strict = TRUE,
          renderer = mandatory_highlight_dropdown()
        ) |>
        hot_cell(
          1,
          5,
          comment = 'Coordinate System: The Coordinate Reference System (CRS) associated with the site longitude and latitude.'
        ) |>
        hot_col(
          "LATITUDE",
          type = "numeric",
          format = "0.000000",
          renderer = mandatory_highlight_text()
        ) |>
        hot_cell(
          1,
          6,
          comment = "Latitude: The site's latitude (northing, y axis, in decimal degrees)"
        ) |>
        hot_col(
          "LONGITUDE",
          type = "numeric",
          format = "0.000000",
          allowInvalid = FALSE,
          renderer = mandatory_highlight_text()
        ) |>
        hot_cell(
          1,
          7,
          comment = "Longitude: The site's longitude (easting, x axis, in decimal degrees)"
        ) |>
        hot_col(
          "COUNTRY",
          type = "dropdown",
          source = countries_vocabulary(),
          strict = TRUE
        ) |>
        hot_cell(
          1,
          8,
          comment = 'Country: The country where the site was sampled.'
        ) |>
        hot_col(
          "AREA",
          type = "dropdown",
          source = areas_vocabulary(),
          strict = TRUE
        ) |>
        hot_cell(
          1,
          9,
          comment = 'Area: The region where the site was sampled.'
        ) |>
        hot_col(
          "ALTITUDE_VALUE",
          type = "numeric",
          allowInvalid = FALSE,
          renderer = mandatory_highlight_text()
        ) |>
        hot_cell(
          1,
          10,
          comment = "Altitude: The sampling site's altitude above or below sea level."
        ) |>
        hot_col(
          "ALTITUDE_UNIT",
          type = "dropdown",
          source = altitude_units_vocabulary(),
          strict = TRUE
        ) |>
        hot_cell(
          1,
          11,
          comment = "Altitude Unit: The unit associated with the site's reported altitude."
        ) |>
        hot_col(
          "ENTERED_DATE",
          type = "date",
          dateFormat = "YYYY-MM-DD",
          allowInvalid = FALSE,
          renderer = mandatory_highlight_text()
        ) |>
        hot_cell(
          1,
          12,
          comment = "Entered Data: The date this site is added to the database."
        ) |>
        hot_col(
          "ENTERED_BY",
          type = "text",
          renderer = mandatory_highlight_text()
        ) |>
        hot_cell(
          1,
          13,
          comment = "Entered By: Your name or initials (autofilled from Campaign if available)."
        ) |>
        hot_col(
          "SITE_COMMENT",
          type = "text",
        ) |>
        hot_cell(
          1,
          14,
          comment = "Site Comment: Any additional comments or relevant details about the site."
        ) |>
        hot_context_menu(
          allowRowEdit = TRUE, # Enable row operations
          allowColEdit = FALSE, # Disable column operations
          customOpts = list(
            # Only include remove_row in the menu
            "row_above" = NULL,
            "row_below" = NULL,
            "remove_row" = list(
              name = "Remove selected rows"
            )
          )
        )
    })

    ## output: sites_map ----
    # upstream: moduleState$sites_data
    # downstream: UI map display
    output$sites_map <- renderLeaflet({
      map <- leaflet() |>
        addTiles() |>
        mapOptions(zoomToLimits = "always") # don't adjust zoom level when we add a point

      # Add markers for sites with valid coordinates
      if (nrow(moduleState$sites_data) > 0) {
        # Convert to numeric, handling potential character values
        lat_numeric <- suppressWarnings(as.numeric(
          moduleState$sites_data$LATITUDE
        ))
        lng_numeric <- suppressWarnings(as.numeric(
          moduleState$sites_data$LONGITUDE
        ))

        # Check for valid coordinates
        valid_coords <- !is.na(lat_numeric) &
          !is.na(lng_numeric) &
          is.finite(lat_numeric) &
          is.finite(lng_numeric) &
          lat_numeric >= -90 &
          lat_numeric <= 90 & # Valid latitude range
          lng_numeric >= -180 &
          lng_numeric <= 180 # Valid longitude range

        if (any(valid_coords)) {
          valid_sites <- moduleState$sites_data[valid_coords, ]
          valid_lat <- lat_numeric[valid_coords]
          valid_lng <- lng_numeric[valid_coords]

          map <- map |>
            addCircleMarkers(
              radius = 3,
              color = "black",
              fillColor = "blue",
              fillOpacity = 0.8,
              stroke = TRUE,
              weight = 2,
              lng = valid_lng,
              lat = valid_lat,
              label = valid_sites$SITE_CODE,
              labelOptions = labelOptions(
                noHide = moduleState$show_labels, # Change this line
                textOnly = FALSE,
                textsize = "12px"
              )
            )
        }
      }
      map
    })

    ## output: selected_coords ----
    # upstream: moduleState$clicked_coords
    # downstream: UI coordinates display
    output$selected_coords <- renderText({
      if (is.null(moduleState$clicked_coords)) {
        "Click to select coordinates"
      } else {
        paste(
          "Lat:",
          round(moduleState$clicked_coords$lat, 6),
          "Lng:",
          round(moduleState$clicked_coords$lng, 6),
          "(WGS84, 6 s.f.)"
        )
      }
    })

    ## output: selected_rows_reporter ----
    # upstream: moduleState$selected_rows
    # downstream: UI selected rows display
    output$selected_rows_reporter <- renderText({
      if (
        is.null(moduleState$selected_rows) ||
          length(moduleState$selected_rows) == 0
      ) {
        "No rows selected"
      } else {
        paste0(
          length(moduleState$selected_rows),
          " row(s) selected: ",
          paste(moduleState$selected_rows, collapse = ", ")
        )
      }
    })

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      llm_indicator <- if (
        session$userData$reactiveValues$llmExtractionComplete
      ) {
        div(
          bs_icon("cpu"),
          "Some data populated from LLM extraction - review for accuracy",
          class = "validation-status validation-llm",
          style = "margin-bottom: 10px;"
        )
      } else {
        NULL
      }

      validation_status <- if (moduleState$is_valid) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All site data validated successfully.",
            nrow(moduleState$sites_data),
            "site(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Add at least one complete, valid site to proceed. Edit fields directly in the table below.",
          class = "validation-status validation-warning"
        )
      }

      div(llm_indicator, validation_status, class = "validation-container")
    })

    ## output: validated_data_display ----
    # upstream: moduleState$validated_data
    # downstream: UI data display
    output$validated_data_display <- renderText({
      if (isTruthy(moduleState$validated_data)) {
        # Format each site as a separate entry
        site_entries <- lapply(1:nrow(moduleState$validated_data), function(i) {
          site <- moduleState$validated_data[i, ]
          site_lines <- sapply(names(site), function(name) {
            value <- site[[name]]
            if (is.na(value) || is.null(value) || value == "") {
              paste0("  ", name, " = NA")
            } else if (is.character(value)) {
              paste0("  ", name, " = '", value, "'")
            } else {
              paste0("  ", name, " = ", as.character(value))
            }
          })
          paste0("Site ", i, ":\n", paste(site_lines, collapse = "\n"))
        })

        paste(site_entries, collapse = "\n\n")
      } else {
        "# Sites data will appear here when valid sites are added"
      }
    })

    # 5. Return ----
    ## return: validated data for other modules ----
    # upstream: moduleState$validated_data
    # downstream: app_server.R
    return(
      reactive({
        moduleState$validated_data %|truthy|% NULL
      })
    )
  })
}

## To be copied in the UI ----
# mod_sites_ui("sites_1")

## To be copied in the server ----
# sites_data <- mod_sites_server("sites_1")
