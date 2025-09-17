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
        full_screen = FALSE,
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
          leafletOutput(ns("sites_map")),

          ### Map controls ----
          layout_column_wrap(
            width = 1 / 2,

            # Add point button
            actionButton(
              inputId = ns("add_map_point"),
              label = "Add Point as New Row",
              icon = icon("map-pin"),
              class = "btn-primary btn-sm",
              width = "300px"
            ),

            # Selected coordinates reporter
            div(
              style = "display: flex; align-items: center; font-size: 0.9em; margin-top: 5px; margin-left: 5px;",
              textOutput(ns("selected_coords"))
            )
          )
        )
      )
    ),

    ### Bottom card: Sites table ----
    # card(
    #   full_screen = TRUE,
    #   card_body(
    #     max_height_full_screen = "80vh",
    card(
      div(
        rHandsontableOutput(ns("sites_table")),
        style = "margin-bottom: 10px;"
      )
    )
    #   )
    # )
  )
}

#' Sites Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu hot_table hot_cell hot_validate_numeric hot_validate_character
#' @importFrom shinyjs enable disable
#' @importFrom leaflet renderLeaflet leaflet addTiles addMarkers clearMarkers setView leafletProxy
#' @import ISOcodes
#' @importFrom dplyr pull
#' @export
mod_sites_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      sites_data = data.frame(),
      validated_data = NULL,
      is_valid = FALSE,
      selected_rows = NULL,
      next_site_id = 1,
      clicked_coords = NULL
    )

    ## Controlled vocabulary options ----
    geographic_features <- c(
      "Not relevant",
      "Not reported",
      "River, stream, canal",
      "Lake, pond, pool, reservoir",
      "Ocean, sea, territorial waters",
      "Coastal, fjord",
      "Estuary",
      "Drainage, sewer, artificial water",
      "Swamp, wetland",
      "Groundwater, aquifer",
      "WWTP",
      "Artificial Land/Urban Areas",
      "Landfills",
      "Cropland",
      "Woodland, forest",
      "Shrubland",
      "Grassland",
      "Bare land and lichen/moss",
      "Glacier",
      "Other"
    )

    geographic_features_sub <- c(
      "Not relevant",
      "Not reported",
      "Water surface",
      "Water column, pelagic zone",
      "Water benthos",
      "Other"
    )

    coordinate_systems <- c(
      "Not relevant",
      "Not reported",
      "WGS 84",
      "UTM 32",
      "UTM 33",
      "UTM 34",
      "UTM 35",
      "ETRS89",
      "Other"
    )

    countries <- c(
      "Not relevant",
      "Not reported",
      "Other/Not a Country",
      ISOcodes::ISO_3166_1$Name
    )

    IHO_oceans <- readRDS("inst/data/clean/IHO_oceans.rds") |> pull(NAME)

    areas <- c(
      "Not relevant",
      "Not reported",
      "Other",
      IHO_oceans
    )

    altitude_units <- c("km", "m", "cm", "mm")

    ## Initialize empty sites data frame ----
    init_sites_df <- function() {
      data.frame(
        SITE_CODE = character(0),
        SITE_NAME = character(0),
        SITE_GEOGRAPHIC_FEATURE = character(0),
        SITE_GEOGRAPHIC_FEATURE_SUB = character(0),
        COUNTRY = character(0),
        AREA = character(0),
        LATITUDE = numeric(0),
        LONGITUDE = numeric(0),
        SITE_COORDINATE_SYSTEM = character(0),
        ALTITUDE_VALUE = numeric(0),
        ALTITUDE_UNIT = character(0),
        ENTERED_BY = character(0),
        ENTERED_DATE = character(0),
        SITE_COMMENT = character(0),
        stringsAsFactors = FALSE
      )
    }

    ## Set initial empty data frame ----
    moduleState$sites_data <- init_sites_df()

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
    create_new_site <- function(site_number = 1, base_code = "") {
      # Generate site code
      if (base_code == "" || is.null(base_code)) {
        site_code <- paste0("SITE_", sprintf("%03d", site_number))
      } else {
        site_code <- paste0(base_code, sprintf("%03d", site_number))
      }

      data.frame(
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
        SITE_COMMENT = "",
        stringsAsFactors = FALSE
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Add new site(s) ----
    # upstream: user clicks input$add_site
    # downstream: moduleState$sites_data
    observe({
      num_sites <- input$num_sites
      base_code <- input$base_site_code

      # Create multiple sites
      new_sites_list <- lapply(1:num_sites, function(i) {
        create_new_site(site_number = i, base_code = base_code)
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
    })

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
        base_code = input$base_site_code
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

      # Show notification
      showNotification(
        paste(
          "New site added from map coordinates:",
          round(new_site$LATITUDE, 6),
          ",",
          round(new_site$LONGITUDE, 6)
        ),
        type = "message"
      )
    }) |>
      bindEvent(input$add_map_point)

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
        print_dev(glue(
          "moduleState$is_valid: {moduleState$is_valid},
          session$userData$reactiveValues$sitesData: {session$userData$reactiveValues$sitesData}"
        ))
      } else {
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
      }
    })

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
            "sites from LLM extraction. Review coordinates and details."
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
      # if (nrow(moduleState$sites_data) == 0) {
      #   # Show empty table structure
      #   rhandsontable(create_new_site(), stretchH = "all") |>
      #     hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      # } else {
      rhandsontable(
        moduleState$sites_data,
        overflow = "visible",
        height = 500
      ) |>
        hot_table(stretchH = "right") |>
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
          source = geographic_features,
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
          source = geographic_features_sub,
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
          source = coordinate_systems,
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
          source = countries,
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
          source = areas,
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
          source = altitude_units,
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
      # }
    })

    ## output: sites_map ----
    # upstream: moduleState$sites_data, input$map_center_country
    # downstream: UI map display
    output$sites_map <- renderLeaflet({
      map <- leaflet() |>
        addTiles()

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
            addMarkers(
              lng = valid_lng,
              lat = valid_lat,
              popup = paste0(
                "<strong>",
                valid_sites$SITE_CODE,
                "</strong><br/>",
                valid_sites$SITE_NAME,
                "<br/>",
                valid_sites$SITE_GEOGRAPHICAL_FEATURE,
                ":",
                valid_sites$SITE_GEOGRAPHICAL_FEATURE_SUB,
                "<br/>",
                "Lat: ",
                round(valid_lat, 6),
                "<br/>",
                "Lng: ",
                round(valid_lng, 6)
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
        "Click map to select coordinates"
      } else {
        paste0(
          "Selected: (",
          round(moduleState$clicked_coords$lng, 6),
          ", ",
          round(moduleState$clicked_coords$lat, 6),
          ") WGS84"
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
