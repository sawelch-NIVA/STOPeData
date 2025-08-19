# Biota Data Module ----
# A Shiny module for entering biota-specific sample details

#' Biota UI Function ----
#'
#' @description A shiny Module for biota data entry with species, tissue, and other biological details.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput actionButton
#' @importFrom bslib card card_body accordion accordion_panel tooltip layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom golem print_dev
#' @export
mod_biota_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(
          content_file = "inst/app/www/md/intro_biota.md"
        ),

        ## Species selection controls ----
        div(
          style = "margin: 15px 0; padding: 15px;",
          h5("Study Species Selection"),
          p(
            "First select a species group, then choose specific species for your study. Selected species will be available in the sample table below.",
            class = "text-muted"
          ),

          layout_columns(
            col_widths = c(4, 8),
            selectizeInput(
              ns("species_group_filter"),
              "Filter by Species Group:",
              choices = c(
                "Worms",
                "Insects/Spiders",
                "Molluscs",
                "Fungi",
                "Crustaceans",
                "Mammals",
                "Amphibians",
                "Moss/Hornworts",
                "Birds",
                "Fish",
                "Plants",
                "Algae",
                "Invertebrates",
                "Reptiles",
                "Bacteria",
                "Ecosystem",
                "Other"
              ),
              selected = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              ns("study_species_selector"),
              "Select Species for Study:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            )
          ),

          ## Selected species display and clear button ----
          div(
            style = "margin-top: 10px;",
            layout_columns(
              col_widths = c(10, 2),
              div(
                h6("Currently Selected Species:"),
                verbatimTextOutput(
                  ns("selected_species_display"),
                  placeholder = TRUE
                )
              ),
              div(
                style = "display: flex; align-items: end; margin-top: calc(1em + 10px);",
                actionButton(
                  ns("clear_species"),
                  "Clear All",
                  class = "btn-danger btn-sm",
                  style = "margin-bottom: 15px;"
                )
              )
            )
          )
        ),

        ## Validation status ----
        div(
          style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 15px 0;",

          ### Validation status ----
          uiOutput(ns("validation_reporter"))
        ),

        ## Raw data accordion ----
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Click to view raw validated biota data",
            icon = bs_icon("code"),
            verbatimTextOutput(ns("validated_data_display"))
          )
        )
      )
    ),

    ## Biota table card ----
    card(
      div(
        rHandsontableOutput(
          ns("biota_table"),
          width = "100%",
          height = "100%"
        ),
        style = "margin-bottom: 10px;"
      )
    )
  )
}

#' Biota Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectizeInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom dplyr pull
#' @export
mod_biota_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      biota_data = data.frame(),
      validated_data = NULL,
      is_valid = FALSE,
      has_biota_samples = FALSE,
      species_options = data.frame(),
      study_species = character(0)
    )

    moduleState$species_options <- readr::read_csv(
      "inst/data/clean/dummy_species.csv",
      show_col_types = FALSE
    )

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    iv$add_rule("biota_table_validation", function(value) {
      if (!moduleState$has_biota_samples) {
        return(NULL) # No biota samples, validation passes
      }

      if (nrow(moduleState$biota_data) == 0) {
        return("Biota samples detected but no biota data available")
      }

      # Check required biota fields
      required_biota_fields <- c(
        "SPECIES_GROUP",
        "SAMPLE_SPECIES",
        "SAMPLE_TISSUE",
        "SAMPLE_SPECIES_LIFESTAGE",
        "SAMPLE_SPECIES_GENDER"
      )

      for (i in 1:nrow(moduleState$biota_data)) {
        for (field in required_biota_fields) {
          value <- moduleState$biota_data[i, field]
          if (is.na(value) || value == "" || value == "Not reported") {
            return(paste("Row", i, "is missing required biota field:", field))
          }
        }
      }
      NULL # All validations passed
    })
    iv$enable()

    # 2. Helper functions ----

    ## Helper: Initialize biota data frame
    init_biota_df <- function() {
      data.frame(
        SAMPLE_ID = character(0),
        SITE_CODE = character(0),
        PARAMETER_NAME = character(0),
        ENVIRON_COMPARTMENT = character(0),
        ENVIRON_COMPARTMENT_SUB = character(0),
        MEASURED_CATEGORY = character(0),
        SAMPLING_DATE = character(0),
        REP = integer(0),
        SPECIES_GROUP = character(0),
        SAMPLE_SPECIES = character(0),
        SAMPLE_TISSUE = character(0),
        SAMPLE_SPECIES_LIFESTAGE = character(0),
        SAMPLE_SPECIES_GENDER = character(0),
        stringsAsFactors = FALSE
      )
    }

    ## Helper: Filter biota samples
    extract_biota_samples <- function(samples_data) {
      if (is.null(samples_data) || nrow(samples_data) == 0) {
        return(init_biota_df())
      }

      # Look for biota samples using the ENVIRON_COMPARTMENT column
      biota_samples <- samples_data[
        samples_data$ENVIRON_COMPARTMENT == "Biota" &
          !is.na(samples_data$ENVIRON_COMPARTMENT),
      ]

      if (nrow(biota_samples) == 0) {
        return(init_biota_df())
      }

      # Add biota-specific columns if they don't exist
      biota_columns <- c(
        "SPECIES_GROUP",
        "SAMPLE_SPECIES",
        "SAMPLE_TISSUE",
        "SAMPLE_SPECIES_LIFESTAGE",
        "SAMPLE_SPECIES_GENDER"
      )

      for (col in biota_columns) {
        if (!col %in% names(biota_samples)) {
          biota_samples[[col]] <- "Not reported"
        }
      }

      # Select columns in the expected order
      expected_columns <- c(
        "SAMPLE_ID",
        "SITE_CODE",
        "PARAMETER_NAME",
        "ENVIRON_COMPARTMENT",
        "ENVIRON_COMPARTMENT_SUB",
        "MEASURED_CATEGORY",
        "SAMPLING_DATE",
        "REP",
        biota_columns
      )

      # Only select columns that exist
      available_columns <- intersect(expected_columns, names(biota_samples))
      return(biota_samples[, available_columns, drop = FALSE])
    }

    ## Helper: Merge biota data back into samples
    merge_biota_into_samples <- function(samples_data, biota_data) {
      # Add biota columns to samples data if they don't exist
      biota_columns <- c(
        "SPECIES_GROUP",
        "SAMPLE_SPECIES",
        "SAMPLE_TISSUE",
        "SAMPLE_SPECIES_LIFESTAGE",
        "SAMPLE_SPECIES_GENDER"
      )

      for (col in biota_columns) {
        if (!col %in% names(samples_data)) {
          samples_data[[col]] <- NA
        }
      }

      # Update biota rows with validated data
      for (i in 1:nrow(biota_data)) {
        sample_id <- biota_data$SAMPLE_ID[i]
        row_idx <- which(samples_data$SAMPLE_ID == sample_id)

        if (length(row_idx) > 0) {
          for (col in biota_columns) {
            samples_data[row_idx, col] <- biota_data[i, col]
          }
        }
      }

      return(samples_data)
    }

    # 3. Observers and Reactives ----

    ## observe ~ bindEvent(input$species_group_filter): Update species choices based on group ----
    # upstream: input$species_group_filter, moduleState$species_options
    # downstream: input$study_species_selector choices
    observe({
      # Filter species by selected group
      filtered_species <- moduleState$species_options[
        moduleState$species_options$SPECIES_GROUP == input$species_group_filter,
        "SPECIES_NAME"
      ] |>
        pull(SPECIES_NAME)

      # Update species selector with filtered choices, keeping current selections
      current_selected <- input$study_species_selector %||% character(0)

      updateSelectizeInput(
        session,
        "study_species_selector",
        choices = sort(filtered_species),
        selected = intersect(current_selected, filtered_species)
      )

      print_dev(glue(
        "mod_biota filtered to {length(filtered_species)} species for group: {input$species_group_filter}"
      ))
    }) |>
      bindEvent(input$species_group_filter)

    ## observe ~ bindEvent(input$study_species_selector): Update study species list ----
    # upstream: input$study_species_selector
    # downstream: moduleState$study_species
    observe({
      if (!is.null(input$study_species_selector)) {
        current_group_filter <- input$species_group_filter

        if (!is.null(current_group_filter)) {
          # Get species from the current group that are in the CSV
          current_group_species <- moduleState$species_options[
            moduleState$species_options$SPECIES_GROUP == current_group_filter,
            "SPECIES_NAME"
          ] |>
            pull(SPECIES_NAME)

          # Remove any previously selected species from current group
          moduleState$study_species <- moduleState$study_species[
            !moduleState$study_species %in% current_group_species
          ]

          # Add currently selected species from this group
          moduleState$study_species <- unique(c(
            moduleState$study_species,
            input$study_species_selector
          ))
        }
      }
    }) |>
      bindEvent(input$study_species_selector)

    ## observe ~ bindEvent(input$clear_species): Clear all selected species ----
    # upstream: input$clear_species
    # downstream: moduleState$study_species, input$study_species_selector
    observe({
      updateSelectizeInput(
        session,
        "study_species_selector",
        selected = character(0)
      )
      moduleState$study_species <- character(0)
      print_dev("mod_biota: Cleared all selected species")
    }) |>
      bindEvent(input$clear_species)

    ## observe ~ bindEvent(session$userData$reactiveValues$sampleData): Load biota samples from session data ----
    # upstream: session$userData$reactiveValues$sampleData
    # downstream: moduleState$biota_data, moduleState$has_biota_samples
    observe({
      if (!is.null(session$userData$reactiveValues$sampleData)) {
        samples_data <- session$userData$reactiveValues$sampleData
        biota_samples <- extract_biota_samples(samples_data)

        moduleState$has_biota_samples <- nrow(biota_samples) > 0

        if (moduleState$has_biota_samples) {
          # Only update if we don't already have biota data or if structure changed
          if (
            nrow(moduleState$biota_data) == 0 ||
              !identical(
                biota_samples$SAMPLE_ID,
                moduleState$biota_data$SAMPLE_ID
              )
          ) {
            moduleState$biota_data <- biota_samples
            print_dev(glue(
              "mod_biota loaded {nrow(biota_samples)} biota samples"
            ))
          }
        } else {
          moduleState$biota_data <- init_biota_df()
          print_dev("mod_biota: No biota samples found")
        }
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$sampleData,
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )

    ## observe: Handle table changes ----
    # upstream: input$biota_table changes
    # downstream: moduleState$biota_data
    observe({
      if (!is.null(input$biota_table) && moduleState$has_biota_samples) {
        updated_data <- hot_to_r(input$biota_table)
        moduleState$biota_data <- updated_data
      }
    }) |>
      bindEvent(input$biota_table)

    ## observe: Check validation and update session data ----
    # upstream: moduleState$biota_data, iv
    # downstream: moduleState$is_valid, moduleState$validated_data, session$userData
    observe({
      validation_result <- iv$is_valid()

      if (!moduleState$has_biota_samples) {
        # No biota samples - validation passes by default
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- NULL
        session$userData$reactiveValues$biotaValidated <- TRUE
        session$userData$reactiveValues$biotaData <- NULL
        print_dev("mod_biota: No biota samples, validation passes")
      } else if (validation_result && nrow(moduleState$biota_data) > 0) {
        # Biota samples exist and are valid
        moduleState$is_valid <- TRUE
        moduleState$validated_data <- moduleState$biota_data

        # Merge biota data back into main samples data
        if (!is.null(session$userData$reactiveValues$sampleData)) {
          updated_samples <- merge_biota_into_samples(
            session$userData$reactiveValues$sampleData,
            moduleState$validated_data
          )
          session$userData$reactiveValues$sampleDataWithBiota <- updated_samples
          session$userData$reactiveValues$biotaValidated <- TRUE
          session$userData$reactiveValues$biotaData <- moduleState$validated_data
          print_dev(glue(
            "mod_biota validated and merged {nrow(moduleState$validated_data)} biota samples"
          ))
        }
      } else {
        # Biota samples exist but validation failed
        moduleState$is_valid <- FALSE
        moduleState$validated_data <- NULL
        session$userData$reactiveValues$biotaValidated <- FALSE
        session$userData$reactiveValues$biotaData <- NULL
        print_dev("mod_biota: Validation failed")
      }
    })

    ## observe ~ bindEvent: Load biota data from LLM extraction ----
    observe({
      if (!is.null(session$userData$reactiveValues$biotaDataLLM)) {
        llm_biota_data <- session$userData$reactiveValues$biotaDataLLM

        # replace existing data
        moduleState$biota_data <-
          llm_biota_data

        print_dev(glue(
          "mod_biota loaded {nrow(llm_biota_data)} entries from LLM"
        ))
      }
    }) |>
      bindEvent(session$userData$reactiveValues$biotaDataLLM, ignoreNULL = TRUE)

    # 4. Outputs ----

    ## output: selected_species_display ----
    # upstream: moduleState$study_species
    # downstream: UI display of selected species
    output$selected_species_display <- renderText({
      if (length(moduleState$study_species) == 0) {
        "No species selected for study"
      } else {
        species_count <- length(moduleState$study_species)
        if (species_count <= 10) {
          paste(moduleState$study_species, collapse = ", ")
        } else {
          paste0(
            paste(moduleState$study_species[1:8], collapse = ", "),
            ", ... and ",
            species_count - 8,
            " more species"
          )
        }
      }
    })

    ## output: biota_table ----
    # upstream: moduleState$biota_data, moduleState$study_species
    # downstream: UI table display
    output$biota_table <- renderRHandsontable({
      if (!moduleState$has_biota_samples || nrow(moduleState$biota_data) == 0) {
        # Show message when no biota samples
        rhandsontable(
          init_biota_df(),
          readOnly = TRUE,
        )
      } else {
        # Create controlled vocabulary for biota fields

        # Use only study-selected species for the dropdown
        available_species <- if (length(moduleState$study_species) > 0) {
          moduleState$study_species
        } else {
          c("Select species above first")
        }

        # ! FORMAT-BASED
        tissue_types <- c(
          "Whole organism",
          "Muscle",
          "Liver",
          "Kidney",
          "Brain",
          "Heart",
          "Lung",
          "Gill",
          "Shell",
          "Carapace",
          "Blood",
          "Egg",
          "Larva",
          "Leaf",
          "Root",
          "Stem",
          "Fruit",
          "Seed",
          "Other"
        )

        # ! FORMAT-BASED
        life_stages <- c(
          "Adult",
          "Juvenile",
          "Larva",
          "Embryo",
          "Egg",
          "Seedling",
          "Mature",
          "Young",
          "Mixed",
          "Not applicable",
          "Other"
        )

        # ! FORMAT-BASED
        genders <- c(
          "Male",
          "Female",
          "Mixed",
          "Hermaphrodite",
          "Not applicable",
          "Not determined",
          "Other"
        )

        # Get species groups from loaded data
        species_groups <- sort(unique(
          moduleState$species_options$SPECIES_GROUP
        ))

        rhandsontable(
          moduleState$biota_data,
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
          # Make sample info columns read-only
          hot_col("SAMPLE_ID", readOnly = TRUE) |>
          hot_col("SITE_CODE", readOnly = TRUE) |>
          hot_col("PARAMETER_NAME", readOnly = TRUE) |>
          hot_col("ENVIRON_COMPARTMENT", readOnly = TRUE) |>
          hot_col("ENVIRON_COMPARTMENT_SUB", readOnly = TRUE) |>
          hot_col("MEASURED_CATEGORY", readOnly = TRUE) |>
          hot_col("SAMPLING_DATE", readOnly = TRUE) |>
          hot_col("REP", readOnly = TRUE) |>
          # Add dropdowns for biota-specific fields
          hot_col(
            "SPECIES_GROUP",
            type = "dropdown",
            source = species_groups,
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(
            "SAMPLE_SPECIES",
            type = "dropdown",
            source = available_species,
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(
            "SAMPLE_TISSUE",
            type = "dropdown",
            source = tissue_types,
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(
            "SAMPLE_SPECIES_LIFESTAGE",
            type = "dropdown",
            source = life_stages,
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_col(
            "SAMPLE_SPECIES_GENDER",
            type = "dropdown",
            source = genders,
            strict = TRUE,
            renderer = mandatory_highlight_dropdown()
          ) |>
          hot_context_menu(
            allowRowEdit = FALSE, # Don't allow adding/removing rows
            allowColEdit = FALSE
          )
      }
    })

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid, moduleState$has_biota_samples, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      llm_indicator <- if (
        session$userData$reactiveValues$llmExtractionComplete
      ) {
        div(
          bs_icon("cpu"),
          "Some data populated from LLM extraction - please review for accuracy",
          class = "validation-status validation-info",
          style = "margin-bottom: 10px;"
        )
      } else {
        NULL
      }

      validation_status <- if (!moduleState$has_biota_samples) {
        div(
          bs_icon("info-circle"),
          "No biota samples detected. Biota validation not required.",
          class = "validation-status validation-complete"
        )
      } else if (moduleState$is_valid) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All biota data validated successfully.",
            nrow(moduleState$biota_data),
            "biota sample(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Complete all required biota fields. All biota samples must have species, tissue, life stage, and gender information.",
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
        # Format first few biota samples as examples
        sample_count <- nrow(moduleState$validated_data)
        display_count <- min(3, sample_count)

        sample_entries <- lapply(1:display_count, function(i) {
          sample <- moduleState$validated_data[i, ]
          # Focus on biota-specific fields
          biota_fields <- c(
            "SAMPLE_ID",
            "SPECIES_GROUP",
            "SAMPLE_SPECIES",
            "SAMPLE_TISSUE",
            "SAMPLE_SPECIES_LIFESTAGE",
            "SAMPLE_SPECIES_GENDER"
          )
          sample_lines <- sapply(biota_fields, function(name) {
            if (name %in% names(sample)) {
              value <- sample[[name]]
              if (is.na(value) || is.null(value) || value == "") {
                paste0("  ", name, " = NA")
              } else if (is.character(value)) {
                paste0("  ", name, " = '", value, "'")
              } else {
                paste0("  ", name, " = ", as.character(value))
              }
            } else {
              paste0("  ", name, " = [column not found]")
            }
          })
          paste0(
            "Biota Sample ",
            i,
            ":\n",
            paste(sample_lines, collapse = "\n")
          )
        })

        result <- paste(sample_entries, collapse = "\n\n")
        if (sample_count > display_count) {
          result <- paste0(
            result,
            "\n\n# ... and ",
            sample_count - display_count,
            " more biota samples"
          )
        }

        return(result)
      } else if (moduleState$has_biota_samples) {
        "# Biota sample data will appear here when validated"
      } else {
        "# No biota samples detected"
      }
    })
  })
}

## To be copied in the UI ----
# mod_biota_ui("biota_1")

## To be copied in the server ----
# mod_biota_server("biota_1")
