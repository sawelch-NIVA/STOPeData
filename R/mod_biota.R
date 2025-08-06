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
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_header card_body accordion accordion_panel tooltip
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
      card_header("Biota Sample Details"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Biota Information",
            icon = bs_icon("info-circle"),
            div(
              p(
                "This module captures detailed biological information for samples from living organisms. All biota samples require species identification, tissue type, life stage, and gender information to be considered valid."
              )
            )
          )
        ),

        ## Biota table ----
        div(
          style = "margin: 15px 0;",
          h5("Biota Sample Details"),
          p(
            "Complete the biological details for each biota sample below. All fields are required.",
            class = "text-muted"
          )
        ),

        rHandsontableOutput(
          ns("biota_table"),
          width = "100%",
          height = "100%"
        ),

        ## Validation status ----
        div(
          style = "margin-top: 15px;",
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
    )
  )
}

#' Biota Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
#' @importFrom glue glue
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
      has_biota_samples = FALSE
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

    ## Helper: Create species mapping ----
    create_species_mapping <- function() {
      c(
        # Worms
        "Eisenia fetida",
        "Lumbricus terrestris",
        "Enchytraeus albidus",

        # Insects/Spiders
        "Chironomus riparius",
        "Daphnia magna",
        "Folsomia candida",

        # Molluscs
        "Mytilus edulis",
        "Dreissena polymorpha",
        "Potamopyrgus antipodarum",

        # Fungi
        "Saccharomyces cerevisiae",
        "Penicillium chrysogenum",
        "Aspergillus niger",

        # Crustaceans
        "Gammarus pulex",
        "Hyalella azteca",
        "Artemia salina",

        # Mammals
        "Rattus norvegicus",
        "Mus musculus",
        "Bos taurus",

        # Amphibians
        "Xenopus laevis",
        "Rana temporaria",
        "Bufo bufo",

        # Moss, Hornworts
        "Bryum argenteum",
        "Rhynchostegium murale",
        "Marchantia polymorpha",

        # Birds
        "Gallus gallus",
        "Passer domesticus",
        "Turdus merula",

        # Fish
        "Danio rerio",
        "Oncorhynchus mykiss",
        "Pimephales promelas",

        # Flowers, Trees, Shrubs, Ferns
        "Lemna minor",
        "Arabidopsis thaliana",
        "Zea mays",

        # Algae
        "Pseudokirchneriella subcapitata",
        "Chlorella vulgaris",
        "Scenedesmus quadricauda",

        # Invertebrates
        "Caenorhabditis elegans",
        "Hydra vulgaris",
        "Planaria torva",

        # Reptiles
        "Trachemys scripta",
        "Lacerta agilis",
        "Natrix natrix",

        # Bacteria
        "Escherichia coli",
        "Bacillus subtilis",
        "Pseudomonas fluorescens",

        # Ecosystem
        "Mixed community",
        "Biofilm",
        "Microbial consortium",

        # Other
        "Unknown species",
        "Mixed species",
        "Unidentified organism"
      )
    }
    ## Helper: Get species choices based on group ----
    # TODO: Can this even work for rhandsontable? maybe?
    get_species_choices <- function(species_group) {
      species_mapping <- create_species_mapping()
      if (species_group %in% names(species_mapping)) {
        return(species_mapping[[species_group]])
      } else {
        return(c("Unknown species"))
      }
    }

    #stupid
    create_taxonomic_groups <- function() {
      c(
        "Worms",
        "Insects/Spiders",
        "Molluscs",
        "Fungi",
        "Crustaceans",
        "Mammals",
        "Amphibians",
        "Moss, Hornworts",
        "Birds",
        "Fish",
        "Flowers, Trees, Shrubs, Ferns",
        "Algae",
        "Invertebrates",
        "Reptiles",
        "Bacteria",
        "Ecosystem",
        "Other"
      )
    }

    ## Helper: Initialize biota data frame
    init_biota_df <- function() {
      data.frame(
        SAMPLE_ID = character(0),
        SITE_CODE = character(0),
        PARAMETER_NAME = character(0),
        ENVIRON_COMPARTMENT = character(0), # Changed from COMPARTMENT
        ENVIRON_COMPARTMENT_SUB = character(0), # Added
        MEASURED_CATEGORY = character(0), # Added
        SAMPLING_DATE = character(0),
        REPLICATE = integer(0),
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

      # Look for biota samples using the new ENVIRON_COMPARTMENT column
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

      # Select columns in the expected order - UPDATED for new structure
      expected_columns <- c(
        "SAMPLE_ID",
        "SITE_CODE",
        "PARAMETER_NAME",
        "ENVIRON_COMPARTMENT",
        "ENVIRON_COMPARTMENT_SUB",
        "MEASURED_CATEGORY",
        "SAMPLING_DATE",
        "REPLICATE",
        biota_columns
      )

      # Only select columns that exist
      available_columns <- intersect(expected_columns, names(biota_samples))
      return(biota_samples[, available_columns, drop = FALSE])
    }

    # 3. Observers and Reactives ----

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

    # 4. Outputs ----

    ## output: biota_table ----
    # upstream: moduleState$biota_data
    # downstream: UI table display
    output$biota_table <- renderRHandsontable({
      if (!moduleState$has_biota_samples || nrow(moduleState$biota_data) == 0) {
        # Show message when no biota samples
        empty_df <- data.frame(
          Message = "No biota samples found. Add samples with ENVIRON_COMPARTMENT = 'Biota' in the Samples module.",
          stringsAsFactors = FALSE
        )

        rhandsontable(
          empty_df,
          stretchH = "all",
          height = NULL,
          readOnly = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all")
      } else {
        # Create controlled vocabulary for biota fields
        species_groups <- names(create_species_mapping())
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
        genders <- c(
          "Male",
          "Female",
          "Mixed",
          "Hermaphrodite",
          "Not applicable",
          "Not determined",
          "Other"
        )

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
          hot_col("REPLICATE", readOnly = TRUE) |>
          # Add dropdowns for biota-specific fields
          hot_col(
            "SPECIES_GROUP",
            type = "dropdown",
            source = create_taxonomic_groups(),
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
          # TODO: Species should be updated dynamically based on species group selection
          hot_col(
            "SAMPLE_SPECIES",
            type = "dropdown",
            source = create_species_mapping(),
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
    # upstream: moduleState$is_valid, moduleState$has_biota_samples
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      if (!moduleState$has_biota_samples) {
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
# mod_biota_ui("biota_1")

## To be copied in the server ----
# mod_biota_server("biota_1")
