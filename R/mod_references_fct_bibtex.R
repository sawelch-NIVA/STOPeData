# Function ----
#' Convert BibTeX string to data frame using temporary file
#'
#' @param string A BibTeX formatted string containing one or more entries
#' @param ... Additional arguments passed to \code{\link[bib2df]{bib2df}}
#'
#' @description
#' This function converts a BibTeX formatted string into a data frame by writing
#' the string to a temporary file and then using \code{bib2df::bib2df} to parse it.
#' The temporary file is automatically cleaned up after use.
#'
#' @return A tibble/data frame with parsed BibTeX entries
#'
#' @details
#' This approach uses temporary files to work around the limitation that
#' \code{bib2df::bib2df} only accepts file paths, not strings directly.
#' The temporary file is created in the system temp directory and removed
#' automatically, even if an error occurs.
#'
#' @author Philipp Ottolinger (original function), Sam Welch (added string wrapper)
#'
#' @seealso \code{\link[bib2df]{bib2df}}
#'
#' @examples
#' \dontrun{
#' bibtex_string <- "@article{example2023,
#'   title={Example Article},
#'   author={Smith, John},
#'   year={2023}
#' }"
#'
#' df <- bib_string2df_alt(bibtex_string)
#' print(df)
#' }
#'
#' @export
bib_string2df_alt <- function(string, ...) {
  temp_file <- tempfile(fileext = ".bib")
  on.exit(unlink(temp_file))
  writeLines(string, temp_file)
  bib2df::bib2df(temp_file, ...)
}

# BibTeX Functions ----
# Functions for parsing and mapping BibTeX data to reference fields

#' Map BibTeX fields to reference input fields
#'
#' @description
#' Converts parsed BibTeX data from bib2df format into a list of values
#' that correspond to the reference module input fields. Handles field
#' mapping between BibTeX conventions and the reference data structure.
#'
#' @param bibtex_df A data frame from bib2df containing parsed BibTeX data.
#'   Should contain at least one row of BibTeX entry data.
#' @param access_date Date to use for ACCESS_DATE field. Defaults to today's date.
#'
#' @return A named list containing mapped field values for all reference input fields.
#'   Values are NA for fields not present in the BibTeX entry.
#'
#' @details
#' The function maps BibTeX reference types to the four supported reference types:
#' - article → journal
#' - book → book
#' - techreport/report/manual → report
#' - misc → dataset
#' - Other types (inproceedings, incollection, theses, etc.) are mapped to closest equivalent
#'
#' Field mappings follow standard BibTeX conventions:
#' - JOURNAL/BOOKTITLE → PERIODICAL_JOURNAL
#' - NUMBER → ISSUE
#' - ADDRESS → PUBLISHED_PLACE
#' - SCHOOL → INSTITUTION (for theses)
#' - EDITOR → SERIES_EDITOR
#'
#' @examples
#' \dontrun{
#' # Parse BibTeX string and map to reference fields
#' bibtex_string <- "@article{example2023,
#'   title={Example Article},
#'   author={Smith, John},
#'   journal={Nature},
#'   year={2023},
#'   volume={123},
#'   number={4}
#' }"
#'
#' bibtex_df <- bib_string2df_alt(bibtex_string)
#' mapped_fields <- map_bibtex_to_reference_fields(bibtex_df)
#' }
#'
#' @seealso \code{\link{bib_string2df_alt}}
#'
#' @export
map_bibtex_to_reference_fields <- function(
  bibtex_df,
  access_date = Sys.Date()
) {
  # Input validation
  if (!is.data.frame(bibtex_df) || nrow(bibtex_df) == 0) {
    stop("bibtex_df must be a non-empty data frame")
  }

  # Take first row if multiple entries
  entry <- bibtex_df[1, ]

  # Map BibTeX reference types to our reference types ----
  ref_type_mapping <- list(
    "article" = "journal",
    "book" = "book",
    "techreport" = "report",
    "report" = "report",
    "manual" = "report",
    "misc" = "dataset",
    # Default mappings for unmapped types - TODO: extend definition to cover these types
    "inproceedings" = "journal", # Conference papers → journal
    "conference" = "journal",
    "incollection" = "book", # Book chapters → book
    "inbook" = "book",
    "phdthesis" = "report", # Theses → report
    "mastersthesis" = "report",
    "unpublished" = "report"
  )

  # Get reference type, default to journal if not found
  ref_type <- ref_type_mapping[[tolower(entry$CATEGORY %||% "")]] %||% "journal"

  # Helper function to safely extract and convert values
  safe_extract <- function(value, convert_func = identity, clean_text = TRUE) {
    if (is.null(value) || is.na(value) || value == "") {
      return(NA)
    }

    # Clean BibTeX formatting if it's text
    if (clean_text && is.character(value)) {
      value <- clean_bibtex_text(value)
    }

    tryCatch(convert_func(value), error = function(e) NA)
  }

  # Create field mapping ----
  mapped_fields <- list(
    # Always required fields
    REFERENCE_TYPE = ref_type,
    AUTHOR = safe_extract(entry$AUTHOR),
    TITLE = safe_extract(entry$TITLE),
    YEAR = safe_extract(entry$YEAR, as.numeric),
    ACCESS_DATE = as.Date(access_date),

    # Journal-specific fields
    PERIODICAL_JOURNAL = safe_extract(entry$JOURNAL %||% entry$BOOKTITLE),
    VOLUME = safe_extract(entry$VOLUME, as.numeric),
    ISSUE = safe_extract(entry$NUMBER, as.numeric), # BibTeX uses NUMBER for issue

    # Book-specific fields
    PUBLISHER = safe_extract(entry$PUBLISHER),

    # Report-specific fields
    INSTITUTION = safe_extract(entry$INSTITUTION %||% entry$SCHOOL), # SCHOOL for theses

    # Dataset-specific fields (not typically in BibTeX)
    DB_NAME = NA,
    DB_PROVIDER = NA,

    # Optional fields for all types
    DOI = safe_extract(entry$DOI),
    URL = safe_extract(entry$URL),
    PAGES = safe_extract(entry$PAGES),
    ISBN_ISSN = safe_extract(entry$ISBN %||% entry$ISSN),
    EDITION = safe_extract(entry$EDITION),
    PUBLISHED_PLACE = safe_extract(entry$ADDRESS), # BibTeX uses ADDRESS
    DOCUMENT_NUMBER = safe_extract(entry$NUMBER), # For reports
    ACCESSION_NUMBER = NA, # Not typically in BibTeX
    PMCID = NA, # Not typically in BibTeX
    SERIES_TITLE = safe_extract(entry$SERIES),
    SERIES_EDITOR = safe_extract(entry$EDITOR),
    SERIES_VOLUME = safe_extract(entry$VOLUME, as.numeric),
    NUMBER_OF_PAGES = NA, # Not typically in BibTeX
    NUMBER_OF_VOLUMES = NA, # Not typically in BibTeX
    REF_COMMENT = NA, # Not typically in BibTeX
    ENTERED_BY = NA # Will be filled from session data
  )

  return(mapped_fields)
}

#' Validate and parse BibTeX string with error handling
#'
#' @description
#' Wrapper function that validates BibTeX syntax, parses the string using
#' bib_string2df_alt, and provides detailed error handling with user-friendly
#' error messages.
#'
#' @param bibtex_string Character string containing BibTeX formatted data
#' @param allow_multiple Logical indicating whether to allow multiple entries.
#'   If FALSE (default), will return an error for multiple entries.
#'
#' @return A list with components:
#'   - success: Logical indicating whether parsing succeeded
#'   - data: Parsed data frame (if success = TRUE) or NULL
#'   - message: Success/error message for user feedback
#'   - warning: Additional warning message (if applicable)
#'
#' @examples
#' \dontrun{
#' bibtex_string <- "@article{example2023,
#'   title={Example Article},
#'   author={Smith, John},
#'   year={2023}
#' }"
#'
#' result <- validate_and_parse_bibtex(bibtex_string)
#' if (result$success) {
#'   mapped_fields <- map_bibtex_to_reference_fields(result$data)
#' }
#' }
#'
#' @seealso \code{\link{bib_string2df_alt}}, \code{\link{map_bibtex_to_reference_fields}}
#'
#' @export
validate_and_parse_bibtex <- function(bibtex_string, allow_multiple = FALSE) {
  # Input validation
  if (!is.character(bibtex_string) || length(bibtex_string) != 1) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "BibTeX input must be a single character string",
      warning = NULL
    ))
  }

  bibtex_string <- trimws(bibtex_string)

  if (bibtex_string == "") {
    return(list(
      success = FALSE,
      data = NULL,
      message = "Please paste BibTeX data before importing",
      warning = NULL
    ))
  }

  # Attempt to parse BibTeX
  tryCatch(
    {
      bibtex_df <- bib_string2df_alt(bibtex_string)

      # Check if we have data
      if (nrow(bibtex_df) == 0) {
        return(list(
          success = FALSE,
          data = NULL,
          message = "No valid BibTeX entries found in the provided text",
          warning = NULL
        ))
      }

      # Check for multiple entries
      warning_msg <- NULL
      if (nrow(bibtex_df) > 1) {
        if (!allow_multiple) {
          warning_msg <- paste(
            "Multiple BibTeX entries detected.",
            "Only the first entry will be imported."
          )
        }
      }

      return(list(
        success = TRUE,
        data = bibtex_df,
        message = "BibTeX data parsed successfully",
        warning = warning_msg
      ))
    },
    error = function(e) {
      # Provide user-friendly error message
      error_msg <- "BibTeX parsing failed"

      # Try to provide more specific error information
      if (grepl("unexpected", e$message, ignore.case = TRUE)) {
        error_msg <- paste(
          error_msg,
          "- check for syntax errors in BibTeX format"
        )
      } else if (grepl("file", e$message, ignore.case = TRUE)) {
        error_msg <- paste(error_msg, "- internal file handling error")
      } else {
        error_msg <- paste(error_msg, "-", e$message)
      }

      return(list(
        success = FALSE,
        data = NULL,
        message = error_msg,
        warning = NULL
      ))
    }
  )
}

#' Clean BibTeX text formatting
#'
#' @description
#' Removes common BibTeX formatting artifacts including double curly braces
#' used for capitalization preservation and LaTeX accent commands for
#' non-ASCII characters.
#'
#' @param text Character vector to clean
#'
#' @return Character vector with cleaned text
#'
#' @details
#' This function performs the following cleaning operations:
#' - Removes double curly braces {{}} used for capitalization preservation
#' - Converts common LaTeX accent commands to Unicode characters
#' - Normalises whitespace
#'
#'
#' @importFrom stringi stri_trans_general
#' @export
clean_bibtex_text <- function(text) {
  if (is.null(text) || is.na(text) || text == "") {
    return(text)
  }

  # Convert to character if not already
  text <- as.character(text)

  # Remove double curly braces (preserve inner content)
  # This handles cases like {{Title}} -> Title
  text <- gsub("\\{\\{([^}]+)\\}\\}", "\\1", text)

  # Common LaTeX accent command replacements
  latex_replacements <- list(
    # Diaeresis/umlaut
    '\\{\\\\"a\\}' = 'ä',
    '\\{\\\\"A\\}' = 'Ä',
    '\\{\\\\"e\\}' = 'ë',
    '\\{\\\\"E\\}' = 'Ë',
    '\\{\\\\"i\\}' = 'ï',
    '\\{\\\\"I\\}' = 'Ï',
    '\\{\\\\"o\\}' = 'ö',
    '\\{\\\\"O\\}' = 'Ö',
    '\\{\\\\"u\\}' = 'ü',
    '\\{\\\\"U\\}' = 'Ü',

    # Acute accent
    "\\{\\\\'a\\}" = 'á',
    "\\{\\\\'A\\}" = 'Á',
    "\\{\\\\'e\\}" = 'é',
    "\\{\\\\'E\\}" = 'É',
    "\\{\\\\'i\\}" = 'í',
    "\\{\\\\'I\\}" = 'Í',
    "\\{\\\\'o\\}" = 'ó',
    "\\{\\\\'O\\}" = 'Ó',
    "\\{\\\\'u\\}" = 'ú',
    "\\{\\\\'U\\}" = 'Ú',

    # Grave accent
    '\\{\\\\`a\\}' = 'à',
    '\\{\\\\`A\\}' = 'À',
    '\\{\\\\`e\\}' = 'è',
    '\\{\\\\`E\\}' = 'È',
    '\\{\\\\`i\\}' = 'ì',
    '\\{\\\\`I\\}' = 'Ì',
    '\\{\\\\`o\\}' = 'ò',
    '\\{\\\\`O\\}' = 'Ò',
    '\\{\\\\`u\\}' = 'ù',
    '\\{\\\\`U\\}' = 'Ù',

    # Circumflex accent
    '\\{\\\\\\^a\\}' = 'â',
    '\\{\\\\\\^A\\}' = 'Â',
    '\\{\\\\\\^e\\}' = 'ê',
    '\\{\\\\\\^E\\}' = 'Ê',
    '\\{\\\\\\^i\\}' = 'î',
    '\\{\\\\\\^I\\}' = 'Î',
    '\\{\\\\\\^o\\}' = 'ô',
    '\\{\\\\\\^O\\}' = 'Ô',
    '\\{\\\\\\^u\\}' = 'û',
    '\\{\\\\\\^U\\}' = 'Û',

    # Tilde
    '\\{\\\\~a\\}' = 'ã',
    '\\{\\\\~A\\}' = 'Ã',
    '\\{\\\\~n\\}' = 'ñ',
    '\\{\\\\~N\\}' = 'Ñ',
    '\\{\\\\~o\\}' = 'õ',
    '\\{\\\\~O\\}' = 'Õ',

    # Cedilla
    '\\{\\\\c\\{c\\}\\}' = 'ç',
    '\\{\\\\c\\{C\\}\\}' = 'Ç',

    # Special characters
    '\\{\\\\ss\\}' = 'ß',
    '\\{\\\\ae\\}' = 'æ',
    '\\{\\\\AE\\}' = 'Æ',
    '\\{\\\\o\\}' = 'ø',
    '\\{\\\\O\\}' = 'Ø',
    '\\{\\\\aa\\}' = 'å',
    '\\{\\\\AA\\}' = 'Å'
  )

  # Apply LaTeX replacements
  for (pattern in names(latex_replacements)) {
    text <- gsub(pattern, latex_replacements[[pattern]], text, perl = TRUE)
  }

  # Clean up any remaining single curly braces around single characters
  # This catches cases like {a} -> a
  text <- gsub("\\{([^{}])\\}", "\\1", text)

  # Normalize whitespace
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  # Use stringi for additional Unicode normalization if available
  if (requireNamespace("stringi", quietly = TRUE)) {
    text <- stringi::stri_trans_general(text, "Latin-ASCII; Any-NFC")
  }

  return(text)
}
