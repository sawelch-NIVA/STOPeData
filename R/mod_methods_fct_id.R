#' Generate Protocol ID ----
#'
#' @description
#' Generates a standardised protocol identifier based on protocol type, name,
#' sequence number, and campaign. The function is fully vectorised and can
#' handle multiple protocols simultaneously.
#'
#' @param protocol_type Character vector. The category of protocol
#'   (e.g., "Sampling Protocol", "Analytical Protocol"). Must match one of
#'   the predefined categories or will default to "X".
#' @param protocol_name Character vector. The specific name/method within
#'   the protocol category. Will be abbreviated and cleaned for ID generation.
#' @param sequence_number Numeric vector. Sequential number for protocols
#'   within the same category. Defaults to 1. Will be zero-padded to 2 digits.
#' @param campaign_name Character vector. Name of the campaign/study.
#'   Will be abbreviated to first 3 alphanumeric characters. Defaults to "".
#'
#' @return Character vector of protocol IDs with format:
#'   `{TypeCode}{SequenceNumber}_{AbbreviatedName}_{CampaignAbbrev}`
#'
#'   Where:
#'   - TypeCode: S (Sampling), F (Fractionation), E (Extraction),
#'     A (Analytical), X (Unknown)
#'   - SequenceNumber: Zero-padded 2-digit number
#'   - AbbreviatedName: Up to 15 alphanumeric characters (optional)
#'   - CampaignAbbrev: Up to 3 alphanumeric characters (optional)
#'
#' @details
#' The function handles edge cases gracefully:
#' - NULL or empty values result in appropriate defaults
#' - Invalid protocol types default to "X"
#' - Names are cleaned of special characters and spaces
#' - Empty components are omitted from the final ID
#'
#' @examples
#' # Single protocol
#' generate_protocol_id("Sampling Protocol", "Water Sample", 1, "MyStudy")
#' # Returns: "S01_WaterSample_MyS"
#'
#' # Multiple protocols (vectorised)
#' types <- c("Sampling Protocol", "Analytical Protocol")
#' names <- c("Water Sample", "LC-MS Analysis")
#' sequences <- c(1, 2)
#' generate_protocol_id(types, names, sequences, "Study2024")
#' # Returns: c("S01_WaterSample_Stu", "A02_LCMSAnalysis_Stu")
#'
#' @family protocol_functions
#' @export
generate_protocol_id <- function(
  protocol_type,
  protocol_name,
  sequence_number = 1,
  campaign_name = ""
) {
  # Map protocol categories to single letters ----
  type_mapping <- c(
    "Sampling Protocol" = "S",
    "Fractionation Protocol" = "F",
    "Extraction Protocol" = "E",
    "Analytical Protocol" = "A"
  )

  # Get the letter code (vectorised) ----
  type_code <- type_mapping[protocol_type]
  type_code <- ifelse(is.na(type_code), "X", type_code)

  # Create abbreviated name (vectorised) ----
  # Handle NULL and empty protocol_name values
  abbreviated_name <- case_when(
    is.null(protocol_name) |
      is.na(protocol_name) |
      nchar(trimws(protocol_name)) == 0 ~ "",
    TRUE ~ {
      # Split into words, capitalize first letter of each, then join and clean
      words <- strsplit(protocol_name, "\\s+")[[1]]
      capitalized <- paste0(
        toupper(substr(words, 1, 1)),
        tolower(substr(words, 2, nchar(words)))
      )
      clean_name <- paste0(capitalized, collapse = "")
      # Remove any remaining special chars and limit length
      clean_name <- gsub("[^A-Za-z0-9]", "", clean_name)
      substr(clean_name, 1, 15)
    }
  )

  # Format sequence number (vectorised) ----
  formatted_sequence <- sprintf("%02d", sequence_number)

  # Create campaign abbreviation (vectorised) ----
  campaign_abbrev <- case_when(
    is.null(campaign_name) |
      is.na(campaign_name) |
      nchar(trimws(campaign_name)) == 0 ~ "",
    TRUE ~ {
      # Take first 3 characters, remove spaces and special chars
      clean_campaign <- gsub("[^A-Za-z0-9]", "", campaign_name)
      substr(clean_campaign, 1, 10)
    }
  )

  # Combine all parts ----
  protocol_id <- paste0(
    type_code,
    formatted_sequence,
    ifelse(abbreviated_name == "", "", paste0("_", abbreviated_name)),
    ifelse(campaign_abbrev == "", "", paste0("_", campaign_abbrev))
  )

  return(protocol_id)
}
