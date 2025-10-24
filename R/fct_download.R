# Metadata Format Helper Functions ----
# Functions to create and read human-readable metadata files

#' Create readable metadata text file ----
#'
#' @description Create a human-readable text file with export metadata
#' @param metadata_list List containing metadata
#' @param file_path Path where to write the metadata file
#' @importFrom glue glue
#' @export
write_metadata_txt <- function(metadata_list, file_path) {
  # Helper operator for string repetition
  `%r%` <- function(string, times) {
    paste(rep(string, times), collapse = "")
  }

  # Create human-readable content
  content <- c(
    "STOPeData Export Metadata",
    "=" %r% 50, # 50 equals signs
    "",
    glue("Campaign Name: {metadata_list$campaign_name}"),
    glue("Export Date/Time: {metadata_list$export_datetime}"),
    glue("Exported By: {metadata_list$user}"),
    "",
    "Application Information:",
    glue("  App Name: {metadata_list$app_name}"),
    glue("  App Version: {metadata_list$app_version}"),
    glue("  Git Commit: {metadata_list$git_commit}"),
    glue("  App URL: {metadata_list$app_url}"),
    "",
    "Technical Information:",
    glue("  Browser: {metadata_list$browser}"),
    "",
    "=" %r% 50,
    "",
    "This ZIP file contains CSV data files exported from STOPeData.",
    "Each CSV file contains one type of data (sites, parameters, etc.).",
    "To re-import this data, use the 'Upload session data' option",
    "in STOPeData."
  )

  # Write to file
  writeLines(content, file_path)
}
