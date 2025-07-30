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
