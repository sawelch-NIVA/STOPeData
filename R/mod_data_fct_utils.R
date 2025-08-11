#' Read in ecotoxicological units and conversion factors from csv
#'
#' @param select_column name of column to pull ("MEASURED_UNIT", "BASE_SI_UNIT", "CONVERSION_FACTOR", "UNIT_COMMENTS")
#'
#' @returns a dataframe or a character vector
#'
#' @export
#' @importFrom readr read_csv
parameter_units <- function(select_column = NULL) {
  units <- read_csv(
    file = "inst/data/clean/unit_conversion_factors.csv",
    col_names = TRUE,
    show_col_types = FALSE
  )
  if (is.null(select_column)) {
    return(units)
  }
  stopifnot(select_column %in% names(units))
  return(units[[select_column]])
}
