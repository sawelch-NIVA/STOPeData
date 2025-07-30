#' utilities
#' @title isTruthy switch operator
#'
#' @description isTruthy switch operator
#'
#' @return object types are unchanged
#'
#' @details
#' A simple switch operator based on purrr's %||% that returns the first
#' argument if it isTruthy (exists, not NULL/NA/"", etc.), and otherwise the
#' second argument.
#'
#' @param first The first argument, a value we want to use only if it isTruthy
#' @param second The second argument, a safe alternative if first isn't Truthy
#'
#'
#' @importFrom shiny isTruthy
`%|truthy|%` <- function(first, second) {
  if (isTruthy(first)) {
    first
  } else {
    second
  }
}

#' @title isRelevant
#'
#' @description Check if a categorical variable is "Not relevant" or "Not reported"
#'
#' @return Boolean
#'
#' @details
#' Check if a user has entered "Not relevant" or "Not reported" in selectInput
#' for the purpose of including conditional fields, validation, etc.
#'
#' @param first The first argument, a value we want to use only if it isTruthy
#' @param second The second argument, a safe alternative if first isn't Truthy
#'
#'
isRelevant <- function(input) {
  stopifnot(is.character(input))
  !(input %in% c("Not relevant", "Not reported"))
}

#' print content of reactiveValues object
#'
#' @param data A reactiveValues object with named variables
#' @description
#' Print a reactiveValues object, with each named variable and its value on a new
#' line
#'
#'
#' @returns a string of variable names and values
#' @export
#'
#' @examples
#' reactiveValues(x = 1, y = 2) |> printreactiveValues()
printreactiveValues <- function(data) {
  data_lines <- sapply(
    names(data),
    function(name) {
      value <- data[[name]]
      if (is.na(value) || is.null(value)) {
        paste0(name, " = NA")
      } else if (inherits(value, "Date")) {
        paste0(name, " = as.Date('", as.character(value), "')")
      } else if (is.character(value)) {
        paste0(name, " = '", value, "'")
      } else {
        paste0(name, " = ", as.character(value), "")
      }
    }
  )
  paste(data_lines, collapse = "\n")
}
