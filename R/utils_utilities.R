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
