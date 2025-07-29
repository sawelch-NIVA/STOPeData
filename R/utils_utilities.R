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
