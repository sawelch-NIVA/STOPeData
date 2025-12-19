#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom logger log_appender log_layout log_threshold log_messages log_warnings layout_json

run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = "server",
  uiPattern = "/",
  ...
) {
  # set up logger logging for gcp
  # or don't, if we're on posit connect cloud, because it crashes
  # on startup

  if (Sys.getenv("R_CONFIG_ACTIVE") != "connect_cloud") {
    logger::log_appender(logger::appender_stdout)
    logger::log_layout(logger::layout_json())
    logger::log_threshold(logger::INFO)

    logger::log_messages()
    logger::log_warnings()
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
