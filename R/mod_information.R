#' information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_body
#' @importFrom report cite_packages
mod_information_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(card_body(
      h1("Packages and Software"),
      p(
        "The following packages and software were used in the development of 
      this app."
      ),
      HTML(cite_packages() |> suppressWarnings())
    ))
  )
}

#' information Server Functions
#'
#' @noRd
mod_information_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_information_ui("information_1")

## To be copied in the server
# mod_information_server("information_1")
