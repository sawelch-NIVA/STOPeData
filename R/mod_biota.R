#' biota UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList pre
#'@importFrom golem print_dev
#'
cat_art <- "                   _ |\\_
                   \\` ..\\
              __,.-\" =__Y=
            .\"        )
      _    /   ,    \\/\\_
     ((____|    )_-\\ \\_-`
jgs  `-----'`-----` `--`"

mod_biota_ui <- function(id) {
  ns <- NS(id)
  tagList(
 pre(cat_art)
  )
}

#' biota Server Functions
#'
#' @noRd
mod_biota_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    print_dev("mod_biota_server initialised")
  })
}

## To be copied in the UI
# mod_biota_ui("biota_1")

## To be copied in the server
# mod_biota_server("biota_1")
