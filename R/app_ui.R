#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_menu nav_item bs_theme navset_card_tab sidebar navbar_options input_dark_mode
#' @importFrom bsicons bs_icon
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "STOP eData",
      window_title = "STOP eData",
      header = input_dark_mode(id = "darkmode"),
      navbar_options = navbar_options(
        underline = TRUE,
        theme = "auto",
        position = "static-top"
      ),
      # nav_panel(title = "Landing", value = "00-landing", mod_landing_ui("landing")),
      nav_panel(
        title = "1. Campaign",
        value = "01-campaign",
        mod_campaign_ui("campaign"),
        icon = bs_icon("info-square")
      ),
      nav_panel(
        title = "2. Reference",
        value = "02-references",
        mod_references_ui("references"),
        icon = bs_icon("chat-left-quote-fill")
      ),
      nav_panel(
        title = "3. Sites",
        value = "03-sites",
        mod_sites_ui("sites"),
        icon = bs_icon("pin-map-fill")
      ),
      nav_panel(
        title = "4. Parameters",
        value = "04-parameters",
        mod_parameters_ui("parameters"),
        icon = bs_icon("flag")
      ),
      nav_panel(
        title = "5. Compartments",
        value = "04-compartments",
        mod_compartments_ui("compartments"),
        icon = bs_icon("box")
      ),
      nav_panel(
        title = "6. Methods",
        value = "06-methods",
        mod_methods_ui("methods"),
        icon = bs_icon("wrench-adjustable-circle-fill")
      ),
      nav_panel(
        title = "7. Samples",
        value = "07-samples",
        mod_samples_ui("samples"),
        icon = bs_icon("ui-checks-grid")
      ),
      nav_panel(
        title = "8. Export",
        value = "08-export",
        mod_export_ui("export"),
        icon = bs_icon("box-arrow-right")
      ),
      nav_panel(
        title = "9. Review",
        value = "09-review",
        mod_review_ui("review"),
        icon = bs_icon("clipboard-data-fill")
      ),
      nav_spacer(),
      footer = tags$span(
        tags$a(
          href = "https://github.com/sawelch-NIVA/STOPeData",
          "stop-e-data"
        ),
        " developed by ",
        tags$a(href = "https://www.niva.no/en/projects/expect", "EXPECT"),
        ", and ",
        tags$a(href = "https://www.niva.no/en/featured-pages/nctp", "NCTP"),
        "."
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "STOPeData"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
