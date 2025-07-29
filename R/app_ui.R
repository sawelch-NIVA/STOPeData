#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_menu nav_item bs_theme navset_card_tab sidebar navbar_options
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "My App",
      navbar_options = navbar_options(
        bg = "#0062cc",
        underline = TRUE,
        theme = "auto",
        position = "static-top"
      ),
      # nav_panel(title = "Landing", mod_landing_ui("landing")),
      nav_panel(title = "1. Campaign", mod_campaign_ui("campaign")),
      nav_panel(title = "2. Reference", mod_references_ui("references")),
      nav_panel(title = "3. Sites", mod_sites_ui("sites")),
      nav_panel(title = "4. Parameters", mod_parameters_ui("parameters")),
      nav_panel(title = "5. Compartments", mod_compartments_ui("compartments")),
      nav_panel(title = "6. Methods", mod_methods_ui("methods")),
      nav_panel(title = "7. Samples", mod_samples_ui("samples")),
      nav_panel(title = "8. Export", mod_export_ui("export")),
      nav_panel(title = "9. Review", mod_review_ui("review")),
      nav_spacer(),
      footer = p("footer")
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
