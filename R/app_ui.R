#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_menu nav_item bs_theme navset_card_tab sidebar navbar_options input_dark_mode
#' @importFrom bsicons bs_icon
#' @importFrom htmltools HTML
#' @noRd
app_ui <- function(request) {
  tagList(
    # Notification CSS gets overwritten unless we set it here - moved to top right
    tags$head(
      tags$style(HTML(
        "
          #shiny-notification-panel {
            top: 0;
            bottom: unset;
            left: unset;
            right: 0;
            margin-left: auto;
            margin-right: auto;
            width: 100%;
            max-width: 450px;
          }
    "
      ))
    ),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "STOP eData",
      id = "main-page",
      window_title = "STOP eData",
      navbar_options = navbar_options(
        underline = TRUE,
        theme = "auto",
        position = "static-top"
      ),
      # nav_panel(title = "Landing", value = "00-landing", mod_landing_ui("landing")),
      nav_panel(
        title = "0. LLM Extract",
        value = "00-llm-extract",
        mod_llm_ui("llm_extract"),
        icon = bs_icon("cpu")
      ),
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
        value = "05-compartments",
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
        title = "8. Biota",
        value = "08-biota",
        mod_biota_ui("biota"),
        icon = HTML(
          '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-leaf" viewBox="0 0 16 16">
                      <path d="M1.4 1.7c.216.289.65.84 1.725 1.274 1.093.44 2.884.774 5.834.528l.37-.023c1.823-.06 3.117.598 3.956 1.579C14.16 6.082 14.5 7.41 14.5 8.5c0 .58-.032 1.285-.229 1.997q.198.248.382.54c.756 1.2 1.19 2.563 1.348 3.966a1 1 0 0 1-1.98.198c-.13-.97-.397-1.913-.868-2.77C12.173 13.386 10.565 14 8 14c-1.854 0-3.32-.544-4.45-1.435-1.125-.887-1.89-2.095-2.391-3.383C.16 6.62.16 3.646.509 1.902L.73.806zm-.05 1.39c-.146 1.609-.008 3.809.74 5.728.457 1.17 1.13 2.213 2.079 2.961.942.744 2.185 1.22 3.83 1.221 2.588 0 3.91-.66 4.609-1.445-1.789-2.46-4.121-1.213-6.342-2.68-.74-.488-1.735-1.323-1.844-2.308-.023-.214.237-.274.38-.112 1.4 1.6 3.573 1.757 5.59 2.045 1.227.215 2.21.526 3.033 1.158.058-.39.075-.782.075-1.158 0-.91-.288-1.988-.975-2.792-.626-.732-1.622-1.281-3.167-1.229l-.316.02c-3.05.253-5.01-.08-6.291-.598a5.3 5.3 0 0 1-1.4-.811"/>
                      </svg>'
        )
      ),
      nav_panel(
        title = "9. Data",
        value = "09-data",
        mod_data_ui("data"),
        icon = bs_icon("app")
      ),
      nav_panel(
        title = "10. Review",
        value = "10-review",
        mod_review_ui("review"),
        icon = bs_icon("clipboard-data-fill")
      ),
      nav_panel(
        title = "11. Export",
        value = "11-export",
        mod_export_ui("export"),
        icon = bs_icon("box-arrow-right")
      ),
      nav_item(input_dark_mode(id = "darkmode")),
      nav_item(htmlOutput(outputId = "dbStatus")),
      nav_spacer(),
      footer = tags$span(
        ## Navigation buttons ----
        div(
          class = "navigation-buttons-container",
          style = "display: flex; justify-content: space-between; margin: 20px;",

          input_task_button(
            id = "previous_section",
            label = HTML('Previous Section'),
            type = "primary"
          ),

          input_task_button(
            id = "next_section",
            label = HTML('Next Section'),
            type = "primary"
          )
        ),
        div(
          style = "margin: 0px 5px 10px 15px;",
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
