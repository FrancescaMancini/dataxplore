#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib DT
#' @noRd

app_ui <- function(request) {
  tagList(
    navbarPage(
      "dataXplore",
      tabPanel(
        "Info",
        mod_info_tab_ui("info_tab_1")
      ),
      tabPanel(
        "Data",
        sidebarLayout(
          sidebarPanel(
            fileInput("upload", "Upload your data", accept = c(".csv", ".txt")),
            checkboxInput("grid_ref", "Convert British National Grid References", FALSE),
            tags$div(id = "placeholder"),
            varSelectInput("species", "Species column", data = NULL),
            varSelectInput("date", "Date column", data = NULL),
            radioButtons("date_format", "Select date format (please ignore separator)",
              choices = c(
                "day/month/year" = "format_a",
                "month/day/year" = "format_b",
                "year/month/day" = "format_c"
              ),
              selected = "format_a"
            ),
            varSelectInput("lon", "Longitude column", data = NULL),
            varSelectInput("lat", "Latitude column", data = NULL),
            varSelectInput("id", "Choose the identifier", data = NULL),
            checkboxInput("report", "Add to report", FALSE)
          ),
          mainPanel(
            DTOutput("uploaded_data_table"),
            DTOutput("formatted_data_table"),
            mod_data_tab_ui(id = "data_tab_1"),
            h2(textOutput("species_title")),
            DTOutput("species_summary_table"),
            h2(textOutput("date_title")),
            DTOutput("date_summary_table"),
            h2(textOutput("year_title")),
            DTOutput("year_summary_table"),
            h2(textOutput("id_title")),
            DTOutput("id_summary_table"),
            h2(textOutput("coords_title")),
            DTOutput("coords_summary_table")
          )
        )
      ),
      tabPanel(
        "Time", mod_time_bias_tab_ui("time_bias_tab_1")
      ),
      tabPanel(
        "Species",
        tabsetPanel(
          tabPanel("Species number", mod_species_bias_tab_ui("species_bias_tab_1")),
          tabPanel("Species ID", mod_species_id_bias_tab_ui("species_id_bias_tab_1")),
          tabPanel("Rarity bias", mod_rarity_bias_tab_ui("rarity_bias_tab_1"))
        )
      ),
      tabPanel(
        "Space",
        tabsetPanel(
          tabPanel("Spatial coverage", mod_space_cov_tab_ui("space_cov_tab_1")),
          tabPanel("Spatial bias", mod_space_bias_tab_ui("space_bias_tab_1"))
        )
      ),
      tabPanel("Environment", mod_environment_bias_tab_ui("environment_bias_tab_1")),
      tabPanel("Export", mod_export_tab_ui("export_tab_1"))
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
      app_title = "dataxplore"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
