#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      "dataXplore",
      tabPanel("Info",
               mod_info_tab_ui("info_tab_1")
      ),
      tabPanel("Data",
               fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),
                 sidebarLayout(
                   sidebarPanel(
                     fileInput("upload",
                               "Upload your data",
                               accept=c(".csv", ".txt")),
                     checkboxInput(
                       "grid_ref",
                       "Convert British National Grid References", FALSE),
                     tags$div(id = 'placeholder'),
                     actionButton(
                       "view_button", "View data")),
                   mainPanel(
                     DTOutput("data_table")
                     )
                 ),
                 mod_data_tab_ui("data_tab_1")
               )
      ),
      tabPanel("Time",
               fluidPage(
                 theme = bslib::bs_theme(bootswatch = "sandstone"),
                 mod_time_bias_tab_ui("time_bias_tab_1")
               )
      ),
      tabPanel("Species",
               tabsetPanel(
                 tabPanel("Species number",
                          fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),
                                    mod_species_bias_tab_ui("species_bias_tab_1")
                          )
                 ),
                 tabPanel("Species ID",
                          fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),
                                    mod_species_id_bias_tab_ui("species_id_bias_tab_1")

                          )
                 ),
                 tabPanel("Rarity bias",
                          fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),
                                    mod_rarity_bias_tab_ui("rarity_bias_tab_1")
                          )
                 )
               ) #Close inner tabsetPanel
      ),
      tabPanel("Space",
               tabsetPanel(
                 tabPanel("Spatial coverage",
                          fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),
                                    mod_space_cov_tab_ui("space_cov_tab_1")
                          )
                 ),
                 tabPanel("Spatial bias",
                          fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),
                                    mod_space_bias_tab_ui("space_bias_tab_1")

                          )
                 )
               ) #Close inner tabsetPanel
      ),
      # tabPanel("Space",
      #          fluidPage(
      #            theme = bslib::bs_theme(bootswatch = "sandstone"),
      #            mod_space_bias_tab_ui("space_bias_tab_1")
      #          )
      # ),
      tabPanel("Environment",
               fluidPage(
                 theme = bslib::bs_theme(bootswatch = "sandstone"),
                 mod_environment_bias_tab_ui("environment_bias_tab_1")
               )
      ),
      tabPanel("Export",
               fluidPage(
                 theme = bslib::bs_theme(bootswatch = "sandstone"),
                 mod_export_tab_ui("export_tab_1")
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
      app_title = "dataxplore"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
