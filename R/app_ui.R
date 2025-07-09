#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib DT shinyhelper
#' @noRd

app_ui <- function(request) {
  tagList(
    shinyjs::useShinyjs(),
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
            fileInput("upload", "Upload your data", accept = c(".csv", ".txt")) %>%
          helper(icon = "info-circle", colour = "black", 
            content = "data_upload",
            type = "markdown"),

            div(id = "data_upload_inputs",
            checkboxInput("grid_ref", "Do you want to convert British grid references to easting and northing", FALSE),
            tags$div(id = "placeholder"),
            selectInput("species", "Species column", choices = NULL),
            checkboxInput("has_year_column", "Do you have a column containing year values (as separate integers e.g., 2014, 2015...)?", FALSE),
            uiOutput("date_year_ui"),
            uiOutput("y_coordinate_x_coordinate"),
            
            ## please add documentation ##
            checkboxInput("has_spatial_uncertainty", "Do you have a column recording spatial uncertainty?", FALSE),
            uiOutput("spatial_uncertainty"),

            selectInput("id", "Choose the identifier", choices = NULL)
            )
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
          tabPanel("Rarity bias", mod_species_rarity_bias_tab_ui("species_rarity_bias_tab_1"))
        )
      ),
      tabPanel(
        "Space",
        tabsetPanel(
          tabPanel("Spatial coverage", mod_space_cov_tab_ui("space_cov_tab_1")),
          tabPanel("Spatial bias", mod_space_bias_tab_ui("space_bias_tab_1"))
        )
      ),
      tabPanel("Environment", mod_environment_bias_tab_ui("environment_bias_tab_1"))
    )
  )
}
