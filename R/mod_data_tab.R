#' data_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny DT spData dplyr vroom tools rnrfa lubridate
#' @importFrom sf st_coordinates st_sample

mod_data_tab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    mainPanel(
      # Buttons row
      fluidRow(
        column(3, actionButton(ns("species_summary_button"), "Species summary")),
        column(3, actionButton(ns("year_summary_button"), "Year summary")),
        column(3, actionButton(ns("id_summary_button"), "Identifier summary")),
        column(3, actionButton(ns("coords_summary_button"), "Calculate bounds"))
      ),

      br(),

      # Species summary section
      h2(textOutput(ns("species_title"))),
      DTOutput(ns("species_summary_table")),

      # Year summary section
      h2(textOutput(ns("date_title"))),
      DTOutput(ns("year_summary_table")),

      # Identifier summary section
      h2(textOutput(ns("id_title"))),
      DTOutput(ns("id_summary_table")),

      # Bounding box summary section
      h2(textOutput(ns("coords_title"))),
      DTOutput(ns("coords_summary_table"))
    )
  )
}

#' data_tab Server Functions
#'
#' @noRd
mod_data_tab_server <- function(id, uploaded_data, reformatted_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Species summary
    species_summary <- eventReactive(input$species_summary_button, {
      req(reformatted_data())
      if ("species" %in% colnames(reformatted_data())) {
        reformatted_data() %>%
          group_by(species) %>%
          summarise(`Number of Records` = n(), .groups = "drop") %>%
          rename(Species = species)
      } else {
        NULL
      }
    })

    output$species_summary_table <- renderDT({
      req(species_summary())
      species_summary()
    })

    output$species_title <- renderText({
      req(species_summary())
      "Species Summary"
    })

    # Year summary
    year_summary <- eventReactive(input$year_summary_button, {
      req(reformatted_data())
      if ("year" %in% colnames(reformatted_data())) {
        reformatted_data() %>%
          group_by(year) %>%
          summarise(`Number of Records` = n(), .groups = "drop")
      } else {
        NULL
      }
    })

    output$year_summary_table <- renderDT({
      req(year_summary())
      year_summary()
    })

    output$date_title <- renderText({
      req(year_summary())
      "Year Summary"
    })

    # Identifier summary
    id_summary <- eventReactive(input$id_summary_button, {
      req(reformatted_data())

      if ("identifier" %in% colnames(reformatted_data()) &&
          "species" %in% colnames(reformatted_data())) {
        reformatted_data() %>%
          group_by(identifier) %>%
          summarise(
            `Number of Records` = n(),
            `Number of Species` = n_distinct(species),
            .groups = "drop"
          ) %>%
          rename(Identifier = identifier)
      } else {
        NULL
      }
    })

    output$id_summary_table <- renderDT({
      req(id_summary())
      id_summary()
    })

    output$id_title <- renderText({
      req(id_summary())
      "Identifier Summary"
    })

    # Bounding box summary
    bbox <- eventReactive(input$coords_summary_button, {
      req(reformatted_data())

      if (all(c("x_coordinate", "y_coordinate") %in% colnames(reformatted_data()))) {
        reformatted_data() %>%
          summarise(
            `y-coordinate Min` = min(y_coordinate, na.rm = TRUE),
            `y-coordinate Max` = max(y_coordinate, na.rm = TRUE),
            `x-coordinate Min` = min(x_coordinate, na.rm = TRUE),
            `x-coordinate Max` = max(x_coordinate, na.rm = TRUE)
          )
      } else {
        NULL
      }
    })

    output$coords_summary_table <- renderDT({
      req(bbox())
      bbox()
    })

    output$coords_title <- renderText({
      req(bbox())
      "Bounding Box"
    })
  })
}
