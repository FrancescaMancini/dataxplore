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
      fluidRow(
        column(2, actionButton(ns("species_summary_button"), "Species summary")),
        column(2, actionButton(ns("date_summary_button"), "Dates summary")),
        column(2, actionButton(ns("year_summary_button"), "Year summary")),
        column(2, actionButton(ns("id_summary_button"), "Identifier summary")),
        column(2, actionButton(ns("coords_summary_button"), "Calculate bounds"))
      ),

      h2(textOutput(ns("species_title"))),
      DTOutput(ns("species_summary_table")),
      h2(textOutput(ns("date_title"))),
      DTOutput(ns("date_summary_table")),
      h2(textOutput(ns("year_title"))),
      DTOutput(ns("year_summary_table")),
      h2(textOutput(ns("id_title"))),
      DTOutput(ns("id_summary_table")),
      h2(textOutput(ns("coords_title"))),
      DTOutput(ns("coords_summary_table"))
    )
  )
}

#' data_tab Server Functions
#'
#' @noRd
mod_data_tab_server <- function(id, user_selections, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Number of records per species
    species_summary <- eventReactive(input$species_summary_button, {
      req(uploaded_data(), user_selections()$species)
      uploaded_data() %>%
        group_by(Species = get(user_selections()$species)) %>%
        summarise(`Number of Records` = n())
    })
    
    output$species_summary_table <- DT::renderDT({
      req(species_summary())
      species_summary()
    })
    
    output$species_title <- renderText({
      req(user_selections()$species, input$species_summary_button)
      "Species Summary"
    })
    
    # Date summary
    date_summary <- eventReactive(input$date_summary_button, {
      req(user_selections()$date)
      format_str <- switch(user_selections()$date_format,
                           "format_a" = "%d/%m/%Y",
                           "format_b" = "%m/%d/%Y",
                           "format_c" = "%Y/%m/%d"
      )
      uploaded_data() %>%
        mutate(date = case_when(
          user_selections()$date_format == "format_a" ~ dmy(.data[[user_selections()$date]]),
          user_selections()$date_format == "format_b" ~ mdy(.data[[user_selections()$date]]),
          user_selections()$date_format == "format_c" ~ ymd(.data[[user_selections()$date]])
        )) %>%
        summarise(
          `First Record` = min(date, na.rm = TRUE),
          `Last Record` = max(date, na.rm = TRUE), .groups = "drop"
        )
    })
    
    output$date_summary_table <- DT::renderDT({
      req(date_summary())
      date_summary()
    })
    
    output$date_title <- renderText({
      req(user_selections()$date, input$date_summary_button)
      "Date Summary"
    })
    
    # Number of records per year
    year_summary <- eventReactive(input$year_summary_button, {
      req(uploaded_data(), user_selections()$date)
      uploaded_data() %>%
        group_by(Year = get(user_selections()$date)) %>%
        summarise(`Number of Records` = n())
    })
    
    output$year_summary_table <- DT::renderDT({
      req(year_summary())
      year_summary()
    })
    
    output$year_title <- renderText({
      req(user_selections()$date, input$year_summary_button)
      "Year Summary"
    })
    
    # Number of records per group (ID)
    id_summary <- eventReactive(input$id_summary_button, {
      req(uploaded_data(), user_selections()$id, user_selections()$species)
      uploaded_data() %>%
        group_by(`Identifier` = get(user_selections()$id)) %>%
        summarise(
          `Number of Records` = n(),
          `Number of Species` = n_distinct(get(user_selections()$species))
        )
    })
    
    output$id_summary_table <- DT::renderDT({
      req(id_summary())
      id_summary()
    })
    
    output$id_title <- renderText({
      req(user_selections()$id, input$id_summary_button)
      "Identifier Summary"
    })
    
    # Bounding box
    bbox <- eventReactive(input$coords_summary_button, {
      req(uploaded_data(), user_selections()$lat, user_selections()$lon)
      uploaded_data() %>%
        select(user_selections()$lat, user_selections()$lon) %>%
        summarise(
          `Latitude Min` = min(eval(as.name(user_selections()$lat))),
          `Latitude Max` = max(eval(as.name(user_selections()$lat))),
          `Longitude Min` = min(eval(as.name(user_selections()$lon))),
          `Longitude Max` = max(eval(as.name(user_selections()$lon)))
        )
    })
    
    output$coords_summary_table <- DT::renderDT({
      req(bbox())
      bbox()
    })
    
    output$coords_title <- renderText({
      req(user_selections()$lat, user_selections()$lon, input$coords_summary_button)
      "Bounding Box"
    })
  })
}