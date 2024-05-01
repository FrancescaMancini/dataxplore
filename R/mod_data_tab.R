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
  tagList(
    mainPanel(
      actionButton("species_summary_button", "Species summary"),
      actionButton("date_summary_button", "Dates summary"),
      actionButton("coords_summary_button", "Calculate bounding box"),
      actionButton("year_summary_button", "Year summary"),
      actionButton("id_summary_button", "Identifier summary"),
      
      h2(textOutput("species_title")), # add a title for each summary table only if the action button is clicked
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
}

#' data_tab Server Functions
#'
#' @noRd
mod_data_tab_server <- function(id, uploaded_data, user_selections) {
  moduleServer(id, function(input, output, session) {
    options(shiny.maxRequestSize = 100 * 1024^2)
    ns <- session$ns

    # Species Summary
    output$species_summary_table <- DT::renderDT({
      req(user_selections()$species, user_selections()$species_summary_button)
      uploaded_data %>%
        group_by(.data[[user_selections()$species]]) %>%
        summarise(n_records = n(), .groups = "drop")
    })

    output$species_title <- renderText("Species Summary")

    date_summary <- reactive({
      req(user_selections()$date, user_selections()$date_summary_button)
      format_str <- switch(user_selections()$date_format,
        "format_a" = "%d/%m/%Y",
        "format_b" = "%m/%d/%Y",
        "format_c" = "%Y/%m/%d"
      )
      uploaded_data %>%
        mutate(date = case_when(
          user_selections()$date_format == "format_a" ~ dmy(.data[[user_selections()$date]]),
          user_selections()$date_format == "format_b" ~ mdy(.data[[user_selections()$date]]),
          user_selections()$date_format == "format_c" ~ ymd(.data[[user_selections()$date]])
        )) %>%
        summarise(
          first_record = min(date, na.rm = TRUE),
          last_record = max(date, na.rm = TRUE), .groups = "drop"
        )
    })

    # Date Summary
    output$date_summary_table <- DT::renderDT({
      date_summary()
    })

    output$date_title <- renderText({
      req(user_selections()$date, user_selections()$date_summary_button)

      "Date Summary"
    })

    # number of records per year
    year_summary <- reactive({
      req(uploaded_data, user_selections()$year, user_selections()$year_summary_button)

      uploaded_data %>%
        group_by(get(user_selections()$year)) %>%
        summarise(n_records = n())
    })

    output$year_summary_table <- DT::renderDT({
      year_summary()
    })

    output$year_title <- renderText({
      req(uploaded_data, user_selections()$year, user_selections()$year_summary_button)

      "Year summary"
    })

    # number of records per group
    id_summary <- reactive({
      req(uploaded_data, user_selections()$id, user_selections()$species, user_selections()$id_summary_button)

      uploaded_data %>%
        group_by(get(user_selections()$id)) %>%
        summarise(
          n_records = n(),
          n_species = n_distinct(get(user_selections()$species))
        )
    })

    output$id_summary_table <- DT::renderDT({
      id_summary()
    })

    output$id_title <- renderText({
      req(uploaded_data, user_selections()$id, user_selections()$id_summary_button)

      "Identifier summary"
    })

    # create the variable summaries
    # number of records per species
    species_summary <- reactive({
      req(uploaded_data, user_selections()$species, user_selections()$species_summary_button)

      uploaded_data %>%
        group_by(get(user_selections()$species)) %>% # get() needed because user_selections()$species is a character string
        summarise(n_records = n())
    })

    output$species_summary_table <- DT::renderDT({
      species_summary()
    })

    output$species_title <- renderText({
      req(uploaded_data, user_selections()$species, user_selections()$species_summary_button)

      "Species summary"
    })

    # bounding box
    bbox <- reactive({
      req(uploaded_data, user_selections()$lat, user_selections()$lon, user_selections()$coords_summary_button)

      uploaded_data %>%
        select(user_selections()$lat, user_selections()$lon) %>%
        summarise(
          lat_min = min(eval(as.name(user_selections()$lat))),
          lat_max = max(eval(as.name(user_selections()$lat))),
          lon_min = min(eval(as.name(user_selections()$lon))),
          lon_max = max(eval(as.name(user_selections()$lon)))
        )
    })

    output$coords_summary_table <- DT::renderDT({
      bbox()
    })

    output$coords_title <- renderText({
      req(uploaded_data, user_selections()$lat, user_selections()$lon, user_selections()$coords_summary_button)

      "Bounding box"
    })

    # Render uploaded data table
    output$uploaded_data_table <- DT::renderDT(uploaded_data)
  })
}
