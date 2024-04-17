#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr
#' @noRd

app_server <- function(input, output, session) {

  uploaded_data <- reactiveVal()

  # Your application server logic
  mod_info_tab_server("info_tab_1")

  # Handle file upload
  observe({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    data <- switch(ext,
      csv = vroom(input$upload$datapath, delim = ","),
      txt = vroom(input$upload$datapath, delim = "\t"),
      stop("Invalid file; Please upload a .csv or .txt file")
    )
    uploaded_data(data)
  })

  # Dynamically update variable selections based on the uploaded data
  observeEvent(uploaded_data(), {
    updateVarSelectInput(session, "species", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "date", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "lon", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "lat", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "id", data = uploaded_data(), selected = character(0))
  })

  # Species Summary
  output$species_summary_table <- DT::renderDT({
    req(input$species, input$species_summary_button)
    uploaded_data() %>%
      group_by(.data[[input$species]]) %>%
      summarise(n_records = n(), .groups = "drop")
  })

  output$species_title <- renderText("Species Summary")

  date_summary <- reactive({
    req(input$date, input$date_summary_button)
    format_str <- switch(input$date_format,
      "format_a" = "%d/%m/%Y",
      "format_b" = "%m/%d/%Y",
      "format_c" = "%Y/%m/%d"
    )
    uploaded_data() %>%
      mutate(date = case_when(
        input$date_format == "format_a" ~ dmy(.data[[input$date]]),
        input$date_format == "format_b" ~ mdy(.data[[input$date]]),
        input$date_format == "format_c" ~ ymd(.data[[input$date]])
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
    req(input$date, input$date_summary_button)

    "Date Summary"
  })

  # number of records per year
  year_summary <- reactive({
    req(uploaded_data(), input$year, input$year_summary_button)

    uploaded_data() %>%
      group_by(get(input$year)) %>%
      summarise(n_records = n())
  })

  output$year_summary_table <- DT::renderDT({
    year_summary()
  })

  output$year_title <- renderText({
    req(uploaded_data(), input$year, input$year_summary_button)

    "Year summary"
  })

  # number of records per group
  id_summary <- reactive({
    req(uploaded_data(), input$id, input$species, input$id_summary_button)

    uploaded_data() %>%
      group_by(get(input$id)) %>%
      summarise(
        n_records = n(),
        n_species = n_distinct(get(input$species))
      )
  })

  output$id_summary_table <- DT::renderDT({
    id_summary()
  })

  output$id_title <- renderText({
    req(uploaded_data(), input$id, input$id_summary_button)

    "Identifier summary"
  })

  # create the variable summaries
  # number of records per species
  species_summary <- reactive({
    req(uploaded_data(), input$species, input$species_summary_button)

    uploaded_data() %>%
      group_by(get(input$species)) %>% # get() needed because input$species is a character string
      summarise(n_records = n())
  })

  output$species_summary_table <- DT::renderDT({
    species_summary()
  })

  output$species_title <- renderText({
    req(uploaded_data(), input$species, input$species_summary_button)

    "Species summary"
  })

  # bounding box
  bbox <- reactive({
    req(uploaded_data(), input$lat, input$lon, input$coords_summary_button)

    uploaded_data() %>%
      select(input$lat, input$lon) %>%
      summarise(
        lat_min = min(eval(as.name(input$lat))),
        lat_max = max(eval(as.name(input$lat))),
        lon_min = min(eval(as.name(input$lon))),
        lon_max = max(eval(as.name(input$lon)))
      )
  })

  output$coords_summary_table <- DT::renderDT({
    bbox()
  })

  output$coords_title <- renderText({
    req(uploaded_data(), input$lat, input$lon, input$coords_summary_button)

    "Bounding box"
  })

  # Grid References UI Dynamic Insertion/Removal
  observeEvent(input$grid_ref, {
    if (input$grid_ref) {
      insertUI(
        selector = "#placeholder", where = "beforeEnd",
        ui = fluidRow(
          id = "dynamicUI",
          varSelectInput("grid_ref_column", "Grid Reference column", data = uploaded_data()),
          actionButton("grid_ref_convert", "Convert")
        )
      )
    } else {
      removeUI(selector = "#dynamicUI")
    }
  })

  # New reactive value to store lat/lon conversion results
  lat_lon_conversion <- reactiveVal(NULL)

  # Update the lat_lon_conversion upon grid_ref conversion
  observeEvent(input$grid_ref_convert, {
    req(input$grid_ref_column)

    sites <- pull(uploaded_data(), eval(as.symbol(input$grid_ref_column)))

    # Assuming 'lat_lon' holds the conversion logic returning a dataframe with 'lat' and 'lon'
    conversion_result <- osg_parse(grid_refs = sites, coord_system = "WGS84")

    lat_lon_conversion(conversion_result)
  })

  reformatted_data <- reactive({
    req(uploaded_data(), ) # Ensure there's uploaded data

    data <- uploaded_data()

    # Apply date formatting based on user selection
    if (!is.null(input$date)){
    if (input$date_format == "format_a") {
      date <- lubridate::dmy(data[[input$date]], quiet = TRUE)
    } else if (input$date_format == "format_b") {
      date <- lubridate::mdy(data[[input$date]], quiet = TRUE)
    } else if (input$date_format == "format_c") {
      date <- lubridate::ymd(data[[input$date]], quiet = TRUE)
    }}
    else{
      date <- NA
    }

    # Merge lat_lon_conversion results if they exist
    if (!is.null(lat_lon_conversion())) {
      conversion_result <- lat_lon_conversion()
      # Ensure that the conversion_result has 'lat' and 'lon' columns
      if ("lat" %in% names(conversion_result) && "lon" %in% names(conversion_result)) {
        # Merge or replace the lat and lon columns in data with those from conversion_result
        data$lat <- conversion_result$lat
        data$lon <- conversion_result$lon
      }

      lon_lat_names = c("lat", "lon")
    }

    else{

      lon_lat_names = c(input$lat, input$lon)
    }

    # Select specified columns, including updated 'lat' and 'lon'
    cols_to_select <- c(input$species, input$date, input$id) %>%
      na.omit()

    if (any(c(input$species, input$date, input$id) != "") ){cbind(data.frame(date = date), select(data, all_of(c(cols_to_select, lon_lat_names))))}
  })

  output$formatted_data_table <- renderDT(
    {
      reformatted_data()
    },
    options = list(pageLength = 5)
  )

  # Render uploaded data table
  output$uploaded_data_table <- DT::renderDT(uploaded_data())

  # Load modules
  mod_data_tab_server("data_tab_1", uploaded_data = uploaded_data)
  mod_time_bias_tab_server("time_bias_tab_1", uploaded_data = uploaded_data)
  mod_species_bias_tab_server("species_bias_tab_1", uploaded_data = uploaded_data)
  mod_species_id_bias_tab_server("species_id_bias_tab_1", uploaded_data = uploaded_data)
  mod_rarity_bias_tab_server("rarity_bias_tab_1", uploaded_data = uploaded_data)
  mod_space_cov_tab_server("space_cov_tab_1")
  mod_space_bias_tab_server("space_bias_tab_1")
  mod_environment_bias_tab_server("environment_bias_tab_1")
  mod_export_tab_server("export_tab_1")
}
