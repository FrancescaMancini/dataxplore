#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr sf
#' @noRd

app_server <- function(input, output, session) {

  # Increase memory to accommodate larger tables
  options(shiny.maxRequestSize = 500 * 1024^2)

  uploaded_data <- reactiveVal()

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

  # Generate UI elements for Latitude and Longitude inputs dynamically using selectInput
  output$lat_lon_ui <- renderUI({

    if (!input$grid_ref) {
      tagList(
        selectInput("lat", "Latitude column", choices = c(), selected = FALSE),
        selectInput("lon", "Longitude column", choices = c(), selected = FALSE),
        checkboxInput("convert_osgb36", "Are you using decimal degrees?", FALSE)
      )
    } else {
      NULL  # Remove lat/lon inputs when grid reference conversion is selected
    }
  })

  # Dynamically update variable selections based on the uploaded data
  observe({
    req(uploaded_data())  # Ensure uploaded_data is not NULL before updating selections

    col_choices <- colnames(uploaded_data())

    updateSelectInput(session, "species", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "date", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "id", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "grid_ref_column", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "lat", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "lon", choices = col_choices, selected = FALSE)
  })

  observeEvent(input$grid_ref, {

    req(uploaded_data())

    updateSelectInput(session, "lat", choices = colnames(uploaded_data()), selected = FALSE)
    updateSelectInput(session, "lon", choices = colnames(uploaded_data()), selected = FALSE)

  })

  # Grid References UI Dynamic Insertion/Removal
  observeEvent(input$grid_ref, {
    if (input$grid_ref) {
      insertUI(
        selector = "#placeholder", where = "beforeEnd",
        ui = fluidRow(
          id = "dynamicUI",
          selectInput("grid_ref_column", "Grid Reference column", choices = colnames(uploaded_data())),
          actionButton("grid_ref_convert", "Convert")
        )
      )
    } else {
      removeUI(selector = "#dynamicUI")
    }
  })

  # New reactive value to store lat/lon conversion results with default value
  conversion_result <- reactiveVal()

  observeEvent(input$grid_ref_convert, {
    req(input$grid_ref_column)

    sites <- pull(uploaded_data(), eval(as.symbol(input$grid_ref_column)))

    # Assuming 'osg_parse' is a function that converts grid references to lat/lon
    result <- osg_parse(grid_refs = sites, coord_system = "BNG")

    conversion_result(data.frame("lat" = result$northing, "lon" = result$easting))
  })

  reformatted_data <- reactive({
    req(uploaded_data()) # Ensure there's uploaded data

    data <- uploaded_data()

    if (!is.null(conversion_result()) && "lat" %in% names(conversion_result()) && "lon" %in% names(conversion_result())) {
      data$lat <- conversion_result()$lat
      data$lon <- conversion_result()$lon
      lat_lon_names <- as.character(c("lat", "lon"))
    } else {
      lat_lon_names <- as.character(c(input$lat, input$lon))
    }

    cols_to_select <- c(input$species, input$date, input$id)
    cols_to_select <- na.omit(cols_to_select)

    if (length(cols_to_select) > 0) {
      formatted_data <- select(data, !!!syms(cols_to_select))

      if (nchar(input$species)) {
        formatted_data <- rename(formatted_data, species = !!sym(input$species))
      }

      if (nchar(input$date) > 0) {
        formatted_data <- rename(formatted_data, date = !!sym(input$date))

        if (input$date_format == "format_a") {
          formatted_data$date <- lubridate::dmy(formatted_data$date, quiet = TRUE)
        } else if (input$date_format == "format_b") {
          formatted_data$date <- lubridate::mdy(formatted_data$date, quiet = TRUE)
        } else if (input$date_format == "format_c") {
          formatted_data$date <- lubridate::ymd(formatted_data$date, quiet = TRUE)
        }
        formatted_data$year <- year(formatted_data$date)
      }
      if (nchar(input$id) > 0) {
        formatted_data <- rename(formatted_data, identifier = !!sym(input$id))
      }
    } else {
      formatted_data <- data.frame()
    }

    if (length(lat_lon_names) == 2) {

      if (nrow(formatted_data) == 0) {
        formatted_data <- select(data, !!!syms(lat_lon_names))
      } else {
        formatted_data <- cbind(formatted_data, select(data, !!!syms(lat_lon_names)))
      }

      formatted_data <- rename(formatted_data, latitude = !!sym(lat_lon_names[1]))
      formatted_data <- rename(formatted_data, longitude = !!sym(lat_lon_names[2]))

    }

    if (!input$grid_ref){

      if(input$convert_osgb36 && all(lat_lon_names != "")){

      # Convert to an sf object
      sf_data <- st_as_sf(data, coords = c(lat_lon_names[2], lat_lon_names[1]), crs = 4326)

      # Transform to British National Grid (EPSG:27700)
      sf_data_bng <- st_transform(sf_data, crs = 27700)

      # Extract only the transformed coordinates as a new data frame
      df_bng <- as.data.frame(st_coordinates(sf_data_bng))

      df_bng = rename(df_bng, latitude = Y, longitude = X) %>%
      select(latitude, longitude)

      formatted_data = formatted_data %>% select(-latitude, -longitude) %>% cbind(df_bng)

      }
    }

    return(formatted_data)
  })

  # Render uploaded data table
  output$uploaded_data_table <- DT::renderDT(uploaded_data())
  # Render formatted data table
  output$formatted_data_table <- DT::renderDT({
    req(uploaded_data())
    reformatted_data()
  })

  # Initialize reactiveValues
  input_tracker <- reactiveValues(
    species = NULL,
    date = NULL,
    date_format = NULL,
    year = NULL,
    id = NULL,
    lat = NULL,
    lon = NULL,
    grid_ref = NULL,
    grid_ref_convert = NULL,
    grid_ref_column = NULL
  )

  # Update reactiveValues when inputs change
  observe({
    input_tracker$species <- input$species
  })

  observe({
    input_tracker$date <- input$date
  })

  observe({
    input_tracker$date_format <- input$date_format
  })

  observe({
    input_tracker$year <- input$year
  })

  observe({
    input_tracker$id <- input$id
  })

  observe({
    input_tracker$lat <- input$lat
  })

  observe({
    input_tracker$lon <- input$lon
  })

  observe({
    input_tracker$grid_ref <- input$grid_ref
  })

  observe({
    input_tracker$grid_ref_convert <- input$grid_ref_convert
  })

  observe({
    input_tracker$grid_ref_column <- input$grid_ref_column
  })

  # Reactive wrapper for input_tracker
  user_selections <- reactive({
    list(
      species = input_tracker$species,
      date = input_tracker$date,
      date_format = input_tracker$date_format,
      year = input_tracker$year,
      id = input_tracker$id,
      lat = input_tracker$lat,
      lon = input_tracker$lon,
      grid_ref = input_tracker$grid_ref,
      grid_ref_convert = input_tracker$grid_ref_convert,
      grid_ref_column = input_tracker$grid_ref_column
    )
  })

  # Load modules
  mod_info_tab_server("info_tab_1")
  mod_data_tab_server(id = "data_tab_1", user_selections = user_selections, uploaded_data = uploaded_data)
  mod_time_bias_tab_server("time_bias_tab_1", reformatted_data = reformatted_data)

  module_outputs = list()

  # Note we save module_outputs to extract spatial uncertainty
  module_outputs$mod_species_bias_tab <- mod_species_bias_tab_server("species_bias_tab_1", reformatted_data = reformatted_data, uploaded_data = uploaded_data)
  mod_species_id_bias_tab_server("species_id_bias_tab_1", uploaded_data = uploaded_data, module_outputs = module_outputs, reformatted_data = reformatted_data)
  mod_species_rarity_bias_tab_server("species_rarity_bias_tab_1", uploaded_data = uploaded_data, module_outputs = module_outputs, reformatted_data = reformatted_data)

  module_outputs$mod_space_cov_tab <- mod_space_cov_tab_server("space_cov_tab_1", reformatted_data = reformatted_data)
  mod_space_bias_tab_server("space_bias_tab_1", module_outputs = module_outputs, uploaded_data = uploaded_data, reformatted_data = reformatted_data)
  # mod_environment_bias_tab_server("environment_bias_tab_1")
  # mod_export_tab_server("export_tab_1")
}
