#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr
#' @noRd

app_server <- function(input, output, session) {

  # increase memory to accommodate larger tables
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

  # Dynamically update variable selections based on the uploaded data
  observeEvent(uploaded_data(), {
    updateVarSelectInput(session, "species", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "date", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "lon", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "lat", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "id", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "grid_ref_column", data = uploaded_data(), selected = character(0))
  })

  # Generate UI elements for Latitude and Longitude inputs dynamically
  output$lat_lon_ui <- renderUI({
    if (!input$grid_ref) {
      tagList(
        varSelectInput("lat", "Latitude column", data = uploaded_data()),
        varSelectInput("lon", "Longitude column", data = uploaded_data())
      )
    }
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

  # New reactive value to store lat/lon conversion results with default value
  conversion_result <- reactiveVal(data.frame(lat = NA, lon = NA))

  observeEvent(input$grid_ref_convert, {
    req(input$grid_ref_column)

    sites <- pull(uploaded_data(), eval(as.symbol(input$grid_ref_column)))

    # Assuming 'osg_parse' is a function that converts grid references to lat/lon
    result <- osg_parse(grid_refs = sites, coord_system = "WGS84")
    conversion_result(result)
  })

  lat_lon_conversion <- reactive({
    conversion_result()
  })

  reformatted_data <- reactive({
    req(uploaded_data()) # Ensure there's uploaded data

    data <- uploaded_data()
    conversion_result <- lat_lon_conversion()

    if (!is.null(conversion_result) && "lat" %in% names(conversion_result) && "lon" %in% names(conversion_result)) {
      data$lat <- conversion_result$lat
      data$lon <- conversion_result$lon
      lon_lat_names <- c("lat", "lon")
    } else {
      lon_lat_names <- as.character(c(input$lat, input$lon))
    }

    cols_to_select <- c(input$species, input$date, input$id)
    cols_to_select <- na.omit(cols_to_select)

    if (length(cols_to_select) > 0) {
      formatted_data <- select(data, !!!syms(cols_to_select))

      if (!is.null(input$species)) {
        formatted_data <- rename(formatted_data, species = !!sym(input$species))
      }
      if (!is.null(input$date)) {
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
      if (!is.null(input$id)) {
        formatted_data <- rename(formatted_data, identifier = !!sym(input$id))
      }
    } else {
      formatted_data <- data.frame()
    }

    if (length(lon_lat_names) == 2) {
      if (nrow(formatted_data) == 0) {
        formatted_data <- select(data, !!!syms(lon_lat_names))
      } else {
        formatted_data <- cbind(formatted_data, select(data, !!!syms(lon_lat_names)))
      }

      formatted_data <- rename(formatted_data, latitude = !!sym(lon_lat_names[1]))
      formatted_data <- rename(formatted_data, longitude = !!sym(lon_lat_names[2]))
    }

    formatted_data
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

  # Note we save module_outputs to extract spatial uncertainty
  module_outputs <- mod_species_bias_tab_server("species_bias_tab_1", reformatted_data = reformatted_data, uploaded_data = uploaded_data)
  mod_species_id_bias_tab_server("species_id_bias_tab_1", uploaded_data = uploaded_data, module_outputs = module_outputs, reformatted_data = reformatted_data)
  mod_species_rarity_bias_tab_server("species_rarity_bias_tab_1", uploaded_data = uploaded_data, module_outputs = module_outputs, reformatted_data = reformatted_data)

  mod_space_cov_tab_server("space_cov_tab_1", reformatted_data = reformatted_data)
  # mod_space_bias_tab_server("space_bias_tab_1")
  # mod_environment_bias_tab_server("environment_bias_tab_1")
  # mod_export_tab_server("export_tab_1")
}
