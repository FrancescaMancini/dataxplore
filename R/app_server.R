#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr
#' @noRd

app_server <- function(input, output, session) {
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

    # Merge lat_lon_conversion results if they exist
    if (!is.null(lat_lon_conversion())) {
      conversion_result <- lat_lon_conversion()
      # Ensure that the conversion_result has 'lat' and 'lon' columns
      if ("lat" %in% names(conversion_result) && "lon" %in% names(conversion_result)) {
        # Merge or replace the lat and lon columns in data with those from conversion_result
        data$lat <- conversion_result$lat
        data$lon <- conversion_result$lon
      }

      lon_lat_names <- c("lat", "lon")
    } else {
      lon_lat_names <- as.character(c(input$lat, input$lon))
    }

    # Select specified columns, including updated 'lat' and 'lon'
    cols_to_select <- sapply(c(input$species, input$date, input$id), FUN = "as.character", USE.NAMES = FALSE) %>% na.omit()

    if (length(cols_to_select) > 0) {
      formatted_data <- select(data, cols_to_select)

      if (!is.null(input$species)) {
        formatted_data <- rename(formatted_data, species = input$species)
      }
      if (!is.null(input$date)) {
        formatted_data <- rename(formatted_data, date = input$date)

        if (input$date_format == "format_a") {
          formatted_data$date <- lubridate::dmy(data[[input$date]], quiet = TRUE)
        } else if (input$date_format == "format_b") {
          formatted_data$date <- lubridate::mdy(data[[input$date]], quiet = TRUE)
        } else if (input$date_format == "format_c") {
          formatted_data$date <- lubridate::ymd(data[[input$date]], quiet = TRUE)
        }
        formatted_data$year <- year(formatted_data$date)
      }
      if (!is.null(input$id)) {
        formatted_data <- rename(formatted_data, identifier = input$id)
      }
    } else {
      formatted_data <- NULL
    }

    if (length(lon_lat_names) == 2) {
      if (is.null(formatted_data)) {
        formatted_data <- select(data, lon_lat_names)
      } else {
        formatted_data <- cbind(formatted_data, select(data, lon_lat_names))
      }

      formatted_data <- rename(formatted_data, latitude = lon_lat_names[1])
      formatted_data <- rename(formatted_data, longitude = lon_lat_names[2])
    }

    formatted_data
  })

  
  # Render uploaded data table
  output$uploaded_data_table <- DT::renderDT(uploaded_data())
  # Render formatted data table
  output$formatted_data_table <- DT::renderDT(reformatted_data())

  # Load modules
  mod_info_tab_server("info_tab_1")
  mod_data_tab_server("data_tab_1",
    uploaded_data = uploaded_data(),
    user_selections = reactive(list(
      species = input$species,
      species_summary_button = input$species_summary_button,
      date = input$date,
      date_summary_button = input$date_summary_button,
      date_format = input$date_format,
      year = input$year, year_summary_button = input$year_summary_button,
      id = input$id,
      id_summary_button = input$id_summary_button,
      lat = input$lat,
      lon = input$lon,
      input$coords_summary_button,
      grid_ref = input$grid_ref,
      grid_ref_convert = input$grid_ref_convert,
      grid_ref_column = input$grid_ref_column
    ))
  )
  # mod_time_bias_tab_server("time_bias_tab_1", uploaded_data = uploaded_data)
  # mod_species_bias_tab_server("species_bias_tab_1", uploaded_data = uploaded_data)
  # mod_species_id_bias_tab_server("species_id_bias_tab_1", uploaded_data = uploaded_data)
  # mod_rarity_bias_tab_server("rarity_bias_tab_1", uploaded_data = uploaded_data)
  # mod_space_cov_tab_server("space_cov_tab_1")
  # mod_space_bias_tab_server("space_bias_tab_1")
  # mod_environment_bias_tab_server("environment_bias_tab_1")
  # mod_export_tab_server("export_tab_1")
}
