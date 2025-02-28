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

  # Generate UI elements for northing and easting inputs dynamically using selectInput
  output$northing_easting_ui <- renderUI({

    if (!input$grid_ref) {
      tagList(
        selectInput("northing", "Northing column", choices = c(), selected = FALSE),
        selectInput("easting", "Easting column", choices = c(), selected = FALSE),
        checkboxInput("convert_osgb36", "Are you using decimal degrees?")
      )
    } else {
      NULL  # Remove northing/easting inputs when grid reference conversion is selected
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
    updateSelectInput(session, "northing", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "easting", choices = col_choices, selected = FALSE)
  })

  observeEvent(input$grid_ref, {

    req(uploaded_data())

    updateSelectInput(session, "northing", choices = colnames(uploaded_data()), selected = FALSE)
    updateSelectInput(session, "easting", choices = colnames(uploaded_data()), selected = FALSE)

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

  # New reactive value to store northing/easting conversion results with default value
  conversion_result <- reactiveVal()

  observeEvent(input$grid_ref_convert, {
    req(input$grid_ref_column)

    sites <- pull(uploaded_data(), eval(as.symbol(input$grid_ref_column)))

    # Assuming 'osg_parse' is a function that converts grid references to northing/easting
    result <- osg_parse(grid_refs = sites, coord_system = "BNG")

    conversion_result(data.frame("northing" = result$northing, "easting" = result$easting))
  })

  reformatted_data <- reactive({
    req(uploaded_data()) # Ensure there's uploaded data

    data <- uploaded_data()

    if (!is.null(conversion_result()) && "northing" %in% names(conversion_result()) && "easting" %in% names(conversion_result())) {
      data$northing <- conversion_result()$northing
      data$easting <- conversion_result()$easting
      northing_easting_names <- as.character(c("northing", "easting"))
    } else {
      northing_easting_names <- as.character(c(input$northing, input$easting))
    }

    cols_to_select <- c(input$species, input$date, input$id)
    cols_to_select <- na.omit(cols_to_select)

    if (length(cols_to_select) > 0) {
      formatted_data <- dplyr::select(data, !!!syms(cols_to_select))

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

    if (length(northing_easting_names) == 2) {

      if (nrow(formatted_data) == 0) {
        formatted_data <- dplyr::select(data, !!!syms(northing_easting_names))
      } else {
        formatted_data <- cbind(formatted_data, dplyr::select(data, !!!syms(northing_easting_names)))
      }

      formatted_data <- rename(formatted_data, northing = !!sym(northing_easting_names[1]))
      formatted_data <- rename(formatted_data, easting = !!sym(northing_easting_names[2]))

    }

    if (!input$grid_ref){

      if(input$convert_osgb36 && all(northing_easting_names != "")){

      # Convert to an sf object
      sf_data <- st_as_sf(data, coords = c(northing_easting_names[2], northing_easting_names[1]), crs = 4326)

      # Transform to British National Grid (EPSG:27700)
      sf_data_bng <- st_transform(sf_data, crs = 27700)

      # Extract only the transformed coordinates as a new data frame
      df_bng <- as.data.frame(st_coordinates(sf_data_bng))

      df_bng = rename(df_bng, northing = Y, easting = X) %>%
      dplyr::select(northing, easting)

      formatted_data = formatted_data %>% dplyr::select(-northing, -easting) %>% cbind(df_bng)

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
    northing = NULL,
    easting = NULL,
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
    input_tracker$northing <- input$northing
  })

  observe({
    input_tracker$easting <- input$easting
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
      northing = input_tracker$northing,
      easting = input_tracker$easting,
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
