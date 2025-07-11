#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr sf vroom
#' @noRd

app_server <- function(input, output, session) {

  # Increase memory to accommodate larger tables
  options(shiny.maxRequestSize = 500 * 1024^2)

  # Needed to make helper buttons responsive
  observe_helpers(help_dir = "help_md")

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

    data = as.data.frame(data)

    uploaded_data(data)
  })

  data_ready <- reactiveVal(FALSE)
  
  # reactive value to store northing and easting conversion results with default value. Placed higher to ensure reset when new data is uploaded
  conversion_result <- reactiveVal()

  observeEvent(input$upload, {
  data_ready(FALSE)
  shinyjs::reset("data_upload_inputs")
  shinyjs::reset("mod_environment_bias_tab")
  shinyjs::reset("mod_time_bias_tab")
  shinyjs::reset("mod_species_rarity_bias_tab")
  shinyjs::reset("mod_species_id_bias_tab")
  shinyjs::reset("mod_species_bias_tab")
  shinyjs::reset("mod_space_cov_tab")
  shinyjs::reset("mod_space_bias_tab")

  conversion_result(NULL)
  data_ready(TRUE)
  })

  # Generate UI elements for y coordinate and x_coordinate inputs dynamically using selectInput
  output$y_coordinate_x_coordinate <- renderUI({

    if (!input$grid_ref) {
      tagList(

        ## please add documentation ##
        tags$p("NOTE: This can be any CRS. It must however match any spatial parameters you specify later, such as spatial resolution and uncertainty."),

        selectInput("y_coordinate", "y coordinate column", choices = c(), selected = FALSE),
        selectInput("x_coordinate", "x coordinate column", choices = c(), selected = FALSE),
        # checkboxInput("convert_osgb36", "Are you using decimal degrees?")
      )
    } else {
      NULL  # Remove y_coordinate/x_coordinate inputs when grid reference conversion is selected
    }
  })

  # Generate UI elements for y coordinate and x_coordinate inputs dynamically using selectInput
  output$spatial_uncertainty <- renderUI({

    if (input$has_spatial_uncertainty) {
      tagList(
    selectInput(
      "spat_uncert", "Spatial Uncertainty column",
      choices = NULL) %>%
    helper(
        icon = "info-circle", colour = "black", 
        content = "spatial_uncertainty",
        type = "markdown"
    )
      )
    } else {
      NULL  # Remove y_coordinate/x_coordinate inputs when grid reference conversion is selected
    }
  })

  output$date_year_ui <- renderUI({

    if (!input$has_year_column) {
      tagList(
            selectInput("date", "Date column", choices = NULL),

            radioButtons("date_format", "Select date format (please ignore separator)",
              choices = c(
                "day/month/year" = "format_a",
                "month/day/year" = "format_b",
                "year/month/day" = "format_c"
              ),
              selected = "format_a"
            )
      )
    } else {
      selectInput("year", "Year column", choices = NULL)
    }
  })

  # Dynamically update variable selections based on the uploaded data
  observe({
    req(uploaded_data())  # Ensure uploaded_data is not NULL before updating selections

    col_choices <- colnames(uploaded_data())

    updateSelectInput(session, "species", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "id", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "grid_ref_column", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "y_coordinate", choices = col_choices, selected = FALSE)
    updateSelectInput(session, "x_coordinate", choices = col_choices, selected = FALSE)
  })
  
  observeEvent(input$has_spatial_uncertainty, {

    req(uploaded_data())

    col_choices <- colnames(uploaded_data())

    updateSelectInput(session, "spat_uncert", choices = col_choices, selected = FALSE)

  })

  observeEvent(input$grid_ref, {

    req(uploaded_data())

    if(!input$grid_ref){

    updateSelectInput(session, "y_coordinate", choices = colnames(uploaded_data()), selected = FALSE)
    updateSelectInput(session, "x_coordinate", choices = colnames(uploaded_data()), selected = FALSE)

    } else{

      NULL
    }

  })
  
  observeEvent(input$has_year_column, {

    req(uploaded_data())

    if(input$has_year_column){

      updateSelectInput(session, "year", choices = colnames(uploaded_data()), selected = FALSE)

    } else{

      updateSelectInput(session, "date", choices = colnames(uploaded_data()), selected = FALSE)
    }

  })

  # Dynamically update variable selections based on the uploaded data
  observe({
    req(uploaded_data())  # Ensure uploaded_data is not NULL before updating selections

    if(input$has_year_column){

      updateSelectInput(session, "year", choices = colnames(uploaded_data()), selected = FALSE)

    } else{

      updateSelectInput(session, "date", choices = colnames(uploaded_data()), selected = FALSE)
    }
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

      # Reset conversion result
      conversion_result(NULL)
      removeUI(selector = "#dynamicUI")
    }
  })

  observeEvent(input$grid_ref_convert, {
    req(input$grid_ref_column)

    sites <- pull(uploaded_data(), eval(as.symbol(input$grid_ref_column)))

    # convert to northing x_coordinate
    result <- osg_parse(grid_refs = sites, coord_system = "BNG")

    conversion_result(data.frame("y_coordinate" = result$northing, "x_coordinate" = result$easting))
  })

  reformatted_data <- reactive({
    req(uploaded_data(), data_ready()) # Ensure there's uploaded data

    data <- uploaded_data()

    if (!is.null(conversion_result()) && "y_coordinate" %in% names(conversion_result()) && "x_coordinate" %in% names(conversion_result())) {
      data$y_coordinate <- conversion_result()$y_coordinate
      data$x_coordinate <- conversion_result()$x_coordinate
      y_coordinate_x_coordinate_names <- as.character(c("y_coordinate", "x_coordinate"))
    } else {
      y_coordinate_x_coordinate_names <- as.character(c(input$y_coordinate, input$x_coordinate))
    }

    cols_to_select <- c(input$species, input$year, input$id)

    cols_to_select <- cols_to_select[!is.null(cols_to_select)]
    cols_to_select <- cols_to_select[cols_to_select != ""]

    if (length(cols_to_select) > 0 && all(cols_to_select %in% colnames(data))) {
      formatted_data <- dplyr::select(data, !!!syms(cols_to_select))

      if (nchar(input$species)) {
        formatted_data <- rename(formatted_data, species = !!sym(input$species))
      }

      if (input$has_year_column){

        if (!is.null(input$year)){

          if(nchar(input$year) > 0){

          formatted_data <- rename(formatted_data, year = !!sym(input$year))
        }

        }

      } else{

        if (nchar(input$date) > 0){

          dates = data %>% pull(!!sym(input$date))

          if (input$date_format == "format_a") {
          dates <- lubridate::dmy(dates, quiet = TRUE)
          } else if (input$date_format == "format_b") {
          dates <- lubridate::mdy(dates, quiet = TRUE)
          } else if (input$date_format == "format_c") {
          dates <- lubridate::ymd(dates, quiet = TRUE)
          }
          formatted_data$year <- year(dates)
        }

      }

      if (nchar(input$id) > 0) {
        formatted_data <- rename(formatted_data, identifier = !!sym(input$id))
      }
    } else {
      formatted_data <- data.frame()
    }

    if (length(y_coordinate_x_coordinate_names) == 2) {

      if (nrow(formatted_data) == 0) {
        formatted_data <- dplyr::select(data, !!!syms(y_coordinate_x_coordinate_names))
      } else {
        formatted_data <- cbind(formatted_data, dplyr::select(data, !!!syms(y_coordinate_x_coordinate_names)))
      }

      formatted_data <- rename(formatted_data, y_coordinate = !!sym(y_coordinate_x_coordinate_names[1]))
      formatted_data <- rename(formatted_data, x_coordinate = !!sym(y_coordinate_x_coordinate_names[2]))

    }
    
    if(input$has_spatial_uncertainty && !is.null(input$spat_uncert) && input$spat_uncert != ""){


        if (nrow(formatted_data) == 0) {
          formatted_data <- dplyr::select(data, input$spat_uncert)
        } else {
          formatted_data = formatted_data %>% mutate(spatial_uncertainty = data %>% pull(input$spat_uncert))
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
    y_coordinate = NULL,
    x_coordinate = NULL,
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
    input_tracker$y_coordinate <- input$y_coordinate
  })

  observe({
    input_tracker$x_coordinate <- input$x_coordinate
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
      y_coordinate = input_tracker$y_coordinate,
      x_coordinate = input_tracker$x_coordinate,
      grid_ref = input_tracker$grid_ref,
      grid_ref_convert = input_tracker$grid_ref_convert,
      grid_ref_column = input_tracker$grid_ref_column
    )
  })

  # Are we developing locally?
  dev = FALSE

  # Create a server for the export
  tmp_dir <- tempdir() # file.path(getwd(), "export_dir") # tempdir()
  dir.create(file.path(tmp_dir, "export"), recursive = TRUE)

  # Load modules
  mod_info_tab_server("info_tab_1")
  mod_data_tab_server(id = "data_tab_1", user_selections = user_selections, uploaded_data = uploaded_data, reformatted_data = reformatted_data)
  mod_time_bias_tab_server("time_bias_tab_1", reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)

  mod_species_bias_tab_server("species_bias_tab_1", reformatted_data = reformatted_data, uploaded_data = uploaded_data, tmp_dir = tmp_dir, dev = dev)
  mod_species_id_bias_tab_server("species_id_bias_tab_1", uploaded_data = uploaded_data, reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)
  mod_species_rarity_bias_tab_server("species_rarity_bias_tab_1", uploaded_data = uploaded_data, reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)

  mod_space_cov_tab_server("space_cov_tab_1", reformatted_data = reformatted_data, iso_2_country_names = iso_2_country_names, countriesLow = countriesLow, tmp_dir = tmp_dir, dev = dev)
  mod_space_bias_tab_server("space_bias_tab_1", uploaded_data = uploaded_data, reformatted_data = reformatted_data, iso_2_country_names = iso_2_country_names, countriesLow = countriesLow, tmp_dir = tmp_dir, dev = dev)
  
  mod_environment_bias_tab_server("environment_bias_tab_1", reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)
  
}
