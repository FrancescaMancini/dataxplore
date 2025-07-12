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

  data_ready <- reactiveVal()
  
  # reactive value to store northing and easting conversion results with default value. Placed higher to ensure reset when new data is uploaded
  conversion_result <- reactiveVal()

  observeEvent(input$upload, {
  shinyjs::reset("data_upload_inputs")
  shinyjs::reset("mod_environment_bias_tab")
  shinyjs::reset("mod_time_bias_tab")
  shinyjs::reset("mod_species_rarity_bias_tab")
  shinyjs::reset("mod_species_id_bias_tab")
  shinyjs::reset("mod_species_bias_tab")
  shinyjs::reset("mod_space_cov_tab")
  shinyjs::reset("mod_space_bias_tab")
  
  data_ready(TRUE)

  })

  # Generate UI elements for y coordinate and x_coordinate inputs dynamically using varSelectInput
  output$y_coordinate_x_coordinate <- renderUI({

    if (!input$grid_ref) {
      tagList(

        tags$p("NOTE: This can be any CRS. It must however match with the spatial metrics you specify later, including the spatial resolution and uncertainty."),

        varSelectInput("y_coordinate", "y coordinate column", data = data.frame(), selected = character(0),  selectize = FALSE),
        varSelectInput("x_coordinate", "x coordinate column", data = data.frame(), selected = character(0),  selectize = FALSE)
      )
    } else {
      NULL  # Remove y_coordinate/x_coordinate inputs when grid reference conversion is selected
    }
  })

  output$date_year_ui <- renderUI({

    if (!input$has_year_column) {
      tagList(
            varSelectInput("date", "Date column", data = data.frame(), selected = character(0),  selectize = FALSE),

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
      varSelectInput("year", "Year column", data = data.frame(), selected = character(0),  selectize = FALSE)
    }
  })

  # Dynamically update variable selections based on the uploaded data
  observe({
    req(uploaded_data())  # Ensure uploaded_data is not NULL before updating selections

    updateVarSelectInput(session, "species", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "id", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "grid_ref_column", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "y_coordinate", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "x_coordinate", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "spat_uncert", data = uploaded_data(), selected = character(0))
  })

  observeEvent(input$grid_ref, {

    req(uploaded_data())

    if(!input$grid_ref){

    updateVarSelectInput(session, "y_coordinate", data = uploaded_data(), selected = character(0))
    updateVarSelectInput(session, "x_coordinate", data = uploaded_data(), selected = character(0))

    } else{

      NULL
    }

  })
  
  observeEvent(input$has_year_column, {

    req(uploaded_data())

    if(input$has_year_column){

      updateVarSelectInput(session, "year", data = uploaded_data(), selected = character(0))

    } else{

      updateVarSelectInput(session, "date", data = uploaded_data(), selected = character(0))
    }

  })

  # Dynamically update variable selections based on the uploaded data
  observe({
    req(uploaded_data())  # Ensure uploaded_data is not NULL before updating selections

    if(input$has_year_column){

      updateVarSelectInput(session, "year", data = uploaded_data(), selected = character(0))

    } else{

      updateVarSelectInput(session, "date", data = uploaded_data(), selected = character(0))
    }
  })

observeEvent(input$grid_ref, {
  if (input$grid_ref) {
    # Insert UI
    insertUI(
      selector = "#placeholder", where = "beforeEnd",
      ui = fluidRow(
        id = "dynamicUI",
        varSelectInput("grid_ref_column", "Grid Reference column", data = data.frame(), selected = character(0), selectize = FALSE),
        actionButton("grid_ref_convert", "Convert")
      )
    )

    # Defer update slightly so UI is rendered before update call
    shinyjs::delay(50, {
      req(uploaded_data())
      updateVarSelectInput(
        session,
        inputId = "grid_ref_column",
        data = uploaded_data(),
        selected = character(0)
      )
    })

  } else {
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

    data <- data.frame()

    # latitude and longitude
    if(!is.null(conversion_result())){

      data <- create_column_if_exists(data, "y_coordinate", conversion_result(), "y_coordinate")
      data <- create_column_if_exists(data, "x_coordinate", conversion_result(), "x_coordinate")
    }

    data <- create_column_if_exists(data, "y_coordinate", uploaded_data(), input$y_coordinate)
    data <- create_column_if_exists(data, "x_coordinate", uploaded_data(), input$x_coordinate)

    # species, identifier, and spatial uncertainty
    data <- create_column_if_exists(data, "species", uploaded_data(), input$species)
    data <- create_column_if_exists(data, "identifier", uploaded_data(), input$id)
    data <- create_column_if_exists(data, "spatial_uncertainty", uploaded_data(), input$spat_uncert)

    # year or date
    data <- create_column_if_exists(data, "year", uploaded_data(), input$year)

    if(!is.null(input$date) && as.character(input$date) %in% colnames(uploaded_data())){

      dates = uploaded_data() %>% pull(!!sym(input$date))

      if (input$date_format == "format_a") {
      years <- year( lubridate::dmy(dates, quiet = TRUE))
      } else if (input$date_format == "format_b") {
      years <- year(lubridate::mdy(dates, quiet = TRUE))
      } else if (input$date_format == "format_c") {
      years <- year(lubridate::ymd(dates, quiet = TRUE))
      }

      if(!all(is.na(years))){
        years_df = data.frame("year_insert" = years)
        data <- create_column_if_exists(data, "year", years_df, "year_insert")
      }

    }

    return(data)

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

  # Are we developing locally?
  dev = FALSE

  # Create a server for the export
  tmp_dir <- tempdir() # file.path(getwd(), "export_dir") # tempdir()
  dir.create(file.path(tmp_dir, "export"), recursive = TRUE)

  # Load modules
  mod_info_tab_server("info_tab_1")
  mod_data_tab_server(id = "data_tab_1", uploaded_data = uploaded_data, reformatted_data = reformatted_data)
  mod_time_bias_tab_server("time_bias_tab_1", reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)

  mod_species_bias_tab_server("species_bias_tab_1", reformatted_data = reformatted_data, uploaded_data = uploaded_data, tmp_dir = tmp_dir, dev = dev)
  mod_species_id_bias_tab_server("species_id_bias_tab_1", uploaded_data = uploaded_data, reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)
  mod_species_rarity_bias_tab_server("species_rarity_bias_tab_1", uploaded_data = uploaded_data, reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)

  mod_space_cov_tab_server("space_cov_tab_1", reformatted_data = reformatted_data, iso_2_country_names = iso_2_country_names, countriesLow = countriesLow, tmp_dir = tmp_dir, dev = dev)
  mod_space_bias_tab_server("space_bias_tab_1", uploaded_data = uploaded_data, reformatted_data = reformatted_data, iso_2_country_names = iso_2_country_names, countriesLow = countriesLow, tmp_dir = tmp_dir, dev = dev)
  
  mod_environment_bias_tab_server("environment_bias_tab_1", reformatted_data = reformatted_data, tmp_dir = tmp_dir, dev = dev)
  
}
