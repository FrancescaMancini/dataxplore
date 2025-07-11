#' environment_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom zip zipr
#' @import occAssess sf
mod_environment_bias_tab_ui <- function(id){
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        div(id = "mod_environment_bias_tab",
        radioButtons(
          ns("periodtype"), "Time periods as",
          choiceNames = list("Years", "Year ranges"),
          choiceValues = list("years", "ranges"),
          selected = "years"
        ) %>%
        helper(icon = "info-circle", colour = "black",
          content = "time_period",
          type = "markdown"),
        uiOutput(ns("numUI")),
        uiOutput(ns("dateRangesUI")),
        numericInput(ns("n_breaks"), "Number of breaks", value = 50) %>%
          helper(icon = "info-circle", colour = "black",
                  content = "breaks",
                  type = "markdown"),
        ## please add documentation ##
        numericInput(ns("crs"), "Enter your data CRS (note, longitude/latitude = 4326, easting/northing = 27700)", value = 27700),
        selectInput(ns("env_var_column"), "Environmental variables column", choices = NULL, selected = FALSE) %>%
          helper(icon = "info-circle", colour = "black",
                  content = "environmental_variables",
                  type = "markdown"),
        actionButton(ns("plot_button"), "Plot"),
        downloadButton(ns("export_report"), "Export Report")
      )),
      mainPanel(
        h2("Environmental bias"),
        p("This function compares the distribution of some environmental variable in the sample (your data) to its distribution in the population (i.e. the whole geographic domain). It is based on the fact that a sample is representative, at least in terms of the focal variable, if the sample and population distributions are similar. Some environmental data is provided (see the tooltip for the “Environmental variables” drop down menu for details), or the user can upload their own data."),
        plotOutput(ns("env_bias_plot"))
      )
    )
  )

}

#' environment_bias_tab Server Functions
#'
#' @noRd
mod_environment_bias_tab_server <- function(id, reformatted_data, tmp_dir, dev){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

     output$numUI <- renderUI({
      req(input$periodtype == "ranges")
      numericInput(
        ns("num"), "Time periods",
        value = 1, min = 1, max = Inf
      )
    })

    output$dateRangesUI <- renderUI({
      req(input$periodtype == "ranges", input$num)

      min_year <- reformatted_data() %>%
        summarise(min_year = min(year, na.rm = TRUE)) %>%
        pull(min_year)
      max_year <- reformatted_data() %>%
        summarise(max_year = max(year, na.rm = TRUE)) %>%
        pull(max_year)

      dateRanges <- lapply(1:input$num, function(i) {
        numericRangeInput(ns(paste0("dates_", i)),
          label = paste("Year range", i),
          value = c(min_year, max_year)
        )
      })
      tagList(dateRanges)
    })

  observe({
    # Create a named vector: names = descriptions, values = auxcolumn codes
    choices <- setNames(variable_descriptions$auxcolumn, variable_descriptions$description)

    updateSelectInput(session, "env_var_column", choices = choices)
  })

convert_x_coordinate_y_coordinate_to_monad  <- function(x_coordinate, y_coordinate, crs) {

  # Convert to OSGB36 eastings/northings if coordinates are lat/lon
  crs_obj <- st_crs(crs)

  if (crs_obj$IsGeographic) {
    coords_sf <- st_as_sf(data.frame(lon = x_coordinate, lat = y_coordinate),
                          coords = c("lon", "lat"),
                          crs = crs)
    coords_osgb <- st_transform(coords_sf, 27700)
    xy <- st_coordinates(coords_osgb)
    x_coordinate <- xy[, "X"]
    y_coordinate <- xy[, "Y"]
  }

  # OSGB 100k grid letter lookup (A-Z, skipping I)
  grid_letters <- matrix(LETTERS[-9], ncol = 5, byrow = TRUE)[5:1, ]

  # First letter: 500 km grid square
  xo_500 <- trunc(x_coordinate / 500000) + 3
  yo_500 <- trunc(y_coordinate / 500000) + 2
  s1 <- grid_letters[cbind(yo_500, xo_500)]

  # Second letter: 100 km grid square
  xo_100 <- trunc((x_coordinate %% 500000) / 100000) + 1
  yo_100 <- trunc((y_coordinate %% 500000) / 100000) + 1
  s2 <- grid_letters[cbind(yo_100, xo_100)]

  # Numeric part: 1 km square (monad)
  monad_e <- trunc((x_coordinate %% 100000) / 1000)
  monad_n <- trunc((y_coordinate %% 100000) / 1000)

  # Combine into full monad code
  monads <- paste0(s1, s2, sprintf("%02d%02d", monad_e, monad_n))
  return(monads)
}

plot <- eventReactive(input$plot_button, {
  withProgress(message = "Generating plot...", value = 0, {
    req(reformatted_data(), input$n_breaks, input$env_var_column)

    incProgress(0.1, detail = "Processing time periods...")
    # 1. Define periods
    if (input$periodtype == "ranges") {
      ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
      year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
      periods <- lapply(year_ranges, function(range) seq(from = range[1], to = range[2]))
    } else {
      unique_years_range <- range(unique(reformatted_data()$year), na.rm = TRUE)
      periods <- list(seq(from = unique_years_range[1], to = unique_years_range[2]))
    }

    incProgress(0.3, detail = "Mapping records to monads...")

    # 2. Convert WGS84 to monads
    positions <- reformatted_data() %>%
      dplyr::select(x_coordinate, y_coordinate, year) %>%
      mutate(monad = convert_x_coordinate_y_coordinate_to_monad(x_coordinate, y_coordinate, crs = input$crs))

    incProgress(0.5, detail = "Preparing environmental data...")

    env_data <- aux_file %>%
    filter(auxcolumn == input$env_var_column) %>%
    dplyr::select(monad, value) %>%
    rename(!!input$env_var_column := value)

    # 3. Add presence column per period
    for (i in seq_along(periods)) {
      period_years <- periods[[i]]

      monads_in_period <- positions %>%
        filter(year %in% period_years) %>%
        distinct(monad) %>%
        pull(monad)

      if (length(monads_in_period) == 0) {
        showNotification(paste("No monads found in period", i), type = "warning", duration = 6)
      }

      colname <- paste0("presence_", i)
      env_data[[colname]] <- ifelse(env_data$monad %in% monads_in_period, 1, 0)
    }

    incProgress(0.7, detail = "Calculating bias per period...")
    plots <- lapply(seq_along(periods), function(i) {

      presence_col <- paste0("presence_", i)

      assessBias1D(
        pop = env_data,
        breaks = input$n_breaks,
        R = presence_col,
        x = input$env_var_column,
        RNames = presence_col
      )$plot
    })

    incProgress(1, detail = "Plot ready!")
    combined_plot <- patchwork::wrap_plots(plots, ncol = 1)
    return(combined_plot)
  })
})

output$env_bias_plot <- renderPlot({
  plot()
  })

output$export_report <- downloadHandler(
  filename = function() {
    paste0("environment_bias_export_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
  },
  content = function(file) {
    withProgress(message = "Generating report...", value = 0, {
      req(reformatted_data(), input$n_breaks, input$env_var_column)

      incProgress(0.1, detail = "Defining time periods...")
      if (input$periodtype == "ranges") {
        ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
        year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
        periods <- lapply(year_ranges, function(range) seq(range[1], range[2]))
      } else {
        periods <- list(seq(min(reformatted_data()$year, na.rm = TRUE),
                            max(reformatted_data()$year, na.rm = TRUE)))
      }

      incProgress(0.3, detail = "Saving input datasets...")
      tmp_export_dir <- file.path(tmp_dir, "export")
      dir.create(tmp_export_dir, showWarnings = FALSE, recursive = TRUE)

      
      # Save datasets for export
      write.csv(reformatted_data(), file.path(tmp_export_dir, "your_formatted_data.csv"), row.names = FALSE)
      
      # Save aux_file as rds because it is too large as a csv for the posit server
      saveRDS(aux_file, file.path(tmp_export_dir, "aux_file.rds"))

      # Save the variable definitions
      write.csv(variable_descriptions, file.path(tmp_export_dir, "variable_descriptions.csv"), row.names = FALSE)

      incProgress(0.6, detail = "Rendering RMarkdown report...")
  
  result <- tryCatch({
    rmarkdown::render(
      input = get_markdown_path("mod_environment_bias_tab_report.Rmd", dev = dev),
      output_file = "environment_bias_report.html",
      output_dir = tmp_export_dir,
      params = list(
        periods = periods,
        n_breaks = input$n_breaks,
        env_var_column = input$env_var_column,
        crs = input$crs
      ),
      knit_root_dir = tmp_dir,
      envir = new.env(parent = globalenv())
    )
  }, error = function(e) {
    # Print full error object to server log
    print(e)                # Shows full error with class

    # Write error details to a log file (optional)
    dump_file <- file.path(tmp_export_dir, "render_error.rds")
    saveRDS(e, dump_file)

    showNotification("Report generation failed. Check logs for details.", type = "error")
    return(NULL)
  })


      incProgress(0.9, detail = "Zipping output...")
      zip::zipr(zipfile = file, files = list.files(tmp_export_dir, full.names = TRUE), root = tmp_export_dir)

      incProgress(1, detail = "Cleaning up...")
      unlink(tmp_export_dir, recursive = TRUE, force = TRUE)
    })
  }
)

  })
}
