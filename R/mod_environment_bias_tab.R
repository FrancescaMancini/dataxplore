#' environment_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
                  content = "time_period",
                  type = "markdown"),
        selectInput(ns("env_var_column"), "Environmental variables column", choices = NULL, selected = FALSE) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "time_period",
                  type = "markdown"),
        actionButton(ns("plot_button"), "Plot"),
        checkboxInput(ns("report"), "Add to report", FALSE)
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
mod_environment_bias_tab_server <- function(id, reformatted_data){
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

convert_x_coordinate_y_coordinate_to_monad <- function(x_coordinate, y_coordinate) {
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
      mutate(monad = convert_x_coordinate_y_coordinate_to_monad(x_coordinate, y_coordinate))

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
      
      assessBias1D_modified(
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

observeEvent(input$export_report, {
  req(reformatted_data(), input$n_breaks, input$env_var_column)

  if (input$periodtype == "ranges") {
    ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
    year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
    periods <- lapply(year_ranges, function(range) seq(range[1], range[2]))
  } else {
    periods <- list(seq(min(reformatted_data()$year, na.rm = TRUE), max(reformatted_data()$year, na.rm = TRUE)))
  }

  tmp_export_dir <- file.path(tmp_dir, "export")
  dir.create(tmp_export_dir, showWarnings = FALSE)

  # Save formatted data
  if (input$export_report) {

    save_ifnot_exists(reformatted_data(), file.path(tmp_dir, "export", "your_formatted_data.csv"))
    save_ifnot_exists(aux_file, file.path(tmp_dir, "export", "your_formatted_data.csv"))
    save_ifnot_exists(variable_descriptions, file.path(tmp_dir, "export", "your_formatted_data.csv"))

  }

  rmarkdown::render(
    input = "markdown_files/mod_environment_bias_tab_report.Rmd",
    output_file = "environment_bias_report.html",
    output_dir = tmp_export_dir,
    params = list(
      periods = periods,
      n_breaks = input$n_breaks,
      env_var_column = input$env_var_column
    ),
    knit_root_dir = tmp_dir,
    envir = new.env(parent = globalenv())
  )

  showNotification("Report generated. Navigate to the export tab to download the HTML.", type = "message")
})

  })
}

# assessBias1D_modified(pop = callunaData,
#                     breaks = 50, 
#                     R = "sampled_units_1987.1999",
#                     x = c("road_length_299_neighbours"),
#                     RNames = "test")