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
      ),
      mainPanel(
        h2(span("Environmental bias"),
           tooltip(
              bs_icon("info-circle"),
              "Some text",
              placement = "bottom"
            )
        ),
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
    choices <- setNames(aux_file %>% distinct(auxcolumn, description) %>% pull(auxcolumn),
                        aux_file %>% distinct(auxcolumn, description) %>% pull(description))

    updateSelectInput(session, "env_var_column", choices = choices)
  })

convert_long_lat_to_monad <- function(longitude, latitude) {
  # Combine input and convert to sf
  coords <- data.frame(lon = longitude, lat = latitude)
  sf_points <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
  sf_bng <- sf::st_transform(sf_points, crs = 27700)
  bng_coords <- sf::st_coordinates(sf_bng)

  easting <- bng_coords[, 1]
  northing <- bng_coords[, 2]

  # OSGB 100k grid letter lookup (A-Z skipping I)
  m <- matrix(LETTERS[-9], ncol = 5, byrow = TRUE)[5:1, ]

  # Compute grid indices
  xo_500 <- trunc(easting / 500000) + 3
  yo_500 <- trunc(northing / 500000) + 2
  s1 <- m[cbind(yo_500, xo_500)]

  xo_100 <- trunc((easting %% 500000) / 100000) + 1
  yo_100 <- trunc((northing %% 500000) / 100000) + 1
  s2 <- m[cbind(yo_100, xo_100)]

  monad_e <- trunc((easting %% 100000) / 1000)
  monad_n <- trunc((northing %% 100000) / 1000)

  # Combine parts
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
      dplyr::select(longitude, latitude, year) %>%
      mutate(monad = convert_long_lat_to_monad(longitude, latitude))

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

  })
}

# assessBias1D_modified(pop = callunaData,
#                     breaks = 50, 
#                     R = "sampled_units_1987.1999",
#                     x = c("road_length_299_neighbours"),
#                     RNames = "test")