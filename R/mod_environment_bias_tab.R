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
                  content = "breaks",
                  type = "markdown"),
        selectInput(ns("env_var_column"), "Environmental variables column", choices = NULL, selected = FALSE) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "environmental_variables",
                  type = "markdown"),
        actionButton(ns("plot_button"), "Plot"),
        checkboxInput(ns("report"), "Add to report", FALSE)
      ),
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
        updateSelectInput(session, "env_var_column", choices = colnames(aux_file)[!colnames(aux_file) %in% c("x", "y", "monad")])
    })

    convert_bng_to_monad <- function(easting, northing) {
    # OSGB 100k grid letter lookup (A-Z skipping I)
    m <- matrix(LETTERS[-9], ncol = 5, byrow = TRUE)[5:1, ]
    
    # 500km square
    xo <- trunc(easting / 500000) + 3
    yo <- trunc(northing / 500000) + 2
    s1 <- m[yo, xo]
    
    # 100km square
    xo <- trunc((easting %% 500000) / 100000) + 1
    yo <- trunc((northing %% 500000) / 100000) + 1
    s2 <- m[yo, xo]
    
    # Monad (1 km square within 100k square)
    monad_easting <- trunc((easting %% 100000) / 1000)
    monad_northing <- trunc((northing %% 100000) / 1000)
    
    monad <- paste0(s1, s2, sprintf("%02d%02d", monad_easting, monad_northing))
    return(monad)
  }

plot <- eventReactive(input$plot_button, {
  req(reformatted_data(), input$n_breaks, input$env_var_column)

  # 1. Define periods based on input
  if (input$periodtype == "ranges") {
    ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
    year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
    periods <- lapply(year_ranges, function(range) seq(from = range[1], to = range[2]))
  } else {
    unique_years_range <- range(unique(reformatted_data()$year), na.rm = TRUE)
    periods <- list(seq(from = unique_years_range[1], to = unique_years_range[2]))
  }

  # 2. Convert easting/northing to monads in reformatted_data
  positions <- reformatted_data() %>%
    dplyr::select(easting, northing, year) %>%
    mutate(monad = mapply(convert_bng_to_monad, easting, northing))

  # 3. Create base environmental data with monads from aux_file
  env_data <- aux_file

  # 4. Add presence column for each period
  for (i in seq_along(periods)) {
    period_years <- periods[[i]]

    monads_in_period <- positions %>%
      filter(year %in% period_years) %>%
      distinct(monad) %>%
      pull(monad)

    colname <- paste0("presence_", i)
    env_data[[colname]] <- ifelse(env_data$monad %in% monads_in_period, 1, 0)
  }

  # 5. Call assessBias1D_modified for each period and combine plots
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

  # 6. Combine plots using patchwork
  library(patchwork)
  combined_plot <- wrap_plots(plots, ncol = 1)
  return(combined_plot)
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