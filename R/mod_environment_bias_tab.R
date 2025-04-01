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

      if (input$periodtype == "ranges") {
      ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
      year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
      periods <- lapply(year_ranges, function(range) {
        from <- range[1]
        to <- range[2]
        return(seq(from = from, to = to))
      })
    } else {
      periods <- sort(unique(cleaned_data$year))
    }

      req(reformatted_data(), input$n_breaks, input$env_var_column)

      positions = reformatted_data() %>% dplyr::select(easting, northing) %>% distinct()

      positions$monad = mapply(convert_bng_to_monad,
                                 positions$easting,
                                 positions$northing)

      env_data = aux_file %>%
      mutate(presence = ifelse(monad %in% positions$monad, 1, 0))
      
      env_bias = assessBias1D_modified(pop = env_data,
              breaks = input$n_breaks, 
              R = "presence",
              x = input$env_var_column,
              RNames = input$env_var_column)

      return(env_bias$plot)
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