#' space_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import raster
#' 
mod_space_bias_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("spat_uncert"), "Spatial Uncertainty column",
          choices = NULL
        ),
        radioButtons(
        ns("periodtype"), "Time periods as",
        choiceNames = list("Years", "Year ranges"),
        choiceValues = list("years", "ranges"),
        selected = "years"
      ),
      uiOutput(ns("numUI")),
      uiOutput(ns("dateRangesUI")),
        numericInput(ns("num"),
                     "Time periods",
                     value = 1, min = 1, max = Inf
        ),
        numericInput(
          ns("nSamps"), "Number of iterations",
          value = 50
        ),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        checkboxInput(ns("report"), "Add to report", FALSE
        )
      ),
      mainPanel(
        h2(
          span(
            "Spatial bias",
            tooltip(
              bs_icon("info-circle"),
              "The plot shows spatial biased based on the Nearest Neighbour Index (NNI).
              The function simulates n datasets randomly across the study area in equal number to the occurrence data. The NNI can then be given as the ratio of the average observed nearest neighbour distances to the average of the simulated nearest neighbour distances.
              The index displayed in the plot can be interpreted as how far the observed distribution deviates from a random distribution of the same density.
              Values between 0 and 1 are more clustered than a random distribution, and values between 1 and 2.15 are more widely dispersed
              ",
              placement = "bottom"
            )
          )),
        plotOutput(ns("space_bias_plot"))

      )
    )
  )
}

#' space_bias_tab Server Functions
#'
#' @noRd
mod_space_bias_tab_server <- function(id, module_outputs, uploaded_data, reformatted_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$numUI <- renderUI({
      req(input$periodtype == "ranges")
      numericInput(
        ns("num"), "Time periods",
        value = 1, min = 1, max = Inf
      )
    })

    output$dateRangesUI <- renderUI({
      req(input$periodtype == "ranges")
      req(input$num)

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

    observeEvent(uploaded_data(), {
      updateSelectInput(session, "spat_uncert",
                        choices = names(uploaded_data()),
                        selected = character(0)
      )
    })

    plot_data <- eventReactive(input$plot_button, {
      withProgress(message = 'Generating plot...', value = 0, {
        req(reformatted_data(), input$nSamps, module_outputs$mod_space_cov_tab()$sp_df)

        incProgress(0.2, detail = "Cleaning data...")
        cleaned_data <- reformatted_data() %>%
          filter(!is.na(year))

        num_filtered <- nrow(reformatted_data()) - nrow(cleaned_data)
        if (num_filtered > 0) {
          showNotification(paste(num_filtered, "rows with NA values in the year column were removed."), type = "warning")
        }

        incProgress(0.4, detail = "Processing time periods...")
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

        incProgress(0.6, detail = "Creating raster mask...")
        mask <- rasterize(module_outputs$mod_space_cov_tab()$sp_df, 
                          raster(nrow = 1000, ncol = 1000, extent(module_outputs$mod_space_cov_tab()$sp_df)))

        incProgress(0.8, detail = "Calculating spatial bias...")

        data <- reformatted_data() %>%
          mutate("spat_uncert" = uploaded_data() %>% pull(input$spat_uncert))

        plot <- assessSpatialBias(dat = data,
                                  periods = periods,
                                  mask = mask,
                                  nSamps = input$nSamps,
                                  degrade = TRUE,
                                  species = "species",
                                  x = "longitude",
                                  y = "latitude",
                                  year = "year", 
                                  spatialUncertainty = "spat_uncert",
                                  identifier = "identifier")$plot

        incProgress(1, detail = "Finalizing plot...")
        
        list(plot = plot)
      })
    })

    output$space_bias_plot <- renderPlot({
      plot_data()$plot
    })
  })
}
