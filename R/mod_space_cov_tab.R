#' space_cov_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom methods as
#' @import sp
#'
mod_space_cov_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          ns("periodtype"), "Time periods as",
          choiceNames = list("Years", "Year ranges"),
          choiceValues = list("years", "ranges"),
          selected = "years"
        ),
        uiOutput(ns("numUI")),
        uiOutput(ns("dateRangesUI")),
        numericInput(ns("res"), "Spatial resolution", value = 1000),
        # selectInput(ns("country"), "Country", c("UK", "England", "Wales", "Scotland")),
        fileInput(ns("shapefile"), "Select your shapefile and file extensions.",
                  accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple = TRUE),
        selectInput(ns("log"), "Log count", c("TRUE", "FALSE"), selected = FALSE),
        selectInput(ns("output"), "Output", c("density", "Overlap", "Number of periods")),
        actionButton(ns("plot_button"), "Plot"),
        checkboxInput(ns("report"), "Add to report", FALSE)
      ),
      mainPanel(
        h2(span("Spatial coverage"),
           tooltip(
              bs_icon("info-circle"),
              "If output is density, the maps show the density of records in each grid cell per time period.
              If output is number of periods, it returns one map showing the number of time periods in which each grid cell has been sampled.",
              placement = "bottom"
            )
        ),
        plotOutput(ns("space_cov_plot"))
      )
    )
  )
}

#' space_cov_tab Server Functions
#'
#' @noRd
mod_space_cov_tab_server <- function(id, reformatted_data){
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

    sp_df <- reactive({
      req(input$shapefile)

      withProgress(message = "Loading shapefile...", value = 0, {
        # Temporary directory where files are uploaded
        tempdirname <- dirname(input$shapefile$datapath[1])

        # Rename files
        for (i in 1:nrow(input$shapefile)) {
          file.rename(
            input$shapefile$datapath[i],
            paste0(tempdirname, "/", input$shapefile$name[i])
          )
        }

        incProgress(0.5, detail = "Reading shapefile...")

        # Read the shapefile using st_read from sf package
        shape_input <- sf::st_read(paste(tempdirname,
                                     input$shapefile$name[grep(pattern = "*.shp$", input$shapefile$name)],
                                     sep = "/"))

        incProgress(1, detail = "Converting to Spatial format...")

        return(as(shape_input, "Spatial"))
      })
    })

    plot_data <- eventReactive(input$plot_button, {
      withProgress(message = 'Generating plot...', value = 0, {
        req(input$res, input$output, input$shapefile)

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

        incProgress(0.6, detail = "Calculating spatial coverage...")
        spat_cov <- assessSpatialCov(
          dat = cleaned_data,
          periods = periods,
          res = input$res,
          logCount = input$log,
          shp = sp_df(),
          species = "species",
          x = "longitude",
          y = "latitude",
          year = "year",
          spatialUncertainty = NULL,
          maxSpatUncertainty = NULL,
          identifier = "identifier",
          output = input$output
        )

        incProgress(0.8, detail = "Finalizing plot...")

        plot <- do.call(ggpubr::ggarrange, spat_cov)

        incProgress(1, detail = "Plot ready!")
        
        list(plot = plot)
      })
    })

    output$space_cov_plot <- renderPlot({
      plot_data()$plot
    })

    return(reactive(list(
      sp_df = sp_df()
    )))
  })
}
