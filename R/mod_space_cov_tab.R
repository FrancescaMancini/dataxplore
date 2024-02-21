#' space_cov_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_space_cov_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("num"),
                     "Time periods",
                     value = 1, min = 1, max = Inf
        ),
        # dateRangeInput(
          # "periods",
        # "Choose date range"
        # ),
        uiOutput(ns("dateRangesUI")
        ),
        selectInput(
          "species", "Species column",
          c("", "species", "date", "x", "y", "group", "year"), # eventually replace the values with colnames of the dataset
        ),
        selectInput(
          "lon", "Longitude column",
          c("", "species", "date", "x", "y", "group", "year"), # eventually replace the values with colnames of the dataset
        ),
        selectInput(
          "lat", "Latitude column",
          c("", "species", "date", "x", "y", "group", "year"), # eventually replace the values with colnames of the dataset
        ),
        textInput(
          "res", "Spatial resolution",
          value = 1000
        ),
        selectInput(
          "year", "Year column",
          c("", "species", "date", "x", "y", "group", "year"), # eventually replace the values with colnames of the dataset
        ),
        selectInput(
          "id", "Choose the identifier",
          c("", "species", "date", "x", "y", "group", "year"), # eventually replace the values with colnames of the dataset
        ),
        selectInput(
          "country", "Country",
          c("UK", "England", "Wales", "Scotland")
        ),
        fileInput("shapefile",
                  NULL
        ),
        selectInput(
          "log", "Log count",
          c("TRUE", "FALSE"), selected = FALSE
        ),
        selectInput(
          "output", "Output",
          c("Density", "Overlap", "Number of periods")
        ),
        actionButton(
          "plot_button", "Plot"
        ),
        # checkboxInput("code", "View R code"
        # ),
        checkboxInput("report", "Add to report", FALSE
        )
      ),
      mainPanel(
        h2(
          span(
            "Spatial coverage",
            tooltip(
              bs_icon("info-circle"),
              "If output is density, the maps show the density of records in each grid cell per time period.
              If output is number of periods, it returns one map showing the number of time periods in which each grid cell has been sampled.",
              placement = "bottom"
            )
          )),
        plotOutput(ns("space_cov_plot"))

      )
    )
  )
}

#' space_cov_tab Server Functions
#'
#' @noRd
mod_space_cov_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$dateRangesUI <- renderUI({
      num <- input$num
      dateRanges <- lapply(1:num, function(i) {
        dateRangeInput(paste0("dates_", i), label = paste("Date Range", i), start = NULL, end = NULL)
      })
      tagList(dateRanges)
    })

    output$space_cov_plot <- renderPlot({
      random_ggplot(type = "raster")
    })

  })
}

## To be copied in the UI

## To be copied in the server
