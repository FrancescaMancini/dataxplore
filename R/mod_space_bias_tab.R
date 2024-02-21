#' space_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_space_bias_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("num"),
                     "Time periods",
                     value = 1, min = 1, max = Inf
        ),
        uiOutput(ns("dateRangesUI")
                 ),
        selectInput(
          "species", "Species column",
          c("", "species", "date", "x", "y", "group", "year"),
        ),
        selectInput(
          "lon", "Longitude column",
          c("", "species", "date", "x", "y", "group", "year"),
        ),
        selectInput(
          "lat", "Latitude column",
          c("", "species", "date", "x", "y", "group", "year"),
        ),
        selectInput(
          "year", "Year column",
          c("","species", "date", "x", "y", "group", "year"),
        ),
        selectInput(
          "id", "Choose the identifier",
          c("","species", "date", "x", "y", "group", "year"),
        ),
        numericInput(
          "nSamps", "Number of iterations",
          value = 50
        ),
        fileInput("mask",
                  NULL
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
mod_space_bias_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$dateRangesUI <- renderUI({
      num <- input$num
      dateRanges <- lapply(1:num, function(i) {
        dateRangeInput(paste0("dates_", i), label = paste("Date Range", i), start = NULL, end = NULL)
      })
      tagList(dateRanges)
    })

    output$space_bias_plot <- renderPlot({
      random_ggplot(type = "line")
    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
