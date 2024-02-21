#' species_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_species_bias_tab_ui <- function(id){
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
          c("", "species", "date", "x", "y", "group", "year"),
        ),
        selectInput(
          "id", "Choose the identifier",
          c("", "species", "date", "x", "y", "group", "year"),
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
            "Species number",
            tooltip(
              bs_icon("info-circle"),
              "The plot displays the number of species recorded in each time period.",
              placement = "bottom"
            )
          )),
        plotOutput(ns("species_num_plot"))

      )
    )
  )
}

#' species_bias_tab Server Functions
#'
#' @noRd
mod_species_bias_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$dateRangesUI <- renderUI({
      num <- input$num
      dateRanges <- lapply(1:num, function(i) {
        dateRangeInput(paste0("dates_", i), label = paste("Date Range", i), start = NULL, end = NULL)
      })
      tagList(dateRanges)
    })

    output$species_num_plot <- renderPlot({
      random_ggplot(type = "line")
    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
