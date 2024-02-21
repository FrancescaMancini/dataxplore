#' rarity_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rarity_bias_tab_ui <- function(id){
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
        # dateRangeInput(
          # "periods",
        # "Choose date range"
        # ),
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
        textInput(
          "res", "Spatial resolution",
          value = 1000
        ),
        selectInput(
          "year", "Year column",
          c("", "species", "date", "x", "y", "group", "year"),
        ),
        selectInput(
          "id", "Choose the identifier",
          c("", "species", "date", "x", "y", "group", "year"),
        ),
        selectInput(
          "prev", "Calculate prevalence per period",
          c("TRUE", "FALSE"), selected = FALSE,
        ),
        selectInput(
          "metric", "Metric",
          c("R2", "Pearson")
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
            "Rarity bias",
            tooltip(
              bs_icon("info-circle"),
              "The plot displays the rarity bias index in each time period.
              The index can be used to assess the degree to which rare species are oversampled relative to commoner species and whether this changes over time.
              If metric is R2, values close to 0 indicate high bias and values close to 1 indicate low bias.
              If metric is Pearson, values close to -1 indicate high bias and values close to 1 indicate low bias",
              placement = "bottom"
            )
          )),
        plotOutput(ns("rarity_plot"))

      )
    )
  )
}

#' rarity_bias_tab Server Functions
#'
#' @noRd
mod_rarity_bias_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$dateRangesUI <- renderUI({
      num <- input$num
      dateRanges <- lapply(1:num, function(i) {
        dateRangeInput(paste0("dates_", i), label = paste("Date Range", i), start = NULL, end = NULL)
      })
      tagList(dateRanges)
    })

    output$rarity_plot <- renderPlot({
      random_ggplot(type = "line")
    })


  })
}

## To be copied in the UI

## To be copied in the server
