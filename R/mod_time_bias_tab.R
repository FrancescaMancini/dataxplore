#' time_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import occAssess dplyr
#' @importFrom shiny NS tagList
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets numericRangeInput
mod_time_bias_tab_ui <- function(id){
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "periodtype", "Time periods as",
          choiceNames = list("Years", "Year ranges"),
          choiceValues = list("years", "ranges"),
          selected = "years"
        ),
        uiOutput("numUI"),
        uiOutput("dateRangesUI"),
        actionButton(
          "plot_button", "Plot"
        ),
        checkboxInput("report", "Add to report", FALSE)
      ),
      mainPanel(
        h2(
          span(
            "Record number",
            tooltip(
              bs_icon("info-circle"),
              "The plot displays the number of records in each time period.",
              placement = "bottom"
            )
          )
        ),
        plotOutput("number_records")
      )
    )
  )
}

#' time_bias_tab Server Functions
#'
#' @noRd
mod_time_bias_tab_server <- function(id, reformatted_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$numUI <- renderUI({
      req(input$periodtype == "ranges")

      numericInput(
        "num", "Time periods",
        value = 1, min = 1, max = Inf
      )
    })

    output$dateRangesUI <- renderUI({
      req(input$periodtype == "ranges", input$num)
      dateRanges <- lapply(1:input$num, function(i) {
        numericRangeInput(paste0("dates_", i),
          label = paste("Year range", i),
          value = c(
            reformatted_data %>%
              select(year) %>%
              summarise(min = min(eval(year))) %>%
              pull(),
            reformatted_data %>%
              select(year) %>%
              summarise(max = max(eval(year))) %>%
              pull()
          )
        )
      })
      tagList(dateRanges)
    })

    observeEvent(input$plot_button, {

      if (input$periodtype == "ranges") {
        ranges_input_names <- paste0("dates_", 1:input$num)

        # Step 1: Filter the list based on 'period_ranges'
        year_ranges <- input[names(input) %in% ranges_input_names]

        # Step 2: Remove the names from the filtered list
        names(year_ranges) <- NULL

        # Convert the year_ranges into vectors with year intervals of 1
        periods <- lapply(filtered_list, FUN = function(element) {
          return(seq(from = element[1], to = element[2]))
        })
      } else {
        periods <- unique(dat[, input$year])
      }

      output$number_records <- renderPlot({
        assessRecordNumber(
          dat = reformatted_data,
          species = "species",
          periods = periods,
          x = "longitude",
          y = "latitude",
          year = "year",
          spatialUncertainty = NULL,
          identifier = "identifier"
        )$plot
      })
    })
  })
}