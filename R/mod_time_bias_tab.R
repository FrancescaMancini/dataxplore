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
mod_time_bias_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # varSelectInput(
        #   ns("year"), "Year column",
        #   data = NULL
        # ),
        selectInput(
          ns("year"), "Year column",
          choices = NULL
        ),
        radioButtons(
          ns("periodtype"), "Time periods as",
          choiceNames = list("Years", "Year ranges"),
          choiceValues = list("years", "ranges"),
          selected = character(0)
        ),
        uiOutput(ns("numUI")),
        uiOutput(ns("dateRangesUI")),
        # varSelectInput(
        #   ns("species"), "Species column",
        #   data = NULL
        # ),
        selectInput(
          ns("species"), "Species column",
          choices = NULL
        ),
        # varSelectInput(
        #   ns("lon"), "Longitude column",
        #   data = NULL
        # ),
        selectInput(
          ns("lon"), "Longitude column",
          choices = NULL
        ),
        # varSelectInput(
        #   ns("lat"), "Latitude column",
        #   data = NULL
        # ),
        selectInput(
          ns("lat"), "Latitude column",
          choices = NULL
        ),
        # varSelectInput(
        #   ns("id"), "Choose the identifier",
        #   data = NULL
        # ),
        selectInput(
          ns("ident"), "Choose the identifier",
          choices = NULL
        ),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        # checkboxInput("code", "View R code"
        # ),
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
        plotOutput(ns("number_records"))
      )
    )
  )
}

#' time_bias_tab Server Functions
#'
#' @noRd
mod_time_bias_tab_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(uploaded_data(), {
      updateSelectInput(session, "year",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "species",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "lon",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "lat",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "ident",
        choices = names(uploaded_data()),
        selected = character(0)
      )
    })


    output$numUI <- renderUI({
      req(input$periodtype == "ranges")
      numericInput(
        ns("num"), "Time periods",
        value = 1, min = 1, max = Inf
      )
    })

    output$dateRangesUI <- renderUI({
      req(input$periodtype == "ranges", input$num)
      num <- input$num
      dateRanges <- lapply(1:num, function(i) {
        numericRangeInput(paste0("dates_", i),
          label = paste("Year range", i),
          value = c(
            uploaded_data() %>%
              select(input$year) %>%
              summarise(min = min(eval(as.name(input$year)))) %>%
              pull(),
            uploaded_data() %>%
              select(input$year) %>%
              summarise(max = max(eval(as.name(input$year)))) %>%
              pull()
          )
        )
      })
      tagList(dateRanges)
    })

    observeEvent(input$plot_button, {
      req(input$species, input$year, input$lon, input$lat, input$ident, uploaded_data())

      dat <- as.data.frame(uploaded_data())

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
          dat = dat,
          species = input$species,
          periods = periods,
          x = input$lon,
          y = input$lat,
          year = input$year,
          spatialUncertainty = NULL,
          identifier = input$ident
        )$plot
      })
    })
  })
}

mod_time_bias_tab_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(uploaded_data(), {
      updateSelectInput(session, "year",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "species",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "lon",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "lat",
        choices = names(uploaded_data()),
        selected = character(0)
      )
      updateSelectInput(session, "ident",
        choices = names(uploaded_data()),
        selected = character(0)
      )
    })

    output$numUI <- renderUI({
      req(input$periodtype == "ranges")
      numericInput(
        ns("num"), "Time periods",
        value = 1, min = 1, max = Inf
      )
    })

    output$dateRangesUI <- renderUI({
      req(input$periodtype == "ranges", input$num)
      num <- input$num
      dateRanges <- lapply(1:num, function(i) {
        numericRangeInput(ns(paste0("dates_", i)),
          label = paste("Year range", i),
          value = c(
            uploaded_data() %>%
              select(input$year) %>%
              summarise(min = min(eval(as.name(input$year)))) %>%
              pull(),
            uploaded_data() %>%
              select(input$year) %>%
              summarise(max = max(eval(as.name(input$year)))) %>%
              pull()
          )
        )
      })
      tagList(dateRanges)
    })

    observeEvent(input$plot_button, {
      req(input$species, input$year, input$lon, input$lat, input$ident, uploaded_data())

      dat <- as.data.frame(uploaded_data())

      if (input$periodtype == "ranges") {
        ranges_input_names <- paste0("dates_", 1:input$num)

        # Filter the list based on 'period_ranges'
        year_ranges <- lapply(ranges_input_names, function(x){ input[[x]]})

        # Convert the year_ranges into vectors with year intervals of 1
        periods <- lapply(year_ranges, function(element) {
          seq(from = element[1], to = element[2])
        })

      } else{

        periods = sort(unique(dat[[input$year]]))
      }


      output$number_records <- renderPlot({

        if (input$periodtype == "ranges") {

          # Check for increasing years within each period
          for(period in periods) {
            validate(
              need(min(period) == period[1] && max(period) == period[length(period)], "Period years are not in ascending order.")
            )
          }
          
          # Check for overlapping periods
          for(i in 1:(length(periods) - 1)) {
            validate(
              need(max(periods[[i]]) < min(periods[[i+1]]), "Period years are overlapping.")
            )
          }

        }

        assessRecordNumber(
          dat = dat,
          species = input$species,
          periods = periods,
          x = input$lon,
          y = input$lat,
          year = input$year,
          spatialUncertainty = NULL,
          identifier = input$ident
        )$plot
      })
    })
  })
}


## To be copied in the UI
#

## To be copied in the server
#
