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
        radioButtons(
          ns("periodtype"), "Time periods as",
          choiceNames = list("Years", "Year ranges"),
          choiceValues = list("years", "ranges"),
          selected = "years"
        ),
        uiOutput(ns("numUI")),
        uiOutput(ns("dateRangesUI")),
        selectInput(
          ns("spat_uncert"), "Spatial Uncertainty column",
          choices = NULL
        ),
        numericInput(
          ns("max_spat_uncert"), "Maximum Spatial Uncertainty",
          value = 10000
        ),
        selectInput(
          ns("norm"), "Normalize",
          choices = c("Yes", "No")
        ),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        checkboxInput("report", "Add to report", FALSE)
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

mod_species_bias_tab_server <- function(id, reformatted_data, uploaded_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(uploaded_data(), {
      updateSelectInput(session, "spat_uncert",
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

    plot_data <- eventReactive(input$plot_button, {
      req(input$spat_uncert, input$max_spat_uncert, input$norm, uploaded_data())

      cleaned_data <- uploaded_data() %>%
        dplyr::select(input$spat_uncert) %>%
        cbind(reformatted_data()) %>%
        filter(!is.na(year))
      
      num_filtered <- nrow(reformatted_data()) - nrow(cleaned_data)
      if (num_filtered > 0) {
        showNotification(paste(num_filtered, "rows with NA values in the year column were removed."), type = "warning")
      }

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

      plot <- assessSpeciesNumber(
        dat = cleaned_data,
        species = "species",
        periods = periods,
        x = "longitude",
        y = "latitude",
        year = "year",
        spatialUncertainty = input$spat_uncert,
        identifier = "identifier",
        maxSpatUncertainty = input$max_spat_uncert,
        normalize = ifelse(input$norm == "Yes", TRUE, FALSE)
      )$plot

      list(plot = plot)
    })

    output$species_num_plot <- renderPlot({
      plot_data()$plot
    })

    return(reactive(list(
      spat_uncert = input$spat_uncert
    )))
  })
}

## To be copied in the UI
#

## To be copied in the server
#
