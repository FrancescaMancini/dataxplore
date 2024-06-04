#' species rarity bias tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_species_rarity_bias_tab_ui <- function(id){
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
        numericInput(
          ns("max_spat_uncert"), "Maximum Spatial Uncertainty",
          value = 10000,
        ),
        numericInput(
          ns("res"), "Resolution",
          value = 10000,
        ),
        selectInput(
          ns("prev"), "Calculate prevalence per period",
          choices = c("Yes", "No")
        ),
        selectInput(
          ns("metric"), "Metric",
          choices = c("Coefficient of variation",
                      "Pearson's correlation")
        ),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        checkboxInput("report", "Add to report", FALSE)
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
mod_species_rarity_bias_tab_server <- function(id, uploaded_data, module_outputs, reformatted_data){
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

    observeEvent(input$plot_button, {
      req(module_outputs()$spat_uncert,
          input$max_spat_uncert, input$res,
          input$prev, input$metric, reformatted_data())

        cleaned_data = uploaded_data() %>%
   select(module_outputs()$spat_uncert) %>%
    cbind(reformatted_data()) %>%
    filter(!is.na(year))

 # Notify the user about the number of rows filtered
  num_filtered <- nrow(reformatted_data()) - nrow(cleaned_data)
  if (num_filtered > 0) {
    showNotification(paste(num_filtered, "rows with NA values in the year column were removed."), type = "warning")
  }

  if (input$periodtype == "ranges") {

    ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
    
    # Retrieve the year ranges from the inputs
    year_ranges <- lapply(ranges_input_names, function(id) input[[id]])

    # Convert the year_ranges into vectors with year intervals of 1
    periods <- lapply(year_ranges, function(range) {
      from <- range[1]
      to <- range[2]
      return(seq(from = from, to = to))
    })

  } else {
    periods <- sort(unique(cleaned_data$year)) #list(min(cleaned_data$year:max(cleaned_data$year)))
  }


       output$rarity_plot <- renderPlot({

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

        assessRarityBias(
          dat = cleaned_data,
          species = "species",
          periods = periods,
          x = "longitude",
          y = "latitude",
          year = "year",
          spatialUncertainty = module_outputs()$spat_uncert,
          identifier = "identifier",
          maxSpatUncertainty = input$max_spat_uncert,
          res = input$res,
          prevPerPeriod = ifelse(input$prev == "Yes", TRUE, FALSE),
          metric = ifelse(input$metric == "Coefficient of variation", "r2", "cor")
        )$plot
      })
    })
  })
}

## To be copied in the UI

## To be copied in the server
