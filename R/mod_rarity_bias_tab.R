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
        selectInput(
          ns("species"), "Species column",
          choices = NULL
        ),
        selectInput(
          ns("lon"), "Longitude column",
          choices = NULL
        ),
        selectInput(
          ns("lat"), "Latitude column",
          choices = NULL
        ),
        selectInput(
          ns("spat_uncert"), "Spatial Uncertainty column",
          choices = NULL
        ),
        numericInput(
          ns("max_spat_uncert"), "Maximum Spatial Uncertainty",
          value = 10000,
        ),
        selectInput(
          ns("ident"), "Choose the identifier",
          choices = NULL
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
        # checkboxInput("code", "View R code"
        # ),
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
mod_rarity_bias_tab_server <- function(id, uploaded_data){
  moduleServer( id, function(input, output, session){
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
      req(input$species, input$year, input$lon,
          input$lat, input$ident, input$spat_uncert,
          input$max_spat_uncert, input$res,
          input$prev, input$metric, uploaded_data())

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
          dat = dat,
          species = input$species,
          periods = periods,
          x = input$lon,
          y = input$lat,
          year = input$year,
          spatialUncertainty = input$spat_uncert,
          identifier = input$ident,
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
