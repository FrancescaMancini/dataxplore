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
          choiceNames = list("Years","Year ranges"),
          choiceValues = list("years","ranges"),
          selected = character(0)
        ),
        uiOutput(ns("numUI")
        ),
        uiOutput(ns("dateRangesUI")
        ),
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
            "Record number",
            tooltip(
              bs_icon("info-circle"),
              "The plot displays the number of records in each time period.",
              placement = "bottom"
            )
          )),
        plotOutput(ns("time_plot"))
      )
    )
  )
}

#' time_bias_tab Server Functions
#'
#' @noRd
mod_time_bias_tab_server <- function(id, uploaded_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(uploaded_data(), {
      updateSelectInput(session, "year", choices = names(uploaded_data()),
                        selected=character(0))
      updateSelectInput(session, "species", choices = names(uploaded_data()),
                           selected=character(0))
      updateSelectInput(session, "lon", choices = names(uploaded_data()),
                           selected=character(0))
      updateSelectInput(session, "lat", choices = names(uploaded_data()),
                           selected=character(0))
      updateSelectInput(session, "ident", choices = names(uploaded_data()),
                           selected=character(0))
    })


    output$numUI <- renderUI({
      req(input$periodtype == "ranges")
        numericInput(
          ns("num"),"Time periods",
          value = 1, min = 1, max = Inf
          )
    })

    output$dateRangesUI <- renderUI({
      req(input$periodtype == "ranges", input$num)
      num <- input$num
      dateRanges <- lapply(1:num, function(i) {
        numericRangeInput(paste0("dates_", i),
                          label = paste("Year range", i),
                          value = c(uploaded_data() %>%
                                      select(input$year) %>%
                                      summarise(min = min(eval(as.name(input$year)))) %>%
                                      pull(),
                                    uploaded_data() %>%
                                      select(input$year) %>%
                                      summarise(max = max(eval(as.name(input$year)))) %>%
                                      pull()))
      })
      tagList(dateRanges)
    })

    nRec <- reactive({

      req(input$species, input$year, input$lon, input$lat, input$ident, input$periodtype, uploaded_data())

      pers <- list(unique(uploaded_data()[,input$year])) # need to figure out what to do when periodtype == "ranges"



      assessRecordNumber(dat = uploaded_data(),
                         species = input$species,
                         periods = pers,
                         x = input$lon,
                         y = input$lat,
                         year = input$year,
                         spatialUncertainty = NULL,
                         identifier = input$ident # apparently we have to have an identifier column... ask Rob if there is a way around that as this is not ideal
                         )$plot
      })

    # not working!

      output$time_plot <- renderPlot({

        input$plot_button

        nRec()


      })


   })
}

## To be copied in the UI
#

## To be copied in the server
#
