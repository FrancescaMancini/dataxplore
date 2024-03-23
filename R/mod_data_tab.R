#' data_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny DT spData dplyr vroom tools rnrfa lubridate
#' @importFrom sf st_coordinates st_sample

mod_data_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        wellPanel(

        varSelectInput(
          ns("species"), "Species column",
          data = NULL
        ),
        actionButton(
          ns("species_summary_button"), "Species summary"
        ),
        varSelectInput(
          ns("date"), "Date column",
          data = NULL
        ),

        radioButtons(ns("date_format"), "Select date format (please ignore seperator)",
                    choices = c("day/month/year" = "format_a",
                                "month/day/year" = "format_b",
                                "year/month/day" = "format_c"),
                    selected = "format_a"),
        
        actionButton(
          ns("date_summary_button"), "Dates summary"
        ),

        varSelectInput(
          ns("lon"), "Longitude column",
          data = NULL
        ),
        varSelectInput(
          ns("lat"), "Latitude column",
          data = NULL
        ),
        actionButton(
          ns("coords_summary_button"), "Calculate bounding box"
        ),
        actionButton(
          ns("year_summary_button"), "Year summary"
        ),
        actionButton(
          ns("id_summary_button"), "Identifier summary"
        ),
        checkboxInput(ns("report"), "Add to report",
                      FALSE)
      ),
      
        varSelectInput(
          ns("id"), "Choose the identifier",
          data = NULL)

      ),

      mainPanel(
        h2(textOutput(ns("species_title"))),# add a title for each summary table only if the action button is clicked
        DTOutput(ns("species_summary_table")),
        h2(textOutput(ns("date_title"))),
        DTOutput(ns("date_summary_table")),
        h2(textOutput(ns("year_title"))),
        DTOutput(ns("year_summary_table")),
        h2(textOutput(ns("id_title"))),
        DTOutput(ns("id_summary_table")),
        h2(textOutput(ns("coords_title"))),
        DTOutput(ns("coords_summary_table"))
        )
    )
  )
}


#' data_tab Server Functions
#'
#' @noRd
mod_data_tab_server <- function(id, uploaded_data){
  moduleServer(id, function(input, output, session){
    options(shiny.maxRequestSize = 100*1024^2)
    ns <- session$ns
    # once a valid file has been uploaded
    # update the values to select variables
    # from the column names of the dataframe

    observeEvent(uploaded_data(), {
      updateVarSelectInput(session, "species", data = uploaded_data(),
                           selected=character(0))
      updateVarSelectInput(session, "date", data = uploaded_data(),
                           selected=character(0))
      updateVarSelectInput(session, "lon", data = uploaded_data(),
                           selected=character(0))
      updateVarSelectInput(session, "lat", data = uploaded_data(),
                           selected=character(0))
      updateVarSelectInput(session, "year", data = uploaded_data(),
                           selected=character(0))
      updateVarSelectInput(session, "id", data = uploaded_data(),
                           selected=character(0))
      })



    # create the variable summaries
    # number of records per species
    species_summary <- reactive({
      req(uploaded_data(), input$species, input$species_summary_button)

      uploaded_data() %>%
        group_by(get(input$species)) %>% # get() needed because input$species is a character string
        summarise(n_records = n())
    })

    # first and last date of records
    # need to let user specify the format for the dates
    # at the moment this only works if dates are in format dd/mm/yyyy
    date_summary <- reactive({
      req(uploaded_data(), input$date, input$date_summary_button)

      # re-format the dates (according to inputs)
        if (input$date_format == "format_a"){

      uploaded_data() %>%
        select(input$date) %>%
        summarise(first_record = min(dmy(eval(as.name(input$date)))),
                  last_record = max(dmy(eval(as.name(input$date)))))
        }

        else if (input$date_format == "format_b"){
      uploaded_data() %>%
        select(input$date) %>%
        summarise(first_record = min(mdy(eval(as.name(input$date)))),
                  last_record = max(mdy(eval(as.name(input$date)))))
        }

        else {
      uploaded_data() %>%
        select(input$date) %>%
        summarise(first_record = min(ymd(eval(as.name(input$date)))),
                  last_record = max(ymd(eval(as.name(input$date)))))
        }

    })


    # once I get this part to work I have to updateVarSelectInput for lat and lon

    # does not work because uploaded _data() has not been modified
    # observeEvent(input$grid_ref_convert, {
    #   updateVarSelectInput(session, "lat", data = uploaded_data(),
    #                        selected=character(0))
    #   updateVarSelectInput(session, "lon", data = uploaded_data(),
    #                        selected=character(0))
    # })

    # bounding box
    bbox <- reactive({
      req(uploaded_data(), input$lat, input$lon, input$coords_summary_button)

      uploaded_data() %>%
        select(input$lat, input$lon) %>%
        summarise(lat_min = min(eval(as.name(input$lat))),
                  lat_max = max(eval(as.name(input$lat))),
                  lon_min = min(eval(as.name(input$lon))),
                  lon_max = max(eval(as.name(input$lon))))
    })

    # number of records per year
    year_summary <- reactive({
      req(uploaded_data(), input$year, input$year_summary_button)

      uploaded_data() %>%
        group_by(get(input$year)) %>%
        summarise(n_records = n())
    })

    # number of records per group
    id_summary <- reactive({
      req(uploaded_data(), input$id, input$species, input$id_summary_button)

      uploaded_data() %>%
        group_by(get(input$id)) %>%
        summarise(n_records = n(),
                  n_species = n_distinct(get(input$species)))
    })


    output$species_summary_table <- DT::renderDT({
      species_summary()
    })

    output$species_title <- renderText({
      req(uploaded_data(), input$species, input$species_summary_button)

      "Species summary"
    })

    output$date_summary_table <- DT::renderDT({
      date_summary()
    })

    output$date_title <- renderText({
      req(uploaded_data(), input$date, input$date_summary_button)

      "Date summary"
    })


    output$coords_summary_table <- DT::renderDT({
      bbox()
    })

    output$coords_title <- renderText({
      req(uploaded_data(), input$lat, input$lon, input$coords_summary_button)

      "Bounding box"
    })

    output$year_summary_table <- DT::renderDT({
      year_summary()
    })

    output$year_title <- renderText({
      req(uploaded_data(), input$year, input$year_summary_button)

      "Year summary"
    })

    output$id_summary_table <- DT::renderDT({
      id_summary()
    })

    output$id_title <- renderText({
      req(uploaded_data(), input$id, input$id_summary_button)

      "Identifier summary"
    })
  })
}

## To be copied in the UI
#

## To be copied in the server
#

