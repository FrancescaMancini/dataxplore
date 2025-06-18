#' species_id_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyhelper
mod_species_id_bias_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        div(id = "mod_species_id_bias_tab",
        radioButtons(
          ns("periodtype"), "Time periods as",
          choiceNames = list("Years", "Year ranges"),
          choiceValues = list("years", "ranges"),
          selected = "years"
        ) %>%
        helper(icon = "info-circle", colour = "black", 
          content = "time_period",
          type = "markdown"),
        uiOutput(ns("numUI")),
        uiOutput(ns("dateRangesUI")),
        numericInput(
          ns("max_spat_uncert"), "Maximum Spatial Uncertainty",
          value = 10000
        ) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "maximum_spatial_uncertainty",
                  type = "markdown"),
        selectInput(
          ns("type"), "Type",
          choices = c("count", "proportion")
        ) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "type",
                  type = "markdown"),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        actionButton(ns("export_report"), "Export Report")
      )),
      mainPanel(
        h2("Species ID"),
        p("The metric displayed in the plot is the number (or proportion) of records identified to species level in each time period and for each level of the identifier. It provides a measure of taxonomic uncertainty and how it changes over time. Records need to take the value of NA in the species column in order to be considered not identified at species level. If your species column contains taxonomic identifications at a coarser level than species you will have to convert them to NA and reupload your data onto the app in order for this function to work."),
        plotOutput(ns("species_id_plot"))
      )
    )
  )
}

#' species_id_bias_tab Server Functions
#'
#' @noRd
#' 
mod_species_id_bias_tab_server <- function(id, uploaded_data, reformatted_data, tmp_dir){
  moduleServer(id, function(input, output, session){
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

    plot_data <- eventReactive(input$plot_button, {
      req(input$max_spat_uncert, input$type, reformatted_data())
      
      if (!("spatial_uncertainty" %in% names(reformatted_data()))){
        showNotification(paste("This function requires the recording of spatial uncertainty in each entry"), type = "warning")
        stop("Cancelling plot generation - see warning")
      }

      cleaned_data <- reformatted_data() %>%
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

      plot <- assessSpeciesID(
        dat = cleaned_data,
        species = "species",
        periods = periods,
        x = "x_coordinate",
        y = "y_coordinate",
        year = "year",
        spatialUncertainty = "spatial_uncertainty",
        identifier = "identifier",
        maxSpatUncertainty = input$max_spat_uncert,
        type = input$type
      )$plot

      list(plot = plot)
    })

    output$species_id_plot <- renderPlot({
      plot_data()$plot
    })

  observeEvent(input$export_report, {
  req(input$max_spat_uncert, input$type, reformatted_data())

  if (!("spatial_uncertainty" %in% names(reformatted_data()))) {
    showNotification("Spatial uncertainty data is required for export.", type = "error")
    return(NULL)
  }

  if (input$periodtype == "ranges") {
    ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
    year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
    periods <- lapply(year_ranges, function(range) {
      seq(range[1], range[2])
    })
  } else {
    periods <- sort(unique(reformatted_data()$year))
  }

  # Use temp directory for export
  tmp_export_dir <- file.path(tmp_dir, "export")
  dir.create(tmp_export_dir, showWarnings = FALSE)

  # Save data to known filename
  write.csv(reformatted_data(), file = file.path(tmp_export_dir, "your_formatted_data.csv"), row.names = FALSE)

  # Render report
  rmarkdown::render(
    input = "markdown_files/mod_species_id_bias_tab_report.Rmd",
    output_file = "species_id_bias_report.html",
    output_dir = tmp_export_dir,
    params = list(
      species = "species",
      periods = periods,
      x = "x_coordinate",
      y = "y_coordinate",
      year = "year",
      spatialUncertainty = "spatial_uncertainty",
      identifier = "identifier",
      maxSpatUncertainty = input$max_spat_uncert,
      type = input$type
    ),
    knit_root_dir = tmp_dir,
    envir = new.env(parent = globalenv())
  )

  showNotification("Report generated. Navigate to the export tab to download the HTML.", type = "message")
})

  })
}
