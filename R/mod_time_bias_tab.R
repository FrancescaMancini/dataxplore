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
#' @importFrom zip zipr
#' @import shinyhelper
#' 

# UI Function

mod_time_bias_tab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
      div(id = "mod_time_bias_tab",
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
        selectInput(ns("norm"), "Normalize", choices = c("yes", "no"), selected = "yes") %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "normalize",
                  type = "markdown"),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        downloadButton(ns("export_report"), "Export Report")
      )),
      mainPanel(
        h2("Record number"),
        p("The metric displayed in the plot is simply the number of records in each time period for each level of the identifier. This provides a measure of sampling intensity and how it changes over time. A change in the number of records over time could reflect a change in recording intensity, which is likely to affect the prevalence of some species in the dataset in a non-random way."),
        p("If the number of records differs widely between levels of the identifier, we recommend setting Normalise = Yes so that the indices for each level of the identifier fall on a comparable scale, making it easier to assess temporal variation in number of records for the levels with fewer records."),
        plotOutput(ns("number_records"))
      )
    )
  )
}

mod_time_bias_tab_server <- function(id, reformatted_data, tmp_dir) {
  moduleServer(id, function(input, output, session) {
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
      req(reformatted_data())

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

      plot <- assessRecordNumber(
        dat = cleaned_data,
        species = "species",
        periods = periods,
        x = "x_coordinate",
        y = "y_coordinate",
        year = "year",
        spatialUncertainty = NULL,
        identifier = "identifier",
        normalize = ifelse(input$norm == "Yes", TRUE, FALSE))$plot
      list(plot = plot)
    })

    output$number_records <- renderPlot({
      plot_data()$plot
    })

output$export_report <- downloadHandler(
  filename = function() {
    paste0("time_bias_export_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
  },
  content = function(file) {
    export_dir <- file.path(tmp_dir, "export")
    
    dir.create(export_dir, recursive = TRUE)

    # Save the dataset
    write.csv(reformatted_data(), file.path(export_dir, "your_formatted_data.csv"))

    # Get the periods based on user input
    if (input$periodtype == "ranges") {
      ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
      year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
      periods <- lapply(year_ranges, function(range) {
        seq(range[1], range[2])
      })
    } else {
      periods <- sort(unique(reformatted_data()$year))
    }

    # Render the R Markdown report into the export directory
    rmarkdown::render(
      input = "markdown_files/mod_time_bias_tab_report.Rmd",
      output_file = "time_bias_report.html",
      output_dir = export_dir,
      params = list(
        species = "species",
        periods = periods,
        x = "x_coordinate",
        y = "y_coordinate",
        year = "year",
        spatialUncertainty = NULL,
        identifier = "identifier",
        normalize = ifelse(input$norm == "yes", TRUE, FALSE)
      ),
      knit_root_dir = tmp_dir
    )

    # Zip the entire export directory
    zip::zipr(zipfile = file, files = list.files(export_dir, full.names = TRUE), root = export_dir)

    # Optional: delete the folder after zipping
    unlink(export_dir, recursive = TRUE, force = TRUE)
  }
)

  })
}
