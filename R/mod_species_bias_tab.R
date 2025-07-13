#' species_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom zip zipr
#' @import shinyhelper
#'
mod_species_bias_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        div(id = "mod_species_bias_tab",
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
          ns("norm"), "Normalize",
          choices = c("Yes", "No")
        ) %>%
          helper(icon = "info-circle", colour = "black",
                  content = "normalize",
                  type = "markdown"),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        downloadButton(ns("export_report"), "Export Report")
      )),
      mainPanel(
        h2("Species number"),
        p("The metric displayed in the plot is the number of species recorded in each time period for each level of the identifier. It provides a measure of taxonomic coverage and how it changes over time. Changes in the numbers of species recorded could also reflect true extinction/colonisation events in a dataset, but, for heterogeneous, aggregated, data, issues of uneven taxonomic representativeness across time are considerably more likely."),
        p("If the number of species differs widely between levels of the identifier, we recommend setting Normalise = Yes so that the indices for each level of the identifier fall on a comparable scale, making it easier to assess temporal variation in number of species for the levels with fewer species."),
        plotOutput(ns("species_num_plot"))
      )
    )
  )
}

mod_species_bias_tab_server <- function(id, reformatted_data, uploaded_data, tmp_dir, dev){
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
      req(input$max_spat_uncert, input$norm, reformatted_data())

      periods <- if (input$periodtype == "ranges") {
        ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
        year_ranges <- lapply(ranges_input_names, function(id) input[[id]])

        # Check 1: Ensure all start <= end
        for (i in seq_along(year_ranges)) {
          if (year_ranges[[i]][1] > year_ranges[[i]][2]) {
            showNotification(paste("Year range", i, "is invalid: start year is after end year."), type = "error")
            return(NULL)
          }
        }

        # Convert to sequences for overlap detection
        sequences <- lapply(year_ranges, function(range) seq(range[1], range[2]))

        # Check 2: Ensure no overlap between sequences
        all_years <- unlist(sequences)
        if (any(duplicated(all_years))) {
          showNotification("Year ranges must not overlap.", type = "error")
          return(NULL)
        }

        sequences
      } else {
        sort(unique(reformatted_data()$year))
      }

      plot <- assessSpeciesNumber(
        dat = reformatted_data(),
        species = "species",
        periods = periods,
        x = "x_coordinate",
        y = "y_coordinate",
        year = "year",
        spatialUncertainty = "spatial_uncertainty",
        identifier = "identifier",
        maxSpatUncertainty = input$max_spat_uncert,
        normalize = ifelse(input$norm == "Yes", TRUE, FALSE)
      )$plot

      list(plot = plot)
    })

    output$species_num_plot <- renderPlot({
      plot_data()$plot
    })

output$export_report <- downloadHandler(
  filename = function() {
    paste0("species_bias_export_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
  },
  content = function(file) {
    req(input$max_spat_uncert, input$norm, reformatted_data())

    if (!("spatial_uncertainty" %in% names(reformatted_data()))) {
      showNotification("Spatial uncertainty data is required for export.", type = "error")
      return(NULL)
    }

    # Define export path
    tmp_export_dir <- file.path(tmp_dir, "export")
    if (!dir.exists(tmp_export_dir)) dir.create(tmp_export_dir, recursive = TRUE)

    periods <- if (input$periodtype == "ranges") {
      ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
      year_ranges <- lapply(ranges_input_names, function(id) input[[id]])

      # Check 1: Ensure all start <= end
      for (i in seq_along(year_ranges)) {
        if (year_ranges[[i]][1] > year_ranges[[i]][2]) {
          showNotification(paste("Year range", i, "is invalid: start year is after end year."), type = "error")
          return(NULL)
        }
      }

      # Convert to sequences for overlap detection
      sequences <- lapply(year_ranges, function(range) seq(range[1], range[2]))

      # Check 2: Ensure no overlap between sequences
      all_years <- unlist(sequences)
      if (any(duplicated(all_years))) {
        showNotification("Year ranges must not overlap.", type = "error")
        return(NULL)
      }

      sequences
    } else {
      sort(unique(reformatted_data()$year))
    }

    # Save data
    write.csv(reformatted_data(), file = file.path(tmp_export_dir, "your_formatted_data.csv"), row.names = FALSE)

    # Render R Markdown
    rmarkdown::render(
      input = get_markdown_path("mod_species_bias_tab_report.Rmd", dev = dev),
      output_file = "species_bias_report.html",
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
        normalize = ifelse(input$norm == "Yes", TRUE, FALSE)
      ),
      knit_root_dir = tmp_dir,
      envir = new.env(parent = globalenv())
    )

    # Zip the export folder
    zip::zipr(zipfile = file, files = list.files(tmp_export_dir, full.names = TRUE), root = tmp_export_dir)

    # Clean up after zipping
    unlink(tmp_export_dir, recursive = TRUE, force = TRUE)
  }
)

  })

}
