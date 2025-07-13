#' species rarity bias tab UI Function
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
mod_species_rarity_bias_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        div(id = "mod_species_rarity_bias_tab",
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
        numericInput(
          ns("res"), "Resolution",
          value = 10000
        ) %>%
          helper(icon = "info-circle", colour = "black",
                  content = "resolution",
                  type = "markdown"),
        selectInput(
          ns("prev"), "Calculate prevalence per period",
          choices = c("Yes", "No")
        ) %>%
          helper(icon = "info-circle", colour = "black",
                  content = "prevalence_per_period",
                  type = "markdown"),
        selectInput(
          ns("metric"), "Metric",
          choices = c("Coefficient of variation",
                      "Pearson's correlation")
        ) %>%
          helper(icon = "info-circle", colour = "black",
                  content = "metric",
                  type = "markdown"),
        actionButton(
          ns("plot_button"), "Plot"
        ),
        downloadButton(ns("export_report"), "Export Report")
      )),
      mainPanel(
        h2("Rarity bias"),
        p("The metric displayed in the plot is a rarity bias index for each time period and each level of the identifier. This metric can be used to assess the degree to which rare species are oversampled relative to commoner species and whether this changes over time. The premise is that if there was no bias, species would be sampled proportionally to their commonness (common species would be recorded more often than rare species). For each species, the function calculates the number of records and its commonness (measured as the number of grid cells on which the species has been recorded) and assesses their congruence."),
        p("If Metric = coefficient of variation, values range from 0, indicating high bias, to 1, indicating low bias."),
        p("If Metric = Pearson’s correlation, values range from −1, indicating high bias, to 1 indicating low bias."),
        plotOutput(ns("rarity_plot"))
      )
    )
  )
}

#' rarity_bias_tab Server Functions
#'
#' @noRd
mod_species_rarity_bias_tab_server <- function(id, uploaded_data, reformatted_data, tmp_dir, dev){
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

      req(input$max_spat_uncert, input$res,
        input$prev, input$metric, reformatted_data())

      if (!("spatial_uncertainty" %in% names(reformatted_data()))){
        showNotification(paste("This function requires the recording of spatial uncertainty in each entry"), type = "warning")
        stop("Cancelling plot generation - see warning")
      }

      withProgress(message = 'Generating plot...', value = 0, {

        incProgress(0.3, detail = "Preparing time periods...")

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

        incProgress(0.6, detail = "Calculating rarity bias...")

        plot <- assessRarityBias(
          dat = reformatted_data(),
          species = "species",
          periods = periods,
          x = "x_coordinate",
          y = "y_coordinate",
          year = "year",
          spatialUncertainty = "spatial_uncertainty",
          identifier = "identifier",
          maxSpatUncertainty = input$max_spat_uncert,
          res = input$res,
          prevPerPeriod = ifelse(input$prev == "Yes", TRUE, FALSE),
          metric = ifelse(input$metric == "Coefficient of variation", "r2", "cor")
        )$plot

        incProgress(0.8, detail = "Finalizing plot. Please note, this may take some time for larger datasets")

        list(plot = plot)
      })
    })

    output$rarity_plot <- renderPlot({
      plot_data()$plot
    })

output$export_report <- downloadHandler(
  filename = function() {
    paste0("species_rarity_bias_export_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
  },
  content = function(file) {
    req(input$max_spat_uncert, input$res, input$prev, input$metric, reformatted_data())

    if (!("spatial_uncertainty" %in% names(reformatted_data()))) {
      showNotification("Spatial uncertainty data is required for export.", type = "error")
      return(NULL)
    }

    # Define export directory
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

    # Save reformatted data
    write.csv(reformatted_data(), file = file.path(tmp_export_dir, "your_formatted_data.csv"), row.names = FALSE)

    # Render RMarkdown report
    rmarkdown::render(
      input = get_markdown_path("mod_species_rarity_bias_tab_report.Rmd", dev = dev),
      output_file = "species_rarity_bias_report.html",
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
        res = input$res,
        prevPerPeriod = ifelse(input$prev == "Yes", TRUE, FALSE),
        metric = ifelse(input$metric == "Coefficient of variation", "r2", "cor")
      ),
      knit_root_dir = tmp_dir,
      envir = new.env(parent = globalenv())
    )

    # Zip everything in the export folder
    zip::zipr(zipfile = file, files = list.files(tmp_export_dir, full.names = TRUE), root = tmp_export_dir)

    # Clean up after download
    unlink(tmp_export_dir, recursive = TRUE, force = TRUE)
  }
)

  })
}
