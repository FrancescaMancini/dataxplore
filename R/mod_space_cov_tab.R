#' space_cov_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom zip zipr
#' @importFrom methods as
#' @import ggplot2
#' @import sp shinyhelper
#' @import occAssess
#'
mod_space_cov_tab_ui <- function(id){
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        div(id = "mod_space_cov_tab",
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
          ns("max_spat_uncert"), "Maximum Spatial Uncertainty (Only used for overlap and number of periods plots)",
          value = 10000) %>%
          helper(icon = "info-circle", colour = "black",
                  content = "maximum_spatial_uncertainty",
                  type = "markdown"),
        numericInput(ns("res"), "Spatial resolution", value = 1000) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "spatial_resolution",
                  type = "markdown"),
        selectInput(ns("country"), "Country", choices = NULL, selected = character(0)) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "country",
                  type = "markdown"),
        fileInput(ns("shapefile"), "(Alternative to Country selection) Provide a shapefile of your survey region and file extensions. All files have to be uploaded, not just the .shp file. Note this supersedes the selection of Country.",
                  accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple = TRUE) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "shape_file",
                  type = "markdown"),
        selectInput(ns("log"), "Log count", c("TRUE", "FALSE"), selected = FALSE) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "log_count",
                  type = "markdown"),
        numericInput(ns("min_periods"), "Minimum number of periods (only used for overlap plots)", value = 2, min = 2),
        selectInput(ns("output"), "Output", c("density", "overlap", "nPeriods")) %>%
          helper(icon = "info-circle", colour = "black", 
                  content = "output",
                  type = "markdown"),
        actionButton(ns("plot_button"), "Plot"),
        downloadButton(ns("export_report"), "Export Report")
      )),
      mainPanel(
        h2("Spatial coverage"),
        p("This function can be used to assess the extent to which the same portion of the geographic domain has been sampled over time (spatio-temporal bias). This is likely to be crucial for robust estimates of changes in species distribution over time. The function provides this information in one of three ways, which can be selected by the user in the “Output” drop down menu. See the specific tooltip for details on each of the methods."),
        plotOutput(ns("space_cov_plot"), height = "1200px")
      )

    )
  )
}

#' space_cov_tab Server Functions
#' 
#' 
#' @noRd
mod_space_cov_tab_server <- function(id, reformatted_data, iso_2_country_names, countriesLow, tmp_dir, dev) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$numUI <- renderUI({
      req(input$periodtype == "ranges")
      numericInput(ns("num"), "Time periods", value = 1, min = 1)
    })

    output$dateRangesUI <- renderUI({
      req(input$periodtype == "ranges", input$num)

      min_year <- reformatted_data() %>% summarise(min_year = min(year, na.rm = TRUE)) %>% pull(min_year)
      max_year <- reformatted_data() %>% summarise(max_year = max(year, na.rm = TRUE)) %>% pull(max_year)

      tagList(lapply(1:input$num, function(i) {
        numericRangeInput(ns(paste0("dates_", i)), label = paste("Year range", i), value = c(min_year, max_year))
      }))
    })

    observe({
      updateSelectInput(session, "country", choices = iso_2_country_names$country, selected = character(0))
    })

    # Create spatial object from shapefile or country
    sp_df <- reactive({
      if (!is.null(input$shapefile)) {
        tempdirname <- dirname(input$shapefile$datapath[1])
        for (i in 1:nrow(input$shapefile)) {
          file.rename(input$shapefile$datapath[i], file.path(tempdirname, input$shapefile$name[i]))
        }

        shp_path <- file.path(tempdirname, input$shapefile$name[grep("\\.shp$", input$shapefile$name)])
        if (length(shp_path) == 0 || !file.exists(shp_path)) return(NULL)

        tryCatch({
          shape_input <- sf::st_read(shp_path, quiet = TRUE)
          as(shape_input, "Spatial")
        }, error = function(e) {
          warning("Failed to read shapefile: ", conditionMessage(e))
          return(NULL)
        })

      } else if (!is.null(input$country) && input$country != "") {
        iso_2_selected <- iso_2_country_names %>% filter(country == input$country) %>% pull(iso2)
        if (length(iso_2_selected) == 0) return(NULL)
        country_shape <- countriesLow[countriesLow$ISO_A2 == iso_2_selected, ]
        if (nrow(country_shape) == 0) return(NULL)
        return(country_shape)
      } else {
        return(NULL)
      }
    })

    plot_data <- eventReactive(input$plot_button, {
      
      req(input$res, input$output, sp_df())

      withProgress(message = 'Generating plot...', value = 0, {

        incProgress(0.4, detail = "Processing time periods...")
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

        incProgress(0.6, detail = "Calculating spatial coverage...")

        if (input$output == "density") {

          spat_cov <- assessSpatialCov(
            dat = reformatted_data(),
            periods = periods,
            res = input$res,
            logCount = input$log,
            shp = sp_df(),
            species = "species",
            x = "x_coordinate",
            y = "y_coordinate",
            year = "year",
            spatialUncertainty = NULL,
            maxSpatUncertainty = NULL,
            identifier = "identifier",
            output = input$output
          )

          spat_cov <- Map(function(name, p) {
            p + coord_equal() +
              theme(axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank()) +
              ggtitle(name)  # Add the name as plot title
          }, names(spat_cov), spat_cov)

        } else {

          spat_cov <- assessSpatialCov(
            dat = reformatted_data(),
            periods = periods,
            res = input$res,
            logCount = input$log,
            shp = sp_df(),
            species = "species",
            x = "x_coordinate",
            y = "y_coordinate",
            year = "year",
            spatialUncertainty = "spatial_uncertainty",
            maxSpatUncertainty = input$max_spat_uncert,
            identifier = "identifier",
            output = input$output,
            minPeriod = input$min_periods
          )

        spat_cov <- lapply(spat_cov, function(p) {
          p + coord_equal() +
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank()) +
            if (input$output == "overlap") scale_fill_manual(values = "blue", na.value = "white") else NULL
        })

        }

        incProgress(0.8, detail = "Finalizing plot...")
        plot <- do.call(ggpubr::ggarrange, c(spat_cov, ncol = 1))
        incProgress(1, detail = "Plot ready!")
        list(plot = plot)
      })
    })

    output$space_cov_plot <- renderPlot({
      plot_data()$plot
    })

    output$export_report <- downloadHandler(
      filename = function() {
        paste0("spatial_coverage_export_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
      },
      content = function(file) {
        withProgress(message = "Generating report...", value = 0, {
          req(reformatted_data(), input$res, input$output)

          incProgress(0.1, detail = "Preparing time periods...")
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

          incProgress(0.3, detail = "Saving datasets...")
          tmp_export_dir <- file.path(tmp_dir, "export")
          dir.create(tmp_export_dir, showWarnings = FALSE, recursive = TRUE)
          write.csv(reformatted_data(), file.path(tmp_export_dir, "your_formatted_data.csv"), row.names = FALSE)

          shapefile_uploaded <- !is.null(input$shapefile)
          if (shapefile_uploaded && !is.null(sp_df())) {
            shapefile_dir <- file.path(tmp_export_dir, "user_shapefile")
            dir.create(shapefile_dir, showWarnings = FALSE)
            sf_obj <- sf::st_as_sf(sp_df())
            sf::st_write(
              obj = sf_obj,
              dsn = shapefile_dir,
              layer = "region_shapefile",
              driver = "ESRI Shapefile",
              append = FALSE,
              quiet = TRUE
            )
          } else {
            saveRDS(countriesLow, file.path(tmp_export_dir, "countriesLow.rds"))
          }

          iso_2_selected <- iso_2_country_names %>% filter(country == input$country) %>% pull(iso2)

          incProgress(0.6, detail = "Rendering report...")
          tryCatch({

            rmarkdown::render(
              input = get_markdown_path("mod_space_cov_tab_report.Rmd", dev = dev),
              output_file = "space_cov_report.html",
              output_dir = tmp_export_dir,
              params = list(
                species = "species",
                periods = periods,
                x = "x_coordinate",
                y = "y_coordinate",
                year = "year",
                spatialUncertainty = if (input$output == "density") NULL else "spatial_uncertainty",
                maxSpatUncertainty = input$max_spat_uncert,
                res = input$res,
                logCount = input$log == "TRUE",
                identifier = "identifier",
                output = input$output,
                minPeriod = input$min_periods,
                shapefile_uploaded = shapefile_uploaded,
                country = input$country,
                country_iso2 = iso_2_selected
              ),
              knit_root_dir = tmp_dir,
              envir = new.env(parent = globalenv())
            )
          }, error = function(e) {
            print(e)
            saveRDS(e, file.path(tmp_export_dir, "render_error.rds"))
            showNotification("Report generation failed. Check logs for details.", type = "error")
            stop("Rendering failed")
          })

          incProgress(0.9, detail = "Zipping output...")
          zip::zipr(
            zipfile = file,
            files = list.files(tmp_export_dir, recursive = TRUE),
            root = tmp_export_dir
          )

          incProgress(1, detail = "Cleaning up...")
          unlink(tmp_export_dir, recursive = TRUE, force = TRUE)
        })
      }
    )
  })
}