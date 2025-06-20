#' space_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import raster dplyr shinyhelper
#' @importFrom zip zipr
#' 
mod_space_bias_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        div(id = "mod_space_bias_tab",
        radioButtons(
          ns("periodtype"), "Time periods as",
          choiceNames = list("Years", "Year ranges"),
          choiceValues = list("years", "ranges"),
          selected = "years"
        ) %>%
          helper(
            icon = "info-circle", colour = "black", 
            content = "time_period",
            type = "markdown"
          ),

        uiOutput(ns("numUI")),
        uiOutput(ns("dateRangesUI")),

        selectInput(
          ns("country"), "Country", choices = NULL, selected = FALSE
        ) %>%
          helper(
            icon = "info-circle", colour = "black", 
            content = "country",
            type = "markdown"
          ),

        fileInput(ns("shapefile"), "(Alternative to Country selection) Provide a shapefile of your survey region and file extensions. All files have to be uploaded, not just the .shp file. Note this supersedes the selection of Country.",
          accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"),
          multiple = TRUE
        ) %>%
          helper(
            icon = "info-circle", colour = "black", 
            content = "shape_file_mask",
            type = "markdown"
          ),

        numericInput(
          ns("nSamps"), "Number of iterations", value = 50
        ) %>%
          helper(
            icon = "info-circle", colour = "black", 
            content = "iteration",
            type = "markdown"
          ),

        actionButton(ns("plot_button"), "Plot"),
        downloadButton(ns("export_report"), "Export Report")
      )),
      
      mainPanel(
        h2("Spatial bias"),
        p("The metric displayed in the plot is an index of spatial bias, which quantifies the degree to which a sample deviates from a random distribution within the area of interest. The metric is based on the Nearest Neighbour Index (NNI), given as the ratio of the average observed nearest neighbour distances (the Euclidean distance of each data point to its nearest neighbouring point) to the expected average nearest neighbour distance if the data were randomly distributed. The function simulates a user specified number of datasets (Number of simulations) randomly across the study area in equal number to the occurrence data. The NNI can then be given as the ratio of the average observed nearest neighbour distances in the data to the average of the simulated nearest neighbour distances. By using simulations, the function can provide uncertainty associated with the index (the function will display 90% confidence intervals by default). The index displayed in the plot can be interpreted as how far the observed distribution deviates from a random distribution of the same density. Values between 0 and 1 are more clustered than a random distribution, and values above 1 are more widely dispersed."),
        p("It is worth pointing out that the index produced here is a function of both sampling biases in the data and the true distributions of the focal taxa. If the function is used to assess data for one or a small number of species, the NNI will likely indicate a strong departure from a random distribution. This is to be expected because the geographical distribution of records will reflect e.g., the environmental niche of the target taxa. The function is therefore most appropriate for use with data spanning many species, in which case a more accurate picture of the distribution of sampling is likely to be obtained."),
        plotOutput(ns("space_bias_plot"))
      )
    )
  )
}

#' space_bias_tab Server Functions
#'
#' @noRd
mod_space_bias_tab_server <- function(id, uploaded_data, reformatted_data, iso_2_country_names, countriesLow, tmp_dir = tmp_dir){
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
      req(input$periodtype == "ranges")
      req(input$num)

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

    observe({
      updateSelectInput(session, "country", choices = iso_2_country_names$country)
    })

sp_df <- eventReactive(input$plot_button, {
    if (!is.null(input$shapefile)) {
        withProgress(message = "Loading shapefile...", value = 0, {
            tempdirname <- dirname(input$shapefile$datapath[1])
            
            # Rename files to maintain the correct format
            for (i in 1:nrow(input$shapefile)) {
                file.rename(
                    input$shapefile$datapath[i],
                    paste0(tempdirname, "/", input$shapefile$name[i])
                )
            }
            
            incProgress(0.5, detail = "Reading shapefile...")
            
            # Read shapefile using `sf`
            shape_input <- sf::st_read(paste(
                tempdirname,
                input$shapefile$name[grep(pattern = "*.shp$", input$shapefile$name)],
                sep = "/"
            ))

            incProgress(0.8, detail = "Converting to Spatial format...")

            # Convert to sp object
            shape_sp <- as(shape_input, "Spatial")

            incProgress(1, detail = "Done")
            
            return(shape_sp)
        })
    } else if (!is.null(input$country) && input$country != "") {

        # If the user selects a country from the dropdown
        iso_2_selected <- iso_2_country_names %>%
            filter(country == input$country) %>%
            pull(iso2)
        
        if (length(iso_2_selected) == 0) return(NULL)
        
        country_shape <- countriesLow[countriesLow$ISO_A2 == iso_2_selected, ]
        
        if (nrow(country_shape) == 0) return(NULL)

        # # Reproject to British National Grid
        # country_shape <- spTransform(
        #     country_shape,
        #     CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
        # )
        
        return(country_shape)
    } else {
        return(NULL)
    }
})

    plot_data <- eventReactive(input$plot_button, {
      withProgress(message = 'Generating plot...', value = 0, {
        req(reformatted_data(), input$nSamps, sp_df())

        incProgress(0.2, detail = "Cleaning data...")
        cleaned_data <- reformatted_data() %>%
          filter(!is.na(year))

        num_filtered <- nrow(reformatted_data()) - nrow(cleaned_data)
        if (num_filtered > 0) {
          showNotification(paste(num_filtered, "rows with NA values in the year column were removed."), type = "warning")
        }

        incProgress(0.4, detail = "Processing time periods...")
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

        incProgress(0.6, detail = "Creating raster mask...")
        mask <- rasterize(sp_df(), 
                          raster(nrow = 1000, ncol = 1000, extent(sp_df())))

        incProgress(0.8, detail = "Calculating spatial bias...")

        if (!("spatial_uncertainty" %in% names(cleaned_data))){
          showNotification(paste("This function requires the recording of spatial uncertainty in each entry"), type = "warning")
          stop("Cancelling plot generation - see warning")
        }

        plot <- assessSpatialBias(dat = cleaned_data,
                                  periods = periods,
                                  mask = mask,
                                  nSamps = input$nSamps,
                                  degrade = TRUE,
                                  species = "species",
                                  x = "x_coordinate",
                                  y = "y_coordinate",
                                  year = "year", 
                                  spatialUncertainty = "spatial_uncertainty",
                                  identifier = "identifier")$plot

        incProgress(1, detail = "Finalizing plot...")
        
        list(plot = plot)
      })
    })

    output$space_bias_plot <- renderPlot({
      plot_data()$plot
    })

output$export_report <- downloadHandler(
  filename = function() {
    paste0("spatial_bias_export_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
  },
  content = function(file) {
    req(reformatted_data(), input$nSamps, sp_df())

    # Time periods
    if (input$periodtype == "ranges") {
      ranges_input_names <- sapply(1:input$num, function(i) paste0("dates_", i))
      year_ranges <- lapply(ranges_input_names, function(id) input[[id]])
      periods <- lapply(year_ranges, function(range) seq(range[1], range[2]))
    } else {
      periods <- sort(unique(reformatted_data()$year))
    }

    # Export folder
    tmp_export_dir <- file.path(tmp_dir, "export")
    dir.create(tmp_export_dir, showWarnings = FALSE, recursive = TRUE)

    # Save reformatted data
    write.csv(reformatted_data(), file.path(tmp_export_dir, "your_formatted_data.csv"), row.names = FALSE)

    shapefile_uploaded <- !is.null(input$shapefile)

    # Save region shapefile from sp_df()
    if (shapefile_uploaded && !is.null(sp_df())) {
      shapefile_dir <- file.path(tmp_export_dir, "user_shapefile")
      dir.create(shapefile_dir, showWarnings = FALSE)

      sf_obj <- sf::st_as_sf(sp_df())  # Convert sp to sf
      sf::st_write(
        obj = sf_obj,
        dsn = shapefile_dir,
        layer = "region_shapefile",  # Base name of output files
        driver = "ESRI Shapefile",
        append = FALSE,
        quiet = TRUE
      )
    } else {
      saveRDS(countriesLow, file.path(tmp_export_dir, "countriesLow.rds"))
    }

    iso_2_selected <- iso_2_country_names %>%
      filter(country == input$country) %>%
      pull(iso2)

    # Render RMarkdown
    rmarkdown::render(
      input = "markdown_files/mod_space_bias_tab_report.Rmd",
      output_file = "spatial_bias_report.html",
      output_dir = tmp_export_dir,
      params = list(
        species = "species",
        periods = periods,
        x = "x_coordinate",
        y = "y_coordinate",
        year = "year",
        spatialUncertainty = "spatial_uncertainty",
        identifier = "identifier",
        nSamps = input$nSamps,
        shapefile_uploaded = shapefile_uploaded,
        country = input$country,
        country_iso2 = iso_2_selected
      ),
      knit_root_dir = tmp_dir,
      envir = new.env(parent = globalenv())
    )

    # Zip and clean
    zip::zipr(
      zipfile = file,
      files = list.files(tmp_export_dir, full.names = TRUE),
      root = tmp_export_dir
    )

    unlink(tmp_export_dir, recursive = TRUE, force = TRUE)
  }
)

  })

}