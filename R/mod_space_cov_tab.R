#' space_cov_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#' @importFrom methods as
#' @import sp shinyhelper
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
        selectInput(ns("country"), "Country", choices = NULL, selected = FALSE) %>%
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
        checkboxInput(ns("report"), "Add to report", FALSE)
      )),
      mainPanel(
        h2("Spatial coverage"),
        p("This function can be used to assess the extent to which the same portion of the geographic domain has been sampled over time (spatio-temporal bias). This is likely to be crucial for robust estimates of changes in species distribution over time. The function provides this information in one of three ways, which can be selected by the user in the “Output” drop down menu. See the specific tooltip for details on each of the methods."),
        plotOutput(ns("space_cov_plot"))
      )

    )
  )
}

#' space_cov_tab Server Functions
#' 
#' 
#' @noRd
mod_space_cov_tab_server <- function(id, reformatted_data, iso_2_country_names, countriesLow){
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

            # # 🔹 Reproject to British National Grid
            # shape_sp <- spTransform(
            #     shape_sp,
            #     CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")
            # )

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
        req(input$res, input$output, sp_df())

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

        incProgress(0.6, detail = "Calculating spatial coverage...")

        if (input$output == "density"){

          spat_cov <- assessSpatialCov(
          dat = cleaned_data,
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

        # Apply the scale_fill_manual to each plot
        spat_cov = lapply(spat_cov, function(identifier_p){

          identifier_p = identifier_p + scale_fill_gradient(na.value = "white", low = "white", high = "blue") +
          coord_equal()
        })

        } else {
          
          if (!("spatial_uncertainty" %in% names(cleaned_data))){
            showNotification(paste("This function requires the recording of spatial uncertainty in each entry"), type = "warning")
            stop("Cancelling plot generation - see warning")
          }

          spat_cov <- assessSpatialCov(
            dat = cleaned_data,
            periods = as.list(periods),
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
            output = "nPeriods",
            minPeriod = input$min_periods
          )

          if (input$output == "overlap"){

        # Apply the scale_fill_manual to each plot
        spat_cov = lapply(spat_cov, function(identifier_p){

          identifier_p = identifier_p +
          
          coord_equal()+
          scale_fill_brewer(type = "qual") +
          theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
        })

          } else{

        # Apply the scale_fill_manual to each plot
        spat_cov = lapply(spat_cov, function(identifier_p){

          identifier_p = identifier_p +
          coord_equal() +
          theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
        })

          }

        }

        incProgress(0.8, detail = "Finalizing plot...")

        # Then arrange them
        plot <- do.call(ggpubr::ggarrange, spat_cov)

        incProgress(1, detail = "Plot ready!")
        
        list(plot = plot)
      })
    })

    output$space_cov_plot <- renderPlot({
      plot_data()$plot
    })
  })
}
