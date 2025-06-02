#' export_tab UI Function
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
mod_export_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      h2("Download outputs"),
      p("This function can be used to assess the extent to which the same portion of the geographic domain has been sampled over time (spatio-temporal bias). This is likely to be crucial for robust estimates of changes in species distribution over time. The function provides this information in one of three ways, which can be selected by the user in the “Output” drop down menu. See the specific tooltip for details on each of the methods."),
      plotOutput(ns("space_cov_plot")),
      br(), br(),
      downloadButton(ns("export_zip"), "Export data and HTML report", 
                     class = "btn-lg btn-primary")
    )
  )
}

#' export_tab Server Functions
#'
#' @noRd
mod_export_tab_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$export_zip <- downloadHandler(
      filename = function() {
        paste0("biodiversity_export_", Sys.Date(), ".zip")
      },
      content = function(file) {
        export_dir <- file.path(getwd(), "export")

        if (!dir.exists(export_dir)) {
          stop("Export folder not found: ", export_dir)
        }

        # List all files to include in the zip
        files <- list.files(export_dir, recursive = TRUE, full.names = TRUE)

        # Create zip
        utils::zip(zipfile = file, files = files, flags = "-r9Xj", extras = "", zip = Sys.getenv("R_ZIPCMD", "zip"))
      },
      contentType = "application/zip"
    )
  })
}