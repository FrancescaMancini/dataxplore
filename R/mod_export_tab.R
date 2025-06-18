#' export_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      h2("Download outputs"),
      p("Click the button below to download all your exported results, including data, shapefiles, and HTML reports, as a ZIP file."),
      downloadButton(ns("export_zip"), "Download ZIP", class = "btn-lg btn-primary")
    )
  )
}

#' export_tab Server Functions
#'
#' @noRd
mod_export_tab_server <- function(id, tmp_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$export_zip <- downloadHandler(
      filename = function() {
        paste0("dataXplore_export_", format(Sys.Date(), "%Y_%m_%d"), ".zip")
      },
      content = function(file) {
        export_dir <- file.path(tmp_dir, "export")

        # List all files to include in the zip
        files_to_zip <- list.files(export_dir, recursive = TRUE, full.names = TRUE)

        # Create zip archive
        zip::zipr(zipfile = file, files = files_to_zip, root = export_dir)
      },
      contentType = "application/zip"
    )
  })
}
