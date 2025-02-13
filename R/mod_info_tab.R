#' info_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinipsum random_text
mod_info_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      theme = bslib::bs_theme(bootswatch = "sandstone"),
      mainPanel(h2("What is dataXplore"),
                uiOutput(ns("text1")),
                h2("How to use dataXplore"),
                uiOutput(ns("text2")),
                h2("Limitations and caveats"),
                uiOutput(ns("text3")),
                h2("Resources"),
                uiOutput(ns("text4"))
      )
    )
  )
}

#' info_tab Server Functions
#'
#' @noRd
mod_info_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$text1 <- renderUI({
      HTML(
        paste(
          "<p>dataXplore is a web app for screening species occurrence data for common forms of bias. The app takes a user supplied dataset and returns a suite of metrics and visualisations to assess the suitability of species occurrence data for estimating spatial variation in species occurrence or temporal trends in species distributions. dataXplore is an interface to the R package <a href='https://github.com/robboyd/occAssess/'>occAssess</a>.</p>",
          "<p>To learn more about the package see (<a href='https://onlinelibrary.wiley.com/doi/10.1002/ece3.8299'>Boyd et al, 2021</a>).</p>"
        )
      )
    })

    output$text2 <- renderUI({
      HTML(
        paste(
          "<p>There are seven tabs in dataXplore. It is recommended that users read the information in the Info tab before attempting to use the app for the first time. After reading the Info tab, the user can upload the dataset they want to screen for bias in the “Data” tab. After the dataset has been uploaded, the user can open any of the other tabs in any order.</p>",
          "<p>The app accepts data in csv format and the file must contain the following fields: species names; x and y coordinates of the species occurrence (any coordinate reference system may be used); alternatively British National grid References of the grid square in which the occurrence was recorded can be supplied and converted into latitude and longitude within the “Data” tab; the date of the occurrence; the year of the occurrence; the spatial uncertainty associated with the coordinates (units do not matter but must be consistent); and an identifier, which is used to split the data into groups (for example it could represent taxonomic groups or data sources). Missing information in any field should be indicated by “NA”. The column names do not need to match the fields, instead the column name for each field must be specified in the Data tab. See the table below for an example set of input data.</p>",
          "<p>The tabs “Time”, “Species”, “Space” and “Environment” explore common biases in these domains. Each tab has a sidebar to specify arguments required by dataXplore to compute and visualise the metrics. After entering the required inputs, the user can click on the plot button to visualise the output.</p>",
          "<p>At the bottom of the sidebar in each tab, the user has the option to add the output, as well as the code and input used to generate it, to a downloadable report by ticking the checkbox “Add to report”.</p>"
        )
      )
    })

    output$text3 <- renderUI({
      HTML(
        paste(
          "<p>The temporal resolution used in dataXplore is the year, which means that the app cannot tell us anything about intra-annual biases (e.g. seasonal patterns in recording effort).</p>",
          "<p>It will not always be possible to disentangle biases from biological patterns. For example, the spatial bias assessment in dataXplore can show that the distribution of the data deviates from a random distribution, but it will not always be clear if this is due to sampling bias or the true distribution of the species.</p>",
          "<p>dataXplore provides a tool to investigate the quality of species occurrence data but it does not provide formal recommendations as to whether the data is of sufficient quality for any specific use. This is because the usability of species occurrence data depends not only on their biases but also on the question being asked and the method used to answer it. It is possible to derive useful inferences from biased data where biases can be modelled or reduced through subsampling or aggregation.</p>"
        )
      )
    })

    output$text4 <- renderUI({
      HTML(
        paste(
          "<p>Boyd, R. J., Powney, G. D., Carvell, C., Pescott, O. L., & Robin Boyd, C. J. (2021). occAssess: An R package for assessing potential biases in species occurrence data. Ecology and Evolution, 11(22), 16177–16187.</p>",
          "<p>Boyd, R. J., Powney, G. D., Burns, F., Danet, A., Duchenne, F., Grainger, M. J., Jarvis, S. G., Martin, G., Nilsen, E. B., Porcher, E., Stewart, G. B., Wilson, O. J., &#38; Pescott, O. L. (2022). ROBITT: A tool for assessing the risk-of-bias in studies of temporal trends in ecology. Methods in Ecology and Evolution, (7), 1497–1507.</p>"
        )
      )
    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
