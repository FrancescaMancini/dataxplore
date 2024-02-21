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
                textOutput(ns("text1")),
                h2("How to use dataXplore"),
                textOutput(ns("text2")),
                h2("Limitations and caveats"),
                textOutput(ns("text3")),
                h2("Resources"),
                textOutput(ns("text4"))
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

    output$text1 <- renderText({
      random_text(nwords = 100)
    })

    output$text2 <- renderText({
      random_text(nwords = 150)
    })

    output$text3 <- renderText({
      random_text(nwords = 75)
    })

    output$text4 <- renderText({
      random_text(nwords = 20)
    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
