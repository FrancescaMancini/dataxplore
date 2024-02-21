#' environment_bias_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_environment_bias_tab_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' environment_bias_tab Server Functions
#'
#' @noRd
mod_environment_bias_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
#

## To be copied in the server
#
