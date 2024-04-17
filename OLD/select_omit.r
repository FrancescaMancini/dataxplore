#' runs dplyr select with ommision of Null and "" values
#'
#' @param ... arguments to pass to dplyr select
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @import dplyr
#' 
select_omit <- function(...){

    cols = na.omit(...)

}