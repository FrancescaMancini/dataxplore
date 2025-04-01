library(shiny)
library(DT)
library(dplyr)
library(spData)
library(bslib)

# ui <- fluidPage(
#   theme = bs_theme(bootswatch = "flatly"),
#   titlePanel("Simple Shiny App with DT and spData"),
#   sidebarLayout(
#     sidebarPanel(
#       helpText("This app shows countries by continent from the `spData::world` dataset.")
#     ),
#     mainPanel(
#       DTOutput("summary_table")
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   world_summary <- spData::world %>%
#     st_drop_geometry() %>%
#     group_by(continent) %>%
#     summarise(
#       n_countries = n(),
#       mean_gdp = mean(gdpPercap, na.rm = TRUE)
#     )
#
#   output$summary_table <- renderDT({
#     datatable(world_summary)
#   })
# }

shinyApp(ui, server)
