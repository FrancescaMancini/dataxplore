#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_info_tab_server("info_tab_1")

  # upload a data file
  # can only be .csv or .txt
  # otherwise return message "Invalid file"

  # uploaded_data <- reactiveVal()

 uploaded_data <- reactive({
   req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           txt = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .txt file")
    )
  })

  # when checkbox for converting grid references is ticked
  # a drop down menu appears to select the gridref column
  #  and an action button to convert them to lat and long

  observeEvent(input$grid_ref, {
    insertUI(
      selector = paste0("#", "placeholder"),
      where = "beforeEnd",
      ui =  fluidRow(
            tagList(
          varSelectInput(
            "grid_ref_column", "Grid Reference column",
            data = uploaded_data()
          ),
          actionButton(
            "grid_ref_convert",
            "Convert")
        )
      )
    )
  })

  # when the action button is clicked convert the
  # grid references into lat and long
  lat_lon <- eventReactive(input$grid_ref_convert, {
    req(input$grid_ref_column)

    sites <- pull(uploaded_data(), eval(as.symbol(input$grid_ref_column)))
    osg_parse(grid_refs = sites, coord_system = "WGS84")
  })

  # this should work but it doesn't

  # observeEvent(input$grid_ref_convert, {
  #   req(uploaded_data(), lat_lon(), input$grid_ref_column)
  #   # if ('data.frame' %in% class(uploaded_data())){
  #     uploaded_data()$lat <- lat_lon()$lat
  #     uploaded_data()$lon <- lat_lon()$lon
  #   # }
  # })
  #


  # create the datatable only when the view data button is clicked

  view_table <- eventReactive(input$view_button, {
    req(input$view_button)

    datatable(uploaded_data())
  })

  # render the table
  output$data_table <-DT::renderDT({
    view_table()
  })



  mod_data_tab_server("data_tab_1", uploaded_data = uploaded_data)
  mod_time_bias_tab_server("time_bias_tab_1", uploaded_data = uploaded_data)
  mod_species_bias_tab_server("species_bias_tab_1")
  mod_species_id_bias_tab_server("species_id_bias_tab_1")
  mod_rarity_bias_tab_server("rarity_bias_tab_1")
  mod_space_cov_tab_server("space_cov_tab_1")
  mod_space_bias_tab_server("space_bias_tab_1")
  mod_environment_bias_tab_server("environment_bias_tab_1")
  mod_export_tab_server("export_tab_1")
}
