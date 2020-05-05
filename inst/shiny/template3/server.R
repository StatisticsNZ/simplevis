# template3
# server.R

shinyServer(function(input, output, session) {

  # plot

  plot_data <- reactive({ # create a reactive data object

    ### add your plot_data code here ###

  })

  plot <- reactive({ # create a reactive ggplot object
    if (input$isMobile == F) {
      font_size_title <- 11
      font_size_body <- 10
    }
    else if (input$isMobile == T) {
      font_size_title <- 15
      font_size_body <- 14
    }

    ### add your plot code here ###
    ### remember to add the following arguments to simplevis functions:
          ### isMobile = input$isMobile, font_size_title = font_size_title, font_size_body = font_size_body
    ### remember to refer to a reactive plot_data object as plot_data()

  })

  output$plot_desktop <- plotly::renderPlotly({ ### render it as a html object for desktop users
    plotly::ggplotly(plot(), tooltip = "text") %>%
      simplevis::remove_plotly_buttons()    
  })

  output$plot_mobile <- renderPlot({ ### render it as a image for mobile users
    plot() +
      ggplot2::theme(plot.title.position = "plot") +
      ggplot2::theme(plot.caption.position = "plot") 
  })
  
  ### use this code if you want to speed things up for mobile users
  # output$plot_mobile <- renderCachedPlot({
  #   plot()
  # },
  # cacheKeyExpr = {
  #   list() ### list input$dependencies here
  # })
  
  # map
  
  output$map1 <- leaflet::renderLeaflet({
    basemap
  })
  
  map_data1 <- reactive({
    ### add your map_data code here ###
    tibble() #placeholder
    
    # return(map_data1)
  })
  
  draw_map1 <- function() {
    
    map_id <- "map1"
    legend_id <- paste0(map_id, "_legend")
    map_id_zoom <- paste0(map_id, "_zoom") #reactive zoom for points
    radius1 <- ifelse(input[[map_id_zoom]] < 7, 1, 
                      ifelse(input[[map_id_zoom]] < 9, 2, 
                             ifelse(input[[map_id_zoom]] < 12, 3, 4)))      
    
    ### add your leaflet code here ###
    ### remember to add the following argument to simplevis functions: shiny = TRUE
    ### remember to refer to a reactive map_data1 object as map_data1()
    
  }
  
  observe({
    req(input$map1_zoom) #Wait for basemap before plotting points. Change the map prefix to your map_id if different
    withProgress(message = "Loading", {
      draw_map1()
    })
  })
  
  output$map2 <- leaflet::renderLeaflet({
    basemap
  })
  
  map_data2 <- reactive({
    ### add your map_data code here ###
    tibble() #placeholder
    
    # return(map_data2)
  })
  
  draw_map2 <- function() {
    
    map_id <- "map2"
    legend_id <- paste0(map_id, "_legend")
    map_id_zoom <- paste0(map_id, "_zoom") #reactive zoom for points
    radius2 <- ifelse(input[[map_id_zoom]] < 7, 1, 
                      ifelse(input[[map_id_zoom]] < 9, 2, 
                             ifelse(input[[map_id_zoom]] < 12, 3, 4)))    
    
    ### add your leaflet code here ###
    ### remember to add the following argument to simplevis functions: shiny = TRUE
    ### remember to refer to a reactive map_data2 object as map_data2()
    
  }

  observe({
  req(input$map2_zoom) #Wait for basemap before plotting points. Change the map prefix to your map_id if different
  withProgress(message = "Loading", {
    draw_map2()
  })
  
})

  ### table ###

  output$table <- DT::renderDT(
    df, ### adjust data object name, and columns as necessary ###
    filter = "top",
    rownames = F,
    options = list(pageLength = 5,
                   scrollX = T)
  )

  ### download ###

  output$download <- downloadHandler(
    ### add a zip file called download.zip into the data subfolder ###
    filename <- function() {
      "download.zip"
    },
    content <- function(file) {
      file.copy("data/download.zip", file)
    },
    contentType = "application/zip"
  )
  
  # output$download <- downloadHandler(
  #   ### applicable  if 1 dataset ###
  #   filename = function() {
  #     "data.csv"
  #   },
  #   content = function(file) {
  #     readr::write_csv(df, file, na ="") ### adjust data object name, as necessary ###
  #   }
  # )

  ### download code ###

  output$download_code <- downloadHandler(
    filename <- function() {
      "template3.zip"
    },
    content <- function(file) {
      file.copy("data/template3.zip", file)
    },
    contentType = "application/zip"
  )

})
