# example2
# server.R

shinyServer(function(input, output, session) {

  # map

  output$map <- leaflet::renderLeaflet({
    basemap
  })

  map_data <- reactive({
    ### add your map_data code here ###
    map_data <- df %>%
      filter(indicator == input$map_indicator, period == input$map_period)
    
    return(map_data)
  })

  draw_map <- function() {

    title <- paste0("Monitored river ", stringr::str_to_lower(input$map_indicator), " trends, ", input$map_period)
    
    map_id <- "map"
    legend_id <- paste0(map_id, "_legend")
    map_id_zoom <- paste0(map_id, "_zoom") #reactive zoom for points
    radius <- ifelse(input[[map_id_zoom]] < 7, 1, 
                     ifelse(input[[map_id_zoom]] < 9, 2, 
                            ifelse(input[[map_id_zoom]] < 12, 3, 4)))    
    if(nrow(map_data()) == 0) {
      leaflet::leafletProxy(map_id) %>% 
        leaflet::clearMarkers() %>% 
        leaflet::clearShapes() %>% 
        leaflet::clearImages() %>% 
        leaflet::removeControl(legend_id)
    }
    else {
      ### add your leaflet code here ###
      ### remember to add the following argument to simplevis functions: shiny = TRUE
      ### remember to refer to a reactive map_data object as map_data()
      
      leaflet_sf_col(
        map_data(),
        trend_likelihood,
        pal = rev(pal_trend5),
        title = title,
        radius = radius,
        shiny = TRUE
      )
    }
  }
  
  observe({
    ### add req() statements for inputs that are needed before the map should be redrawn ###
    req(input$map_indicator)
    req(input$map_period)
    
    withProgress(message = "Loading", {
      draw_map()
    })
  })
  
  # plot
  
  plot_data <- reactive({ # create a reactive data object
    
    ### add your plot_data code here ###
    plot_data <- df %>%
      filter(period == input$graph_period) %>%
      group_by(period, indicator, trend_likelihood) %>%
      summarise(count = n()) %>%
      group_by(period, indicator) %>%
      mutate(percent = round(count / sum(count) * 100, 1)) %>%
      ungroup()
    
    return(plot_data)
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
    plot <- ggplot_hbar_col(
      data = plot_data(),
      x_var = percent,
      y_var = indicator,
      col_var = trend_likelihood,
      pal = rev(pal_snz_trend5),
      title = paste0("Monitored river water quality trends, ", input$graph_period),
      x_title = "Percent",
      y_title = NULL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ,
      isMobile = input$isMobile,
      font_size_title = font_size_title,
      font_size_body = font_size_body
    )
    
    return(plot)
  })

  output$plot_desktop <- plotly::renderPlotly({ # render it as a html object for desktop users
    plotly::ggplotly(plot(), tooltip = "text") %>%
      simplevis::remove_plotly_buttons()    
  })

  output$plot_mobile <- renderPlot({ # render it as a image for mobile users
    plot() +
      ggplot2::theme(plot.title.position = "plot") +
      ggplot2::theme(plot.caption.position = "plot") 
  })

  ### use renderCachedPlot with relevant inputs listed to improve mobile performance ###
  # output$plot_mobile <- renderCachedPlot({
  #   plot()
  # },
  # cacheKeyExpr = {
  #   list()
  # })

  ### table ###

  output$table <- DT::renderDT(
    df, ### adjust data object name, and columns as necessary ###
    filter = "top",
    rownames = F,
    options = list(pageLength = 5,
                   scrollX = TRUE)
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
      "example2.zip"
    },
    content <- function(file) {
      file.copy("data/example2.zip", file)
    },
    contentType = "application/zip"
  )

})
