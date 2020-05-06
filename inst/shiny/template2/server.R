# template1
# server.R

shinyServer(function(input, output, session) {
  
  ### plot ###
  
  plot_data <- reactive({ # create a reactive data object
    
    # add your plot_data code here 
    selected_color <- input$plot_color
    
    plot_data <- df1 %>%
      filter(color == selected_color) %>% 
      mutate(cut = stringr::str_to_sentence(cut)) %>%
      group_by(cut, clarity, .drop = FALSE) %>%
      summarise(average_price = mean(price)) %>%
      mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
      ungroup()
    
    return(plot_data)
  })
  
  plot <- reactive({ # create a reactive ggplot object
    
    # add your plot code here 
    # remember to add isMobile = input$isMobile to simplevis functions, so that mobile plot looks good
    # remember to refer to a reactive plot_data object as plot_data()
    
    selected_color <- input$plot_color
    
    title <- paste0("Average diamond price of colour ", selected_color, " by cut and clarity")
    
    plot <- ggplot_hbar_col(data = plot_data(), 
                            x_var = average_price_thousands, 
                            y_var = cut, 
                            col_var = clarity, 
                            legend_ncol = 4,
                            title = title, 
                            x_title = "Average price ($US thousands)", 
                            y_title = "Cut", 
                            isMobile = input$isMobile)
    
    
    return(plot)
  })
  
  output$plot_desktop <- plotly::renderPlotly({ 
    plotly::ggplotly(plot(), tooltip = "text") %>%
      simplevis::remove_plotly_buttons()
  })
  
  output$plot_mobile <- renderPlot({
    plot() +
      ggplot2::theme(plot.title.position = "plot") +
      ggplot2::theme(plot.caption.position = "plot")
  })
  
  ### map ###
  output$map <- leaflet::renderLeaflet({
    basemap
  })
  
  map_data <- reactive({
    # add your map_data code here 
    selected_indicator <- input$map_indicator
    
    map_data <- df2 %>%
      filter(indicator == selected_indicator)
    
    return(map_data)
  })
  
  draw_map <- function() {
    # map_id <- "map"
    # legend_id <- paste0(map_id, "_legend")
    # map_id_zoom <- paste0(map_id, "_zoom") #reactive zoom for points
    # radius <- ifelse(input[[map_id_zoom]] < 7, 1, 
    #                  ifelse(input[[map_id_zoom]] < 9, 2, 
    #                         ifelse(input[[map_id_zoom]] < 12, 3, 4)))    
    
    # add your leaflet code here 
    # remember to refer to a reactive map_data object as map_data()
    # remember to add the following argument to simplevis functions: shiny = TRUE
    
    selected_indicator <- input$map_indicator
    
    pal <- c("#4575B4", "#D3D3D3", "#D73027")
    
    title <- paste0("Monitored river ", selected_indicator, " trends, 2008\u201317")
    
    leaflet_sf_col(map_data(), 
                   trend_category, 
                   pal = pal, 
                   col_method = "category",
                   title = title,
                   shiny = TRUE)
  }
  
  observe({
    req(input$map_zoom) #Wait for basemap before plotting points. Change the map prefix to your map_id if different
    withProgress(message = "Loading", {
      draw_map()
    })
  })
  
  ### table ###
  
  table_data <- reactive({   #if more than one dataset
    if(input$table_data == "Diamonds") table_data <- ggplot2::diamonds
    else if(input$table_data == "Storms") table_data <- dplyr::storms
    
    return(table_data)
  })
  
  output$table <- DT::renderDT(
    # df1, ### adjust data object name, and columns as necessary ###
    table_data(),
    filter = "top",
    rownames = F,
    options = list(pageLength = 5, scrollX = T)
  )
  
  ### download ###
  
  # use this code if one dataset
  # output$download <- downloadHandler(
  #   filename = function() {
  #     "data.csv"
  #   },
  #   content = function(file) {
  #     readr::write_csv(df1, file, na = "") # adjust data object name, as necessary 
  #   }
  # )
  
  # use this code if multiple datasets
  output$download <- downloadHandler(
    filename <- function() {
      "data.zip"   # add a zip file called data.zip into the data subfolder
    },
    content <- function(file) {
      file.copy("data/data.zip", file)
    },
    contentType = "application/zip"
  )
  
  ### download code ###
  
  output$download_code <- downloadHandler(
    filename <- function() {
      "template1.zip"
    },
    content <- function(file) {
      file.copy("data/template1.zip", file)
    },
    contentType = "application/zip"
  )
  
})
