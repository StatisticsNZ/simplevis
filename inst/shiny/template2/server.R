# template1
# server.R

shinyServer(function(input, output, session) {
  
  ### plot ###
  
  plot_data <- reactive({ # create a reactive plot_data object
    
    # add plot_data code from make_data_vis.R
    # change any placeholder character values to input widgets
    
    selected_color <- input$plot_color
    
    plot_data <- data1 %>%
      filter(color == selected_color) %>% 
      mutate(cut = stringr::str_to_sentence(cut)) %>%
      group_by(cut, clarity, .drop = FALSE) %>%
      summarise(average_price = round(mean(price), 0)) %>%
      mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
      mutate(average_price = paste0("US$", prettyNum(average_price,  big.mark = ","))) %>% 
      add_tip(c("cut", "clarity", "average_price"))
    
    return(plot_data)
  })
  
  plot <- reactive({ # create a reactive ggplot object
    
    # add plot code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive plot_data object as plot_data()

    selected_color <- input$plot_color
    
    title <- paste0("Average diamond price of colour ", selected_color, " by cut and clarity")
    x_title <- "Average price ($US thousands)"
    y_title <- "Cut"
    
    plot <- ggplot_hbar_col(data = plot_data(), 
                            x_var = average_price_thousands, 
                            y_var = cut, 
                            col_var = clarity, 
                            tip_var = tip_text,
                            legend_ncol = 4,
                            title = title, 
                            x_title = x_title, 
                            y_title = y_title,
                            isMobile = input$isMobile)
    
    
    return(plot)
  })
  
  output$plot_desktop <- plotly::renderPlotly({ 
    plotly::ggplotly(plot(), tooltip = "text") %>%
      plotly_camera()
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
  
  map_data <- reactive({ # create a reactive map_data object
    
    selected_metric <- input$map_metric
    
    map_data <- data2 %>%
      filter(period == "2008-2017") %>% 
      filter(indicator == selected_metric)
    
    return(map_data)
  })
  
  draw_map <- function() {
    
    # add leaflet code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive map_data object as map_data()
    # use reactive radius for points that get bigger as the user zooms in, if necessary 
    
    # reactive_radius <-  case_when(input$map_zoom < 7, 1, ifelse(input$map_zoom < 9, 2, ifelse(input$map_zoom < 12, 3, 4)))  

    selected_metric <- input$map_metric
    
    pal <- c("#4575B4", "#D3D3D3", "#D73027")
    
    title <- paste0("Monitored river ", selected_metric, " trends, 2008\u201317")
    
    leaflet_sf_col(map_data(), 
                   trend_category, 
                   pal = pal, 
                   col_method = "category",
                   title = title,
                   radius = 2)
  }
  
  observe({
    req(input$map_zoom) # wait for basemap before plotting. 

    withProgress(message = "Loading", {
      draw_map()
    })
  })
  
  ### table ###
  
  table_data <- reactive({    # create a reactive table_data object
    if(input$table_data == "Diamonds") ggplot2::diamonds %>% 
      select(carat:price)
    
    else if(input$table_data == "Storms") dplyr::storms %>% 
      select(name, year, month, day, status, wind, pressure)
  })
  
  output$table <- DT::renderDT(
    table_data(),
    filter = "top",
    rownames = FALSE,
    options = list(pageLength = 5, scrollX = TRUE)
  )
  
  ### download ###
  
  output$download <- downloadHandler(
    filename <- function() {
      "data.zip"   # add data.zip into the data subfolder using code in get_app_data.R
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
      file.copy("data/template2.zip", file)
    },
    contentType = "application/zip"
  )
  
})
