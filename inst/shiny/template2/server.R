# template1
# server.R

shinyServer(function(input, output, session) {
  
  ### plot ###
  
  plot_data <- reactive({ # create a reactive plot_data object
    
    # add plot_data code from make_data_vis.R
    # change any placeholder character values to input widgets
    
    .color <- input$plot_color
    
    plot_data <- data1 %>%
      filter(color == .color) %>% 
      mutate(cut = stringr::str_to_sentence(cut)) %>%
      group_by(cut, clarity, .drop = FALSE) %>%
      summarise(price = mean(price)) %>%
      mutate_text(c("cut", "clarity", "price")) 
    
    return(plot_data)
  }) %>% 
    bindCache(input$plot_color)
  
  plot <- reactive({ # create a reactive ggplot object
    
    # add plot code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive plot_data object as plot_data()
    
    .color <- input$plot_color
    
    title <- glue::glue("Average diamond price of colour {.color} by cut and clarity")
    x_title <- "Average price ($US thousands)"
    y_title <- "Cut"
    
    plot <- gg_hbar_col(plot_data(), price, cut, clarity, 
                        text_var = text,
                        title = title, 
                        x_title = x_title, 
                        y_title = y_title,
                        x_labels = scales::comma_format(),
                        col_labels = ggplot2::waiver(),
                        font_family = "Helvetica", 
                        mobile = input$isMobile)
    
    return(plot)
  }) %>% 
    bindCache(input$plot_color)
  
  output$plot_desktop <- plotly::renderPlotly({ 
    plotly::ggplotly(plot(), tooltip = "text") %>%
      plotly_camera() 
  }) %>% 
    bindCache(input$plot_color)
  
  output$plot_mobile <- renderPlot({
    plot() 
  }) %>% 
    bindCache(input$plot_color)
  
  ### map ###
  output$map <- leaflet::renderLeaflet({
    basemap
  })
  
  map_data <- reactive({ # create a reactive map_data object
    
    map_filter <- input$map_filter
    
    if(map_filter == "None") {
      map_data <- data2 
    } else if(map_filter != "None") {
      map_data <- data2 %>% 
        filter(trend_category == map_filter)
    }
    
    return(map_data)
  }) %>% 
    bindCache(input$map_filter)

  draw_map <- function() {
    
    # add leaflet code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive map_data object as map_data()
    # use reactive radius for points that get bigger as the user zooms in, if necessary 
    # reactive_radius <-  case_when(input$map_zoom < 7, 1, ifelse(input$map_zoom < 9, 2, ifelse(input$map_zoom < 12, 3, 4)))  

    title <- paste0("Monitored trends, 2008\u201317")
    
    leaflet_sf_col(map_data(), trend_category, 
                   title = title)
  }
  
  observe({
    req(input$map_zoom) # wait for basemap before plotting. 

    withProgress(message = "Loading", {
      draw_map()
    })
  })
  
  ### table ###
  
  table_data <- reactive({    # create a reactive table_data object
    if(input$table_data == "Diamonds") {
      ggplot2::diamonds %>% 
        select(carat:price) %>% 
        rlang::set_names(~snakecase::to_sentence_case(.))
    } else if(input$table_data == "Storms") {
      dplyr::storms %>% 
        select(name, year, month, day, status, wind, pressure) %>% 
        rlang::set_names(~snakecase::to_sentence_case(.))
    }
  })
  
  output$table <- DT::renderDT(
    table_data(),
    filter = "top",
    rownames = FALSE,
    options = list(scrollX = TRUE)
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
      "template2.zip"
    },
    content <- function(file) {
      file.copy("data/template2.zip", file)
    },
    contentType = "application/zip"
  )
  
})