# template1
# server.R

shinyServer(function(input, output, session) {
  ### plot ###
  
  plot_data <- reactive({
    # create a reactive plot_data object
    
    # add plot_data code from make_data_vis.R
    # change any placeholder character values to input widgets
    
    .color <- input$plot_color
    
    plot_data <- data1 %>%
      filter(color == .color) %>%
      mutate(cut = stringr::str_to_sentence(cut)) %>%
      group_by(cut, clarity, .drop = FALSE) %>%
      summarise(price = round(mean(price), 2)) %>%
      mutate_text(c("cut", "clarity", "price"))
    
    return(plot_data)
  }) #%>%
  # bindCache()
  
  output$plot_data <-
    DT::renderDT(
      plot_data(),
      filter = "top",
      rownames = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  
  plot <- reactive({
    # create a reactive ggplot object
    
    # add plot code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive plot_data object as plot_data()
    
    .color <- input$plot_color
    
    title <-
      glue::glue("Average diamond price of colour {.color} by cut and clarity")
    x_title <- "Average price ($US thousands)"
    y_title <- "Cut"
    
    plot <- gg_hbar_col(
      plot_data(),
      price,
      cut,
      clarity,
      text_var = text,
      title = title,
      x_title = x_title,
      y_title = y_title,
      x_labels = scales::comma_format(),
      col_labels = ggplot2::waiver(),
      title_wrap = title_wrap,
      font_family = "Helvetica",
      mobile = input$isMobile
    )
    
    return(plot)
  }) #%>%
  # bindCache(input$plot_color)
  
  output$plot_desktop <- plotly::renderPlotly({
    plotly::ggplotly(plot(), tooltip = "text") %>%
      plotly_camera()
  }) #%>%
  # bindCache(input$plot_color)
  
  output$plot_mobile <- renderPlot({
    plot()
  }) #%>%
  # bindCache(input$plot_color)
  
  ### map ###
  output$map <- leaflet::renderLeaflet({
    
    if (input$isMobile == FALSE) {
      basemap %>%
        leaflet::addMiniMap(
          tiles = leaflet::providers$CartoDB.PositronNoLabels,
          position = "bottomleft",
          width = 100,
          height = 100,
          autoToggleDisplay = TRUE,
          toggleDisplay = TRUE,
          minimized = TRUE
        )
    } else
      return(basemap)
  })
  
  map_data <- reactive({
    # create a reactive map_data object
    
    map_filter <- input$map_filter
    
    if (map_filter == "None") {
      map_data <- data2
    } else if (map_filter != "None") {
      map_data <- data2 %>%
        filter(trend_category == map_filter)
    }
    
    return(map_data)
  }) #%>%
  # bindCache(input$map_filter)
  
  output$map_data <- DT::renderDT(
    sf::st_drop_geometry(map_data()),
    filter = "top",
    rownames = FALSE,
    options = list(pageLength = 5, scrollX = TRUE)
  )
  
  draw_map <- function() {
    # add leaflet code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive map_data object as map_data()
    # use reactive radius for points that get bigger as the user zooms in, if necessary
    
    reactive_size <-
      ifelse(input$map_zoom < 6,
             1.5,
             ifelse(input$map_zoom < 7, 2, ifelse(input$map_zoom < 8, 3, 4)))
    
    title <- paste0("Monitored trends, 2008\u201317")
    
    leaflet_sf_col(map_data(),
                   col_var = trend_category,
                   size_point = reactive_size,
                   title = title)
  }
  
  observe({
    req(input$map_zoom) # wait for basemap before plotting.
    
    withProgress(message = "Loading", {
      draw_map()
    })
  })
  
  ### table ###
  
  table_data <- reactive({
    # create a reactive table_data object
    
    ggplot2::diamonds %>%
      select(carat:price) %>%
      janitor::clean_names(case = "sentence")
  })
  
  output$table <- DT::renderDT(
    table_data(),
    filter = "top",
    rownames = FALSE,
    options = list(
      pageLength = ifelse(input$isMobile == FALSE, 10, 5),
      scrollX = TRUE
    )
  )
  
  ### download ###
  
  output$download <- downloadHandler(filename <- function() {
    "data.zip"   # add data.zip into the data subfolder using code in get_app_data.R
  },
  content <- function(file) {
    file.copy("data/data.zip", file)
  },
  contentType = "application/zip")
  
  ### download code ###
  
  output$download_code <- downloadHandler(filename <- function() {
    "template2.zip"
  },
  content <- function(file) {
    file.copy("data/template2.zip", file)
  },
  contentType = "application/zip")
  
})