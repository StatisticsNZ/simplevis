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
  }) 
  
  # output$plot_data <- DT::renderDT(
  #   plot_data(), 
  #   filter = "top",
  #   rownames = FALSE,
  #   options = list(pageLength = 5, scrollX = TRUE, lengthChange = FALSE)
  # )
  
  plot_theme <- reactive({
    gg_theme(
      font  = "helvetica", 
      size_title = 11, 
      size_body = 10, 
      gridlines_v = TRUE
    )
  }) 
  
  plot <- reactive({
    # create a reactive ggplot object
    
    # add plot code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive plot_data object as plot_data()
    
    req(plot_data())
    req(plot_theme())
    
    .color <- input$plot_color
    
    title <- glue::glue("Average diamond price of colour {.color} by cut and clarity")
    x_title <- "Average price ($US thousands)"
    y_title <- "Cut"
    
    plot <- gg_hbar_col(
      plot_data(),
      x_var = price,
      y_var = cut,
      col_var = clarity,
      text_var = text,
      title = title,
      x_title = x_title,
      y_title = y_title,
      col_labels = ggplot2::waiver(),
      title_wrap = title_wrap,
      theme = plot_theme()
    )
    
    return(plot)
  })
  
  output$plot <- plotly::renderPlotly({
    plotly::ggplotly(plot(), tooltip = "text") %>%
      plotly_camera() %>% 
      plotly_col_legend(rev = TRUE) %>%
      plotly::style(hoverlabel = list(font = list(family = "helvetica")))
  }) 
  
  ### leaf ###
  output$leaf <- leaflet::renderLeaflet({
    
    leaf_basemap %>%
      leaflet::addMiniMap(
        tiles = leaflet::providers$CartoDB.PositronNoLabels,
        position = "bottomleft",
        width = 100,
        height = 100,
        autoToggleDisplay = TRUE,
        toggleDisplay = TRUE,
        minimized = TRUE
      )
  })
  
  leaf_data <- reactive({
    # create a reactive leaf_data object
    
    leaf_filter <- input$leaf_filter
    
    if (leaf_filter == "None") {
      leaf_data <- data2
    } else if (leaf_filter != "None") {
      leaf_data <- data2 %>%
        filter(trend_category == leaf_filter)
    }
    
    return(leaf_data)
  }) 
  
  # output$leaf_data <- DT::renderDT(
  #   leaf_data(), 
  #   filter = "top",
  #   rownames = FALSE,
  #   options = list(pageLength = 5, scrollX = TRUE, lengthChange = FALSE)
  # )
  
  leaf_draw <- function() {
    # add leaflet code from make_data_vis.R
    # change any placeholder character values to input widgets
    # refer to a reactive leaf_data object as leaf_data()
    # use reactive radius for points that get bigger as the user zooms in, if necessary
    
    size_reactive <- ifelse(input$leaf_zoom < 6, 1.5, #change leaf prefix if different map id used
             ifelse(input$leaf_zoom < 7, 2, 
                    ifelse(input$leaf_zoom < 8, 3, 4)))
    
    title <- "Monitored trends, 2008\u201317"
    
    leaf_sf_col(
      leaf_data(),
      col_var = trend_category,
      size_point = size_reactive,
      col_title = title
    )
  }
  
  observe({
    req(leaf_data())
    req(input$leaf_zoom) #change leaf prefix if different map id used
    
    withProgress(message = "Loading", {
      leaf_clear()  #add map id if leaf_sf*() function has a map id different to leaf
      leaf_draw()
    })
  })
  
  ### table ###
  
  table_data <- reactive({
    # create a reactive table_data object
    
    ggplot2::diamonds %>%
      select(carat:price) %>%
      rename_with(snakecase::to_sentence_case)
  })
  
  output$table <- DT::renderDT(
    table_data(),
    filter = "top",
    rownames = FALSE,
    options = list(pageLength = 10, scrollX = TRUE, lengthChange = FALSE)
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
    "demo1.zip"
  },
  content <- function(file) {
    file.copy("data/demo1.zip", file)
  },
  contentType = "application/zip")
  
})