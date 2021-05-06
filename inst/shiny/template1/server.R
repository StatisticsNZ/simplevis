# template1
# server.R

shinyServer(function(input, output, session) {

  # plot

  plot_data <- reactive({ # create a reactive plot_data object
    
    # add plot_data code from make_data_vis.R
    # change any placeholder character values to input widgets
    
    selected_color <- input$plot_color
    
    plot_data <- data %>%
      filter(color == selected_color) %>% 
      mutate(cut = stringr::str_to_sentence(cut)) %>%
      group_by(cut, clarity, .drop = FALSE) %>%
      summarise(average_price = round(mean(price), 0)) %>%
      mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
      mutate(average_price = paste0("US$", prettyNum(average_price,  big.mark = ","))) %>% 
      mutate_text(c("cut", "clarity", "average_price"))
    
    return(plot_data)
  }) %>% 
    bindCache(input$plot_color)
  
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
                            text_var = text,
                            title = title, 
                            x_title = x_title, 
                            y_title = y_title,
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
  
  ### table ###
  
  table_data <- reactive({   
    ggplot2::diamonds 
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
      "template1.zip"
    },
    content <- function(file) {
      file.copy("data/template1.zip", file)
    },
    contentType = "application/zip"
  )
  
})
