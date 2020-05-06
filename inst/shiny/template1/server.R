# template1
# server.R

shinyServer(function(input, output, session) {

  # plot

  plot_data <- reactive({ # create a reactive data object

    ### add your plot_data code here ###
    selected_color <- input$plot_color
    
    plot_data <- df %>%
      filter(color == selected_color) %>% 
      mutate(cut = stringr::str_to_sentence(cut)) %>%
      group_by(cut, clarity, .drop = FALSE) %>%
      summarise(average_price = mean(price)) %>%
      mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
      ungroup()

    return(plot_data)
  })

  plot <- reactive({ # create a reactive ggplot object

    ### add your plot code here ###
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

  # output$plot_mobile <- renderPlot({ 
  #   plot() +
  #     ggplot2::theme(plot.title.position = "plot") +
  #     ggplot2::theme(plot.caption.position = "plot") 
  # })
  
  ### use this code if you want to speed things up for mobile users
  output$plot_mobile <- renderCachedPlot({
    plot() +
        ggplot2::theme(plot.title.position = "plot") +
        ggplot2::theme(plot.caption.position = "plot")
  },
  cacheKeyExpr = {
    list() ### list input$dependencies here
  })
  
  ### table ###
  
  # table_data <- reactive({   #if more than one dataset
  #   if(input$table_data == "Diamonds") table_data <- ggplot2::diamonds
  #   else if(input$table_data == "Storms") table_data <- dplyr::storms
  #   return(table_data)
  # })
  
  output$table <- DT::renderDT(
    df, ### adjust data object name, and columns as necessary ###
    # table_data(), 
    filter = "top",
    rownames = F,
    options = list(pageLength = 5, scrollX = T)
  )

  ### download ###
  
  # use this code if one dataset
  output$download <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      readr::write_csv(df, file, na = "") # adjust data object name, as necessary 
    }
  )
  
  # use this code if multiple datasets
  # output$download <- downloadHandler(
  #   filename <- function() {
  #     "data.zip"   # add a zip file called data.zip into the data subfolder
  #   },
  #   content <- function(file) {
  #     file.copy("data/data.zip", file)
  #   },
  #   contentType = "application/zip"
  # )
  
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
