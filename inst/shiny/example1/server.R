# example1
# server.R

shinyServer(function(input, output, session) {

  # plot
  plot_data <- reactive({ # create a reactive data object

    ### add your plot_data code here ###
    df %>%
      filter(period == input$graph_period) %>%
      group_by(period, indicator, trend_likelihood) %>%
      summarise(count = n()) %>%
      group_by(period, indicator) %>%
      mutate(percent = round(count / sum(count) * 100, 1)) %>%
      ungroup()

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
    ggplot_hbar_col(
      data = plot_data(),
      x_var = percent,
      y_var = indicator,
      col_var = trend_likelihood,
      title = paste0("Monitored river water quality trends, ", input$graph_period),
      x_title = "Percent",
      y_title = "Indicator",
      isMobile = input$isMobile,
      font_size_title = font_size_title,
      font_size_body = font_size_body
    )
    
  })
  
  output$plot_desktop <- plotly::renderPlotly({ # render it as a html object for desktop users
    plotly::ggplotly(plot(), tooltip = "text") %>%
      plotly::config(displayModeBar = F)
  })
  
  output$plot_mobile <- renderPlot({ # render it as a image for mobile users
    plot()
  })
  
  ### use renderCachedPlot with relevant inputs listed to improve mobile performance ###
  # output$plot_mobile <- renderCachedPlot({
  #   plot()
  # },
  # cacheKeyExpr = {
  #   list()
  # })
  
  # output$plot_data <- DT::renderDT( ### use this reactive table to debug plot_data() ###
  #   plot_data(),
  #   filter = "top",
  #   rownames = F,
  #   options = list(
  #     pageLength = 5,
  #     scrollX = TRUE
  #   )
  # )

  ### table ###

  output$table <- DT::renderDT(
    df, ### adjust data object name, and columns as necessary ###
    filter = "top",
    rownames = F,
    options = list(pageLength = 10,
                   scrollX = TRUE)
  )

  ### download ###

    output$download <- downloadHandler(
    ### applicable  if 1 dataset ###
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      readr::write_csv(df, file) ### adjust data object name, as necessary ###
    }
  )

  # output$download <- downloadHandler(
  #   ### if many files, add a zip file called download.zip into the data subfolder, and use this code instead ###
  #   filename <- function() {
  #     "download.zip"
  #   },
  #   content <- function(file) {
  #     file.copy("data/download.zip", file)
  #   },
  #   contentType = "application/zip"
  # )

  ### download code ###

  output$download_code <- downloadHandler(
    filename <- function() {
      "example1.zip"
    },
    content <- function(file) {
      file.copy("data/example1.zip", file)
    },
    contentType = "application/zip"
  )

})
