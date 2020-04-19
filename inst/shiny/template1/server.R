# template1
# server.R

shinyServer(function(input, output, session) {

  # plot

  plot_data <- reactive({ # create a reactive data object

    ### add your plot_data code here ###

    # return(plot_data)
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
    

    # return(plot)
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
      "template1.zip"
    },
    content <- function(file) {
      file.copy("data/template1.zip", file)
    },
    contentType = "application/zip"
  )

})
