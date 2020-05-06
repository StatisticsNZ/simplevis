# template2
# ui.R

app_title <- "Template 2" ### name the app ###

### function to
mobileDetect <- # https://github.com/g3rv4/mobileDetect
  function(inputId, value = 0) {
    tagList(
      singleton(tags$head(tags$script(src = "js/mobile.js"))),
      tags$input(id = inputId,
                 class = "mobile-element",
                 type = "hidden")
    )
  }

shinyUI(
  fluidPage(
    tags$div(tags$br()),
    downloadButton("download_code", tags$strong("Download code")),
    tags$div(tags$br()),
    tags$head(includeCSS("www/style.css")),
    tags$head(includeScript("www/js/tag-manager.js")),
    mobileDetect("isMobile"),
    navbarPage(
      title = HTML("<b>", app_title, "</b>"),
      windowTitle = app_title,
      tabPanel(
        "Graph",
        icon = icon("chart-area", lib = "font-awesome"),
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            # add radioButtons and other widgets in here 
            radioButtons("plot_color", "Colour", color_vector)
          ),
          mainPanel(
            width = 9,
            conditionalPanel(
              condition = "input.isMobile == false",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("plot_desktop"), 
                type = 7,
                color = "#A8A8A8"
              )
            ),
            conditionalPanel(
              condition = "input.isMobile == true",
              shinycssloaders::withSpinner(
                plotOutput("plot_mobile"), 
                type = 7,
                color = "#A8A8A8"
              )
            )
          )
        )
      ),
      tabPanel(
        "Map",
        icon = icon("globe-americas", lib = "font-awesome"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # add radioButtons and other widgets in here
            radioButtons("map_indicator", "Indicator", indicator_vector)
          ),
          mainPanel(width = 9,
                    fluidRow(
                      shinycssloaders::withSpinner(leaflet::leafletOutput("map"), type = 7, color = "#A8A8A8")
                    ))
        )
      ),
      tabPanel(
        "Table",
        icon = icon("table", lib = "font-awesome"),
        # fluidRow(
        #   shinycssloaders::withSpinner(DT::DTOutput("table"), type = 7, color = "#A8A8A8")
        # )
        sidebarLayout( #use this if multiple datasets
          sidebarPanel(
            width = 3,
            ### add radioButtons and other widgets in here ###
            radioButtons("table_data", "Dataset", c("Diamonds", "Storms"))
          ),
          mainPanel(
            width = 9,
            shinycssloaders::withSpinner(DT::DTOutput("table"), type = 7, color = "#A8A8A8")
          )
        )
      ),
      tabPanel(
        "Download",
        icon = icon("download", lib = "font-awesome"),
        fluidRow(downloadButton("download", "Download all data"))
      ),
      tabPanel(
        "About",
        icon = icon("info-circle", lib = "font-awesome"),
        fluidRow(includeMarkdown("www/about.Rmd"))
      )
    )
  )
)
