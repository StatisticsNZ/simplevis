# template1
# ui.R

app_title <- "Template 1" ### name the app ###

### function to
mobileDetect <-
  function(inputId, value = 0) {
    # https://github.com/g3rv4/mobileDetect
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
    tags$head(includeScript("www/js/tag-manager.js")),
    mobileDetect("isMobile"),
    # https://github.com/g3rv4/mobileDetect
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type = "text/javascript")
    ), # https://www.cultureofinsight.com/blog/2018/03/15/2018-03-15-responsive-iframes-for-shiny-apps/
    navbarPage(
      title = HTML("<b>", app_title, "</b>"),
      windowTitle = app_title,
      tabPanel(
        "Graph",
        icon = icon("chart-area", lib = "font-awesome"),
        sidebarLayout(
          sidebarPanel(
            width = 3
            ### add radioButtons and other widgets in here ###
          ),
          mainPanel(
            width = 9,
            conditionalPanel(
              condition = "input.isMobile == false",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("plot_desktop", height = "450px"), ### test height on desktop, and adjust ###
                type = 7,
                color = "#A8A8A8"
              )
            ),
            conditionalPanel(
              condition = "input.isMobile == true",
              shinycssloaders::withSpinner(
                plotOutput("plot_mobile", height = "500px"), ### test height on mobile, and adjust ###
                type = 7,
                color = "#A8A8A8"
              )
            )
            # DT::DTOutput("plot_data") ### use this reactive table to debug plot_data() ###
          )
        )
      ),
      tabPanel(
        "Table",
        icon = icon("table", lib = "font-awesome"),
        fluidRow(
          shinycssloaders::withSpinner(DT::DTOutput("table"), type = 7, color = "#A8A8A8")
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
      ),
      tags$head(includeCSS("www/style.css"))
    ),
    HTML('<div data-iframe-height></div>')
    # https://www.cultureofinsight.com/blog/2018/03/15/2018-03-15-responsive-iframes-for-shiny-apps/
  )
)
