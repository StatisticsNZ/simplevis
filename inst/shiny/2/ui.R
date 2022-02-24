# template2
# ui.R

app_title <- "Desktop & mobile-friendly" # name app

mobileDetect <-
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
    # delete the below 3 lines
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
            # add widgets
            radioButtons("plot_color", "Colour", color_vector),
            helpText(glue::glue("Data source: {data_source}"))
          ),
          mainPanel(
            width = 9,
            conditionalPanel(
              condition = "input.isMobile == false",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("plot_desktop", height = "500px"),
                type = 7,
                color = "#A8A8A8"
              )
            ),
            conditionalPanel(
              condition = "input.isMobile == true",
              shinycssloaders::withSpinner(
                plotOutput("plot_mobile", height = "500px"),
                type = 7,
                color = "#A8A8A8"
              )
            )
            # DT::DTOutput("plot_data")
          )
        )
      ),
      tabPanel(
        "Map",
        icon = icon("globe-americas", lib = "font-awesome"),
        # fluidRow(
        #   shinycssloaders::withSpinner(leaflet::leafletOutput("leaf"), type = 7, color = "#A8A8A8"),
        #   helpText(glue::glue("Data source: {data_source}"))
        # )
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # add widgets
            radioButtons(
              "leaf_filter",
              "Filter",
              c("None", "Improving", "Indeterminate", "Worsening")
            ),
            helpText(glue::glue("Data source: {data_source}"))
          ),
          mainPanel(width = 9,
                    fluidRow(
                      shinycssloaders::withSpinner(
                        leaflet::leafletOutput("leaf", width = "95%", height = 550),
                        type = 7,
                        color = "#A8A8A8"
                      )
                    )
                    # DT::DTOutput("leaf_data")
            )
          )
        ),
        tabPanel(
          "Table",
          icon = icon("table", lib = "font-awesome"),
          fluidRow(
            shinycssloaders::withSpinner(DT::DTOutput("table"), type = 7, color = "#A8A8A8"),
            helpText(glue::glue("Data source: {data_source}"))
          )
          # sidebarLayout( #use this if multiple datasets
          #   sidebarPanel(
          #     width = 3,
          #     radioButtons("table_data", "Dataset", c("Diamonds", "Storms")),
          #     helpText(glue::glue("Data source: {data_source}"))
          #   ),
          #   mainPanel(
          #     width = 9,
          #     shinycssloaders::withSpinner(DT::DTOutput("table"), type = 7, color = "#A8A8A8")
          #   )
          # )
        ),
        tabPanel(
          "Download",
          icon = icon("download", lib = "font-awesome"),
          fluidRow(
            tags$div(tags$br()),
            downloadButton("download", "Download all data"),
            tags$div(tags$br()),
            helpText(glue::glue("Data source: {data_source}"))
          )
        )
        # tabPanel(
        #   "About",
        #   icon = icon("info-circle", lib = "font-awesome"),
        #   fluidRow(includeMarkdown("www/about.Rmd"))
        # )
      )
    )
  )
