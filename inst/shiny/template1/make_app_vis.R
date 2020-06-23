# use this script to draft visualisation code to then add into the app
# these plots or maps should use character inputs that mimic the inputs that the user will select
# they can then be copied and pasted into the server reactive data and reactive plot/map code
# note in the server, all reactive data objects must be referred to as data()
# note in the server, an additional isMobile = input$isMobile argument should be added for mobile users

library(dplyr)
library(simplevis)

data_folder <- "inst/shiny/template1/data/"

data <-  readRDS(paste0(data_folder, "data.RDS"))

color_vector <- sort(unique(data$color))

selected_color <- "E"

plot_data <- data %>%
  filter(color == selected_color) %>% 
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, .drop = FALSE) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  ungroup()

title <- paste0("Average diamond price of colour ", selected_color, " by cut and clarity")
x_title <- "Average price ($US thousands)"
y_title <- "Cut"

plot <- ggplot_hbar_col(data = plot_data, 
                        x_var = average_price_thousands, 
                        y_var = cut, 
                        col_var = clarity, 
                        legend_ncol = 4,
                        title = title, 
                        x_title = x_title, 
                        y_title = y_title)

plotly::ggplotly(plot, tooltip = "text") %>% 
  plotly_remove_buttons()
