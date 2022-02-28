# use this script to draft visualisation code to then add into the app
# these plots or maps should use character inputs that mimic the inputs that the user will select
# they can then be copied and pasted into the server reactive data and reactive plot/map code
# note in the server, all reactive data objects must be referred to as data()

library(dplyr)
library(simplevis)

# read data from app data folder
data1 <- ggplot2::diamonds %>% 
  slice_sample(prop = 0.1)

data2 <- simplevis::example_point %>% 
  mutate(trend_category = factor(trend_category, levels = c("Improving", "Indeterminate", "Worsening")))

# make a plot filtered by a user selected colour
title_wrap <- 100

color_vector <- sort(unique(data1$color))

.color <- "E"

plot_data <- data1 %>%
  filter(color == .color) %>% 
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, .drop = FALSE) %>%
  summarise(price = mean(price)) %>%
  mutate_text(c("cut", "clarity", "price")) 

plot_theme <- gg_theme("helvetica", gridlines_v = TRUE)

title <- glue::glue("Average diamond price of colour {.color} by cut and clarity")
x_title <- "Average price ($US thousands)"
y_title <- "Cut"

plot <- gg_hbar_col(plot_data, 
                    x_var = price, 
                    y_var = cut, 
                    col_var = clarity, 
                    text_var = text,
                    title = title, 
                    x_title = x_title, 
                    y_title = y_title,
                    col_labels = function(x) x,
                    title_wrap = title_wrap,
                    theme = plot_theme)

plot

plotly::ggplotly(plot, tooltip = "text") %>% 
  plotly_camera() %>% 
  plotly_col_legend(rev = TRUE)

# make a trend map filtered by a user selected metric

leaf_filter <- "None"

if(leaf_filter == "None") {
  leaf_data <- data2 
} else if(leaf_filter != "None") {
  leaf_data <- data2 %>% 
    filter(trend_category == leaf_filter)
}

title <- "Monitored trends, 2008\u201317"

leaf_sf_col(leaf_data,
            col_var = trend_category,
            col_title = title)
