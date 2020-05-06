#use this script to draft visualisation code to then add into the app

library(dplyr)
library(simplevis)

df <-  ggplot2::diamonds

color_vector <- sort(unique(df$color))

selected_color <- "E"

plot_data <- df %>%
  filter(color == selected_color) %>% 
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, .drop = FALSE) %>%
  summarise(average_price = mean(price)) %>%
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  ungroup()

title <- paste0("Average diamond price of colour ", selected_color, " by cut and clarity")

plot <- ggplot_hbar_col(data = plot_data, 
                        x_var = average_price_thousands, 
                        y_var = cut, 
                        col_var = clarity, 
                        legend_ncol = 4,
                        title = title, 
                        x_title = "Average price ($US thousands)", 
                        y_title = "Cut")



