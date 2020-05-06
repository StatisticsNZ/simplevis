#use this script to draft visualisation code to then add into the app

library(dplyr)
library(simplevis)

df1 <- ggplot2::diamonds

df2 <- simplevis::example_sf_nz_river_wq %>% 
  filter(period == "2008-2017") %>% 
  filter(indicator %in% c("Nitrate-nitrogen", "Total nitrogen", "Ammoniacal nitrogen"))

# make a plot filtered by a user selected colour
color_vector <- sort(unique(df1$color))

selected_color <- "E"

plot_data <- df1 %>%
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

plot

# make a trend map filtered by a user selected indicator
indicator_vector <- sort(unique(df2$indicator))

selected_indicator <- "Nitrate-nitrogen"
  
map_data <- df2 %>%
  filter(indicator == selected_indicator)

pal <- c("#4575B4", "#D3D3D3", "#D73027")

title <- paste0("Monitored river ", selected_indicator, " trends, 2008\u201317")
  
leaflet_sf_col(map_data, 
               trend_category, 
               pal = pal, 
               col_method = "category",
               title = title)
