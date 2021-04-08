# use this script to draft visualisation code to then add into the app
# these plots or maps should use character inputs that mimic the inputs that the user will select
# they can then be copied and pasted into the server reactive data and reactive plot/map code
# note in the server, all reactive data objects must be referred to as data()

library(dplyr)
library(simplevis)

data_folder <- "inst/shiny/template2/data/"

data1 <-  readRDS(paste0(data_folder, "data1.RDS"))

data2 <-  readRDS(paste0(data_folder, "data2.RDS"))

# make a plot filtered by a user selected colour
color_vector <- sort(unique(data1$color))

selected_color <- "E"

plot_data <- data %>%
  filter(color == selected_color) %>% 
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, .drop = FALSE) %>%
  summarise(average_price = round(mean(price), 0)) %>%
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  mutate(average_price = paste0("US$", prettyNum(average_price,  big.mark = ","))) %>% 
  add_tip(c("cut", "clarity", "average_price"))

title <- paste0("Average diamond price of colour ", selected_color, " by cut and clarity")
x_title <- "Average price ($US thousands)"
y_title <- "Cut"

plot <- ggplot_hbar_col(data = plot_data, 
                        x_var = average_price_thousands, 
                        y_var = cut, 
                        col_var = clarity,
                        tip_var = tip_text,
                        legend_ncol = 4,
                        title = title, 
                        x_title = x_title, 
                        y_title = y_title)

plotly::ggplotly(plot, tooltip = "text") %>% 
  plotly_camera() 

# make a trend map filtered by a user selected metric
metric_vector <- sort(unique(data2$indicator))

selected_metric <- "Nitrate-nitrogen"

map_data <- data2 %>%
  filter(period == "2008-2017") %>% 
  filter(indicator == selected_metric)

pal <- c("#4575B4", "#D3D3D3", "#D73027")

title <- paste0("Monitored river ", selected_metric, " trends, 2008\u201317")

leaflet_sf_col(map_data, 
               trend_category, 
               pal = pal, 
               col_method = "category",
               title = title)

