# use this script to draft visualisation code to then add into the app
# these plots or maps should use character inputs that mimic the inputs that the user will select
# they can then be copied and pasted into the server reactive data and reactive plot/map code
# note in the server, all reactive data objects must be referred to as data()

library(dplyr)
library(simplevis)

# read data from app data folder
data <- ggplot2::diamonds %>%
  slice_sample(prop = 0.1)

color_vector <- sort(unique(data$color))

selected_color <- "E"

plot_data <- data1 %>%
  filter(color == selected_color) %>% 
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, .drop = FALSE) %>%
  summarise(average_price = round(mean(price), 0)) %>%
  mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
  mutate(average_price = paste0("US$", prettyNum(average_price,  big.mark = ","))) %>% 
  mutate_text(c("cut", "clarity", "average_price"))

title <- paste0("Average diamond price of colour ", selected_color, " by cut and clarity")
x_title <- "Average price ($US thousands)"
y_title <- "Cut"

plot <- gg_hbar_col(plot_data, average_price_thousands, cut, clarity, 
                    text_var = text,
                    title = title, 
                    x_title = x_title, 
                    y_title = y_title,
                    font_family = "Helvetica", 
                    mobile = F)

plot

plotly::ggplotly(plot, tooltip = "text") %>% 
  plotly_camera()

