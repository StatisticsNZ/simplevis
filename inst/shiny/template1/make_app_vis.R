# use this script to draft visualisation code to then add into the app
# these plots or maps should use character inputs that mimic the inputs that the user will select
# they can then be copied and pasted into the server reactive data and reactive plot/map code
# note in the server, all reactive data objects must be referred to as data()

library(dplyr)
library(simplevis)

# read data from app data folder
data <- ggplot2::diamonds %>%
  slice_sample(prop = 0.1)

# make a plot filtered by a user selected colour
color_vector <- sort(unique(data$color))

.color <- "E"

plot_data <- data %>%
  filter(color == .color) %>% 
  mutate(cut = stringr::str_to_sentence(cut)) %>%
  group_by(cut, clarity, .drop = FALSE) %>%
  summarise(price = mean(price)) %>%
  mutate_text(c("cut", "clarity", "price")) 

title <- glue::glue("Average diamond price of colour {.color} by cut and clarity")
x_title <- "Average price ($US thousands)"
y_title <- "Cut"

plot <- gg_hbar_col(plot_data, price, cut, clarity, 
                    text_var = text,
                    title = title, 
                    x_title = x_title, 
                    y_title = y_title,
                    x_labels = scales::comma_format(),
                    col_labels = ggplot2::waiver(),
                    font_family = "Helvetica", 
                    mobile = F)

plot

plotly::ggplotly(plot, tooltip = "text") %>% 
  plotly_camera() %>% 
  plotly_col_legend(rev = T)

