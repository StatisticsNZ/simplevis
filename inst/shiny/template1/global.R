# template1
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
data <- ggplot2::diamonds %>%
  slice_sample(prop = 0.1)

# add helper vectors (if required)
color_vector <- sort(unique(data$color))

# choose a basemap (if required)