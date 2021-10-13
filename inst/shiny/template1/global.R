# template1
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
data <- ggplot2::diamonds %>%
  slice_sample(prop = 0.1)

# add reference
data_source <- "Diamonds Association"

# add other helpers as required
color_vector <- sort(unique(data$color))
title_wrap <- 50

