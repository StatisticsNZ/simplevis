# template2
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
set.seed(123456789)

data1 <- ggplot2::diamonds %>%
  slice_sample(prop = 0.1)

data2 <- simplevis::example_point %>%
  mutate(trend_category = factor(
    trend_category,
    levels = c("Improving", "Indeterminate", "Worsening")
  ))

# add reference
data_source <- "Diamonds Association"

# choose a basemap
leaf_basemap <-
  leaf_basemap(bounds = c(166.70047, -34.45676, 178.52966, -47.06345))

# add other helpers as required
color_vector <- sort(unique(data1$color))
title_wrap <- 50