# template2
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
# data_folder <- "data/"
# data1 <-  readRDS(paste0(data_folder, "data1.RDS"))
# data2 <-  readRDS(paste0(data_folder, "data2.RDS"))

data1 <- ggplot2::diamonds %>% 
  slice_sample(prop = 0.1)

data2 <- simplevis::example_sf_point

# add helper vectors (if required)
color_vector <- sort(unique(data1$color))

# choose a basemap (if required)
basemap <- leaflet_basemap(bounds = c(166.70047,-34.45676, 178.52966,-47.06345))
