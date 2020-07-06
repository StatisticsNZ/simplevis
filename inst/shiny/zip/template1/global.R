# template1
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
data_folder <- "data/"
data <-  readRDS(paste0(data_folder, "data.RDS"))

# add helper vectors (if required)
color_vector <- sort(unique(data$color))

# choose a basemap (if required)