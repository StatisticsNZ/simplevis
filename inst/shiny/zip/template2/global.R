# template2
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
data_folder <- "data/"
data1 <-  readRDS(paste0(data_folder, "data1.RDS"))
data2 <-  readRDS(paste0(data_folder, "data2.RDS"))

# add helper vectors (if required)
color_vector <- sort(unique(data1$color))

metric_vector <- sort(unique(data2$indicator))

# choose a basemap (if required)
basemap <- leaflet_basemap_stack_nz()

