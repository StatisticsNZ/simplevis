# template2
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
### read your RDS data in from data folder here and call it df ###
df1 <-  ggplot2::diamonds

df2 <- simplevis::example_sf_nz_river_wq %>% 
  filter(period == "2008-2017") %>% 
  filter(indicator %in% c("Nitrate-nitrogen", "Total nitrogen", "Ammoniacal nitrogen"))

# make any vectors required for widgets
color_vector <- sort(unique(df1$color))

indicator_vector <- sort(unique(df2$indicator))

# choose a basemap
basemap <- leaflet_basemap_stack_nz()

