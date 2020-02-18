# example2
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
### read your RDS data in from data folder here and call it df ###
df <- example_sf_nz_river_wq

# choose a basemap
basemap <- leaflet_basemap_stack_nz()
