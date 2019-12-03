# template3
# global.R

# load libraries
library(dplyr)
library(simplevis)
library(sf) # required if app contains a simple features map (i.e. points, lines, polygons)
# library(stars) # required if app contains an array map (e.g. raster)
# library(rgdal) # required if app contains an array map (e.g. raster)

# load data
### read your RDS data in from data folder here and call it df ###

# choose a basemap
basemap <- leaflet_basemap_stack_nz()
