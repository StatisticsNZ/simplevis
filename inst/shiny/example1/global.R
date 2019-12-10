# example1
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
### read your RDS data in from data folder here and call it df ###
df <- example_sf_nz_river_wq %>% sf::st_drop_geometry()
