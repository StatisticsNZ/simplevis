# template1
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
### read your RDS data in from data folder here and call it df ###
df1 <-  ggplot2::diamonds
df2 <- simplevis::example_sf_nz_river_wq