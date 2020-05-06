# template1
# global.R

# load libraries
library(dplyr)
library(simplevis)

# load data
### read your RDS data in from data folder here and call it df ###
df <-  ggplot2::diamonds

color_vector <- sort(unique(df$color))
  