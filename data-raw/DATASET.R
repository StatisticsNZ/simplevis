## code to prepare `DATASET` dataset goes here
load("data-raw/example_sf_borders.rda")
load("data-raw/example_sf_point.rda")
load("data-raw/example_sf_polygon.rda")
load("data-raw/example_stars.rda")

usethis::use_data(example_sf_borders, overwrite = TRUE)
usethis::use_data(example_sf_point, overwrite = TRUE)
usethis::use_data(example_sf_polygon, overwrite = TRUE)
usethis::use_data(example_stars, overwrite = TRUE)