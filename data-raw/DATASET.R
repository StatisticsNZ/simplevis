## code to prepare `DATASET` dataset goes here
load("data-raw/example_borders.rda")
load("data-raw/example_point.rda")
load("data-raw/example_polygon.rda")
load("data-raw/example_stars.rda")

usethis::use_data(example_borders, overwrite = TRUE)
usethis::use_data(example_point, overwrite = TRUE)
usethis::use_data(example_polygon, overwrite = TRUE)
usethis::use_data(example_stars, overwrite = TRUE)