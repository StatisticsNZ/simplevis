## code to prepare `DATASET` dataset goes here
load("data-raw/example_sf_point.rda")
load("data-raw/example_sf_polygon.rda")
load("data-raw/example_stars.rda")
load("data-raw/example_stars_2.rda")
load("data-raw/nz.rda")

usethis::use_data(example_sf_point)
usethis::use_data(example_sf_polygon)
usethis::use_data(example_stars)
usethis::use_data(example_stars_2)
usethis::use_data(nz)
