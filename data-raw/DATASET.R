## code to prepare `DATASET` dataset goes here
load("data-raw/example_sf_point.rda")
load("data-raw/example_sf_polygon.rda")
load("data-raw/example_stars.rda")
load("data-raw/example_stars_2.rda")
load("data-raw/nz.rda")
load("data-raw/nz_region.rda")

usethis::use_data(example_sf_point, overwrite = TRUE)
usethis::use_data(example_sf_polygon, overwrite = TRUE)
usethis::use_data(example_stars, overwrite = TRUE)
usethis::use_data(example_stars_2, overwrite = TRUE)
usethis::use_data(nz, overwrite = TRUE)
usethis::use_data(nz_region, overwrite = TRUE)
