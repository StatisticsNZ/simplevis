# shapes

#' @title Example sf object of the New Zealand coastline.
#' 
#' @description Example sf object of the New Zealand coastline used to demonstrate adding borders to maps.
#' @format An \code{sf} object.
#' @examples
#' gg_sf_col(example_sf_point, col_var = trend_category, borders = example_sf_borders)
"example_sf_borders"

#' @title Example sf polygon object.
#' 
#' @description Example sf polygon object.
#' @format An \code{sf} object.
#' @examples
#' gg_sf_col(example_sf_point, col_var = trend_category, borders = example_sf_borders)
"example_sf_polygon"

#' @title Example sf point object.
#' @description Example sf point object.
#' @format An \code{sf} object.
#' @examples
#' gg_sf_col(example_sf_polygon, col_var = density, borders = example_sf_borders)
"example_sf_point"

#' @title Example stars object.
#' @description Example stars object.
#' @format A \code{stars} object.
#' @examples
#' library(stars)
#' gg_stars_col(example_stars, col_var = nitrate, borders = example_sf_borders)
"example_stars"