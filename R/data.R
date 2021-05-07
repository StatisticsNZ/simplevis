# shapes

#' @title New Zealand coastline.
#' @description Simplified New Zealand coastline boundary, excluding the Chatham Islands.
#' @format An \code{sf} object.
#' @examples
#' nz
#'
#' ggplot_sf(nz)
#'
#' ggplot_sf(dplyr::slice(nz, 2, 4))
#'
#' ggplot_sf(dplyr::slice(nz, 1, 3, 5:7))
"nz"

#' @title Example sf polygon object.
#' @description Example sf polygon object.
#' @format An \code{sf} object.
#' @examples
#' example_sf_polygon
"example_sf_polygon"

#' @title Example sf point object.
#' @description Example sf point object.
#' @format An \code{sf} object.
#' @examples
#' example_sf_point
"example_sf_point"