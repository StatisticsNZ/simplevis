# shapes

#' @title New Zealand coastline.
#' @description New Zealand coastline, excluding the Chathams, that is simplified for ggplot.
#' @format An \code{sf} object.
#' @examples
#' nz
#'
#' ggplot_sf(nz)
#'
#' ggplot_sf(dplyr::slice(nz, 2, 4))
#'
#' ggplot_sf(dplyr::slice(nz, 1, 3, 5:7))
#' @source \url{https://data.linz.govt.nz/layer/51153-nz-coastlines-and-islands-polygons-topo-150k/}
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

#' @title Example stars object.
#' @description Example stars object.
#' @format An \code{stars} object.
#' @examples
#' example_stars
"example_stars"

#' @title Example stars object 2.
#' @description Example stars object.
#' @format An \code{stars} object.
#' @examples
#' example_stars_2
"example_stars_2"