# shapes

#' @title New Zealand coastline.
#' @description New Zealand coastline, excluding the Chathams, that is simplified for ggplot.
#' @format An \code{sf} object.
#' @examples
#' nz
#'
#' ggplot_sf(nz)
#'
#' ggplot_sf(dplyr::slice(nz, 2))
#'
#' ggplot_sf(dplyr::slice(nz, 1, 3))
#' @source \url{https://data.linz.govt.nz/layer/51153-nz-coastlines-and-islands-polygons-topo-150k/}
"nz"

#' @title Example sf object of New Zealand livestock.
#' @description Example sf object of New Zealand livestock.
#' @format An \code{sf} object.
#' @examples
#' example_sf_nz_livestock
"example_sf_nz_livestock"

#' @title Example sf object of New Zealand river water quality trends.
#' @description Example sf object of New Zealand river water quality trends.
#' @format An \code{sf} object.
#' @examples
#' example_sf_nz_river_wq
"example_sf_nz_river_wq"

#' @title Example stars object of New Zealand modelled river water NO3N concentrations.
#' @description Example stars object of New Zealand modelled river water nitrate concentrations.
#' @format An \code{stars} object.
#' @examples
#' example_stars_nz_no3n
"example_stars_nz_no3n"

#' @title Example stars object of New Zealand modelled river water DRP concentrations.
#' @description Example stars object of New Zealand modelled river water dissolved reactive phosphorus concentrations.
#' @format An \code{stars} object.
#' @examples
#' example_stars_nz_drp
"example_stars_nz_drp"