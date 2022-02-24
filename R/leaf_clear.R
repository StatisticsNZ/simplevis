#' @title In shiny, clear all features, images and legends.
#' @description In shiny, clear all features, images and legends.
#'
#' @param map_id The map id for a leaflet map. Defaults to "leaf".
#'
#' @return A map object.
#' @export
leaf_clear <- function(map_id = "leaf") {
  leafletProxy(map_id) %>%
    clearMarkers() %>%
    clearShapes() %>%
    clearImages() %>% 
    clearControls()
}