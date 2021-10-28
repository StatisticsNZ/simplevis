# leaflet basemap stack functions

#' @title Basemap stack in leaflet.
#' 
#' @description Make a stack of leaflet baselayers for use in shiny apps.
#' @param bounds A bbox object or numeric vector of length four, with xmin, ymin, xmax and ymax values in WGS84 (epsg 4326).
#' @param basemap The first layer to start in the basemap stack. Either "light", "dark", "street", "satellite", or "ocean". Defaults to "light".

#' @return A leaflet object.
#' @export
#' 
#' @examples
#' leaflet_basemap(basemap = "dark")
#' 
#' leaflet_basemap(bounds = c(166.70047,-34.45676, 178.52966,-47.06345))
#' 
leaflet_basemap <- function(bounds = NULL, basemap = "light"){
  
  if(basemap == "light") basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  else if(basemap == "dark") basemap_order <- c("Dark", "Light", "Street", "Satellite", "Ocean")
  else if(basemap == "satellite") basemap_order <- c("Satellite", "Light", "Dark", "Street", "Ocean")
  else if(basemap == "street") basemap_order <- c("Street", "Light", "Dark", "Satellite", "Ocean")
  else if(basemap == "ocean") basemap_order <- c("Ocean", "Light", "Dark", "Street", "Satellite")
  else basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")

  map <- leaflet() %>%
    leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                               title = "Reset View", 
                                               onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
    htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
    addProviderTiles(
      leaflet::providers$CartoDB.PositronNoLabels,
      group = "Light",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 CartoDB")
    ) %>%
    addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 ESRI")
    ) %>%
    addProviderTiles(
      leaflet::providers$Esri.OceanBasemap,
      group = "Ocean",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 ESRI")
    ) %>%
    addProviderTiles(
      leaflet::providers$OpenStreetMap.Mapnik,
      group = "Street",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 OpenStreetMap")
    ) %>%
    addProviderTiles(
      leaflet::providers$CartoDB.DarkMatterNoLabels,
      group = "Dark",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 CartoDB")
    ) %>%
    addLayersControl(
      baseGroups = basemap_order,
      position = "topleft",
      options = layersControlOptions(autoZIndex = FALSE)
    )
  
  if(!is.null(bounds)){
    bounds <- as.vector(bounds)
    
    map <- map %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) 
  }
  
  return(map)
}
