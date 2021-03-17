# leaflet basemap stack functions

#' @title Basemap stack in leaflet.
#' 
#' @description Make a stack of leaflet baselayers for use in shiny apps.
#' @param top_layer The first layer to start in the basemap stack. Either "light", "dark", "street", "satellite", or "ocean". Defaults to "light".
#' @param bounds A bbox object or numeric vector of length four, with xmin, ymin, xmax and ymax values in WGS84 (epsg 4326).

#' @return A leaflet object.
#' @export
#' 
#' @examples
#' leaflet_basemap("dark")
#' 
#' leaflet_basemap(bounds = c(166.70047,-34.45676, 178.52966,-47.06345))
#' 
#' bb <- rnaturalearth::ne_countries(scale = "small", 
#'            country = "Papua New Guinea", 
#'            returnclass = "sf") %>% 
#'      sf::st_bbox() 
#' 
#' leaflet_basemap("satellite", bounds = bb)  
leaflet_basemap <- function(top_layer = "light", bounds = NULL){
  
  if(top_layer == "light") basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  else if(top_layer == "dark") basemap_order <- c("Dark", "Light", "Street", "Satellite", "Ocean")
  else if(top_layer == "satellite") basemap_order <- c("Satellite", "Light", "Dark", "Street", "Ocean")
  else if(top_layer == "street") basemap_order <- c("Street", "Light", "Dark", "Satellite", "Ocean")
  else if(top_layer == "ocean") basemap_order <- c("Ocean", "Light", "Dark", "Street", "Satellite")
  else basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  
  map <- leaflet() %>%
    leaflet.extras::addResetMapButton() %>% 
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