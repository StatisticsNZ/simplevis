# leaflet basemap stack functions

#' @title Basemap stack in leaflet.
#' 
#' @description Make a stack of leaflet baselayers for use in shiny apps.
#' @param top_layer The first layer to start in the basemap stack. Either "light", "dark", "street", "satellite", or "ocean". Defaults to "light".
#' @param country A country .data$name from Natural Earth.
#' @param state A state or region .data$name within a country from Natural Earth. Must be used in combination with the applicable country specified.
#' @param continent A continent .data$name from Natural Earth.

#' @return A leaflet object.
#' @export
#' 
#' @examples
#' leaflet_basemap("dark")
#' leaflet_basemap(country = "Papua New Guinea")

leaflet_basemap <- function(top_layer = "light", 
                            country = NULL, 
                            state = NULL,
                            continent = NULL){
  
  if(top_layer == "light") basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  else if(top_layer == "dark") basemap_order <- c("Dark", "Light", "Street", "Satellite", "Ocean")
  else if(top_layer == "satellite") basemap_order <- c("Satellite", "Light", "Dark", "Street", "Ocean")
  else if(top_layer == "street") basemap_order <- c("Street", "Light", "Dark", "Satellite", "Ocean")
  else if(top_layer == "ocean") basemap_order <- c("Ocean", "Light", "Dark", "Street", "Satellite")
  else basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  
  providers <- leaflet::providers
  
  if(is.null(country) & is.null(continent) & is.null(state)) {
    map <- leaflet() %>%
      leaflet.extras::addResetMapButton()
  }
  else({
    if(!is.null(country)) {
      if(is.null(state)) {
        bounds <- as.vector(sf::st_bbox(rnaturalearth::ne_countries(country = country, returnclass = "sf")))
      }
      else if(!is.null(state)) {
        bounds <- as.vector(sf::st_bbox(rnaturalearth::ne_states(country = country, returnclass = "sf") %>% filter(.data$name == state)))
      }
    }
    if(!is.null(continent)) bounds <- as.vector(sf::st_bbox(rnaturalearth::ne_countries(continent = continent, returnclass = "sf")))
    
    map <- leaflet() %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      leaflet.extras::addResetMapButton()
  })
  
  map <- map %>% 
    addProviderTiles(
      providers$CartoDB.PositronNoLabels,
      group = "Light",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 CartoDB")
    ) %>%
    addProviderTiles(
      providers$Esri.WorldImagery,
      group = "Satellite",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 ESRI")
    ) %>%
    addProviderTiles(
      providers$Esri.OceanBasemap,
      group = "Ocean",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 ESRI")
    ) %>%
    addProviderTiles(
      providers$OpenStreetMap.Mapnik,
      group = "Street",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 OpenStreetMap")
    ) %>%
    addProviderTiles(
      providers$CartoDB.DarkMatterNoLabels,
      group = "Dark",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 CartoDB")
    ) %>%
    addLayersControl(
      baseGroups = basemap_order,
      position = "topleft",
      options = layersControlOptions(autoZIndex = FALSE)
    )
  
  return(map)
}

#' @title Basemap stack in leaflet for New Zealand.
#' @description Make a stack of leaflet baselayers for use in New Zealand focussed shiny apps.
#' @param top_layer The first layer to start in the basemap stack. Either "light", "dark", "street", "satellite", or "ocean". Defaults to "light".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_basemap_nz("dark")

leaflet_basemap_nz <- function(top_layer = "light"){
  
  if(top_layer == "light") basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  else if(top_layer == "dark") basemap_order <- c("Dark", "Light", "Street", "Satellite", "Ocean")
  else if(top_layer == "satellite") basemap_order <- c("Satellite", "Light", "Dark", "Street", "Ocean")
  else if(top_layer == "street") basemap_order <- c("Street", "Light", "Dark", "Satellite", "Ocean")
  else if(top_layer == "ocean") basemap_order <- c("Ocean", "Light", "Dark", "Street", "Satellite")
  else basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  
  providers <- leaflet::providers
  
  leaflet() %>%
    fitBounds(166.70047,-34.45676, 178.52966,-47.06345) %>%
    leaflet.extras::addResetMapButton() %>%
    addProviderTiles(
      providers$CartoDB.PositronNoLabels,
      group = "Light",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 CartoDB")
    ) %>%
    addProviderTiles(
      providers$Esri.WorldImagery,
      group = "Satellite",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 ESRI")
    ) %>%
    addProviderTiles(
      providers$Esri.OceanBasemap,
      group = "Ocean",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 ESRI")
    ) %>%
    addProviderTiles(
      providers$OpenStreetMap.Mapnik,
      group = "Street",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 OpenStreetMap")
    ) %>%
    addProviderTiles(
      providers$CartoDB.DarkMatterNoLabels,
      group = "Dark",
      options = providerTileOptions(zIndex = 0, attribution = "\u00A9 CartoDB")
    ) %>%
    addLayersControl(
      baseGroups = basemap_order,
      position = "topleft",
      options = layersControlOptions(autoZIndex = FALSE)
    ) 
}
