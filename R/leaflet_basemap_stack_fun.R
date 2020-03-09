# leaflet basemap stack functions

#' @title Basemap stack in leaflet.
#' @description Make a stack of leaflet baselayers for use in shiny apps.
#' @param top_layer The first layer to start in the basemap stack. Either "light", "dark", "street", "satellite", or "ocean". Defaults to "light".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_basemap_stack("dark")

leaflet_basemap_stack <- function(top_layer = "light"){
  
  if(top_layer == "light") basemap_order <- c("Light", "Dark", "Strdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==eet", "Satellite", "Ocean")
  else if(top_layer == "dark") basemap_order <- c("Dark", "Light", "Street", "Satellite", "Ocean")
  else if(top_layer == "satellite") basemap_order <- c("Satellite", "Light", "Dark", "Street", "Ocean")
  else if(top_layer == "street") basemap_order <- c("Street", "Light", "Dark", "Satellite", "Ocean")
  else if(top_layer == "ocean") basemap_order <- c("Ocean", "Light", "Dark", "Street", "Satellite")
  else basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  
  providers <- leaflet::providers
  
  leaflet() %>%
    # leaflet.extras::addFullscreenControl() %>%
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

#' @title Basemap stack in leaflet for New Zealand.
#' @description Make a stack of leaflet baselayers for use in New Zealand focussed shiny apps.
#' @param top_layer The first layer to start in the basemap stack. Either "light", "dark", "street", "satellite", or "ocean". Defaults to "light".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_basemap_stack_nz("dark")

leaflet_basemap_stack_nz <- function(top_layer = "light"){
  
  if(top_layer == "light") basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  else if(top_layer == "dark") basemap_order <- c("Dark", "Light", "Street", "Satellite", "Ocean")
  else if(top_layer == "satellite") basemap_order <- c("Satellite", "Light", "Dark", "Street", "Ocean")
  else if(top_layer == "street") basemap_order <- c("Street", "Light", "Dark", "Satellite", "Ocean")
  else if(top_layer == "ocean") basemap_order <- c("Ocean", "Light", "Dark", "Street", "Satellite")
  else basemap_order <- c("Light", "Dark", "Street", "Satellite", "Ocean")
  
  providers <- leaflet::providers
  
  leaflet() %>%
    fitBounds(166.70047,-34.45676, 178.52966,-47.06345) %>%
    # leaflet.extras::addFullscreenControl() %>%
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
    addWMSTiles(
      baseUrl = "https://s3-stats.cloud.eaglegis.co.nz/arcgis/services/Environmental2018/catchment/MapServer/WmsServer?",
      layers = c("3"),
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        zIndex = 900
      ),
      group = "Regions"
    ) %>%
    addWMSTiles(
      baseUrl = "https://s3-stats.cloud.eaglegis.co.nz/arcgis/services/Environmental2018/catchment/MapServer/WmsServer?",
      layers = c("2"),
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        zIndex = 900
      ),
      group = "Territorial authorities"
    ) %>%
    addWMSTiles(
      baseUrl = "https://s3-stats.cloud.eaglegis.co.nz/arcgis/services/Environmental2018/catchment/MapServer/WmsServer?",
      layers = c("0"),
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        zIndex = 900
      ),
      group = "Catchments"
    ) %>%
    addLayersControl(
      baseGroups = basemap_order,
      overlayGroups = c("Regions", "Territorial authorities", "Catchments"),
      position = "topleft",
      options = layersControlOptions(autoZIndex = FALSE)
    ) %>%
    hideGroup(c("Regions", "Territorial authorities", "Catchments"))
}
