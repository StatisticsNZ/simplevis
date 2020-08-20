# leaflet sf functions

#' @title Map of simple features in leaflet.
#' @description Map of simple features in leaflet that is not coloured. 
#' @param data An sf object of geometry type point/multipoint, linestring/multilinestring or polygon/multipolygon geometry type. Required input.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param popup HTML strings for use in popup. Defaults to making a leafpop::popupTable of all attribute columns in the sf object. 
#' @param radius Radius of points. Defaults to 2.
#' @param weight Stroke border size. Defaults to 2.
#' @param opacity The opacity of the fill. Defaults to 0.1. Only applicable to polygons.
#' @param stroke TRUE or FALSE of whether to draw a border around the features. Defaults to TRUE.
#' @param title A title string that will be wrapped into the legend. Defaults to "Title"
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param legend_labels A vector of legend label values. Defaults to "Feature".
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param map_id The shiny map id for a leaflet map within a shiny app. For standard single-map apps, id "map" should be used. For dual-map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' map_data <- example_sf_nz_river_wq %>%
#'   dplyr::filter(period == "1998-2017", indicator == "Nitrate-nitrogen")
#'
#' leaflet_sf(map_data)
leaflet_sf <- function(data,
                       pal = NULL,
                       popup = leafpop::popupTable(sentence_spaced_colnames(data)),
                       radius = 1,
                       weight = 2,
                       opacity = 0.1,
                       stroke = TRUE,
                       title = "[Title]",
                       legend_digits = 1,
                       legend_labels = "[Feature]",
                       basemap = "light",
                       map_id = "map") {
  
  shiny <- shiny::isRunning()
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if (sf::st_is_longlat(data) == FALSE) data <- sf::st_transform(data, 4326)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  if (is.null(pal)) pal <- pal_snz
  
  legend_id <- paste0(map_id, "_legend")
  
  if (shiny == FALSE) {
    
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
  }
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addCircleMarkers(
          data = data,
          popup = popup,
          color = pal[1],
          radius = radius,
          stroke = stroke,
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        ) 
    }
    else if (shiny == TRUE) {
      
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
      
      map <- leafletProxy(map_id) %>%
        addCircleMarkers(
          data = data,
          popup = popup,
          color = pal[1],
          radius = radius,
          stroke = stroke,
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        ) 
    }
    
    map %>% 
      addLegend(
        layerId = legend_id,
        colors = pal[1],
        labels = legend_labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = 1,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolylines(
          data = data,
          popup = popup,
          color = pal[1],
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        ) 
    }
    else if (shiny == TRUE) {
      
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
      
      map <- leafletProxy(map_id) %>%
        addPolylines(
          data = data,
          popup = popup,
          color = pal[1],
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        ) 
    }
    
    map %>% 
      addLegend(
        layerId = legend_id,
        colors = pal[1],
        labels = legend_labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = 1,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolygons(
          data = data,
          popup = popup,
          color = pal[1],
          fillOpacity = opacity, opacity = 1,
          weight = weight
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
      
      map <- leafletProxy(map_id) %>%
        addPolygons(
          data = data,
          popup = popup,
          color = pal[1],
          fillOpacity = opacity, opacity = 1,
          weight = weight
        )
    }
    
    map %>% 
      addLegend(
        layerId = legend_id,
        colors = pal[1],
        labels = legend_labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = opacity,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
    
  }
  
}

#' @title Map of simple features in leaflet that is coloured.
#' @description Map of simple features in leaflet that is coloured. 
#' @param data An sf object of geometry type point/multipoint, linestring/multilinestring or polygon/multipolygon geometry type. Required input.
#' @param col_var Unquoted variable to colour the features by. Required input.
#' @param label_var Unquoted variable to label the features by. If NULL, defaults to using the colour variable.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." if categorical colour variable, NULL results in "category". If numeric variable, defaults to "quantile". Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to FALSE.
#' @param col_na_remove TRUE or FALSE  of whether to remove NAs of the colour variable. Defaults to FALSE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param popup HTML strings for use in popup. Defaults to making a leafpop::popupTable of all attribute columns in the sf object. 
#' @param radius Radius of points. Defaults to 2.
#' @param weight Stroke border size. Defaults to 2.
#' @param stroke TRUE or FALSE of whether to draw a border around the features. Defaults to TRUE.
#' @param opacity The opacity of polygons. Defaults to 0.9.
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param title A title string that will be wrapped into the legend. Defaults to "Title".
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param map_id The shiny map id for a leaflet map within a shiny app. For standard single-map apps, id "map" should be used. For dual-map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_sf_col(example_sf_nz_livestock, dairydens,
#'      col_method = "quantile", col_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1),
#'      title = "Dairy density in count per km\u00b2, 2017")
#'
#' leaflet_sf_col(example_sf_nz_livestock, dairydens,
#'      col_method = "bin", col_cuts = c(0, 10, 50, 100, 150, 200, Inf), legend_digits = 0,
#'      title = "Dairy density in count per km\u00b2, 2017")
#'
#' map_data <- example_sf_nz_river_wq %>%
#'   dplyr::filter(period == "1998-2017", indicator == "Nitrate-nitrogen")
#'
#' pal <- c("#4575B4", "#D3D3D3", "#D73027")
#'
#' leaflet_sf_col(map_data, trend_category, pal = pal, col_method = "category",
#'    title = "Monitored river nitrate-nitrogen trends, 2008\u201317")
leaflet_sf_col <- function(data,
                           col_var,
                           label_var = NULL,
                           col_method = NULL,
                           col_cuts = NULL,
                           col_drop = FALSE,
                           col_na_remove = FALSE,
                           pal = NULL,
                           pal_rev = FALSE,
                           popup = leafpop::popupTable(sentence_spaced_colnames(data)),
                           radius = 1,
                           weight = 2,
                           opacity = 0.9,
                           stroke = TRUE,
                           title = "[Title]",
                           legend_digits = 1,
                           legend_labels = NULL,
                           basemap = "light",
                           map_id = "map") {
  
  shiny <- shiny::isRunning()
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if (sf::st_is_longlat(data) == FALSE) data <- sf::st_transform(data, 4326)

  col_var <- rlang::enquo(col_var)
  label_var <- rlang::enquo(label_var)
  if(is.null(rlang::get_expr(label_var))) label_var <- col_var
  
  if (col_na_remove == TRUE) data <- data %>% 
    filter(!is.na(!!col_var))
  
  col_var_vector <- dplyr::pull(data, !!col_var)
  label_var_vector <- dplyr::pull(data, !!label_var)
  
  if (is.null(col_method) & !is.numeric(col_var_vector)) col_method <- "category"
  if (is.null(col_method) & is.numeric(col_var_vector)) col_method <- "quantile"
  
  if (col_method == "category") {
    if (is.null(legend_labels)){
      if (is.factor(col_var_vector) &  col_drop == FALSE) labels <- levels(col_var_vector)
      else if (is.character(col_var_vector) | col_drop == TRUE) labels <- sort(unique(col_var_vector))
    }
    else if (!is.null(legend_labels)) labels <- legend_labels
    
    n_col_var_values <- length(labels)
    
    if (is.null(pal)) pal <- pal_point_set1[1:n_col_var_values]
    else if (!is.null(pal)) pal <- pal[1:n_col_var_values]
    if (pal_rev == TRUE) pal <- rev(pal)
    pal <- stringr::str_sub(pal, 1, 7)
    
    pal_fun <- colorFactor(palette = pal,
                           domain = col_var_vector,
                           na.color = "#A8A8A8")
  }
  else if (col_method == "bin") {
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vector)
    else if (!is.null(col_cuts)) {
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
    }
      
    if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
    else if (!is.null(pal)) pal <- pal[1:(length(col_cuts) - 1)]
    if (pal_rev == TRUE) pal <- rev(pal)
    pal <- stringr::str_sub(pal, 1, 7)
    
    pal_fun <- colorBin(
      palette = pal,
      domain = col_var_vector,
      bins = col_cuts,
      pretty = FALSE,
      right = FALSE,
      na.color = "#A8A8A8"
    )
    if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
    else if (!is.null(legend_labels)) labels <- legend_labels
  }
  else if (col_method == "quantile") {
    if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
    else {
      if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
      if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
    }  
    if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
    else if (!is.null(pal)) pal <- pal[1:(length(col_cuts) - 1)]
    if (pal_rev == TRUE) pal <- rev(pal)
    pal <- stringr::str_sub(pal, 1, 7)
    
    col_cuts <- quantile(col_var_vector, probs = col_cuts, na.rm = TRUE)
    if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
    
    pal_fun <- colorBin(
      palette = pal,
      domain = col_var_vector,
      bins = col_cuts,
      right = FALSE,
      na.color = "#A8A8A8"
    )
    
    if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
    else if (!is.null(legend_labels)) labels <- legend_labels
  }
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  legend_id <- paste0(map_id, "_legend")
  
  if (shiny == FALSE) {
    
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
  }
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addCircleMarkers(
          data = data,
          color = ~ pal_fun(col_var_vector),
          label = ~ htmltools::htmlEscape(label_var_vector),
          popup = popup,
          radius = radius,
          stroke = stroke,
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        )
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
      
      map <- leafletProxy(map_id) %>%
        addCircleMarkers(
          data = data,
          color = ~ pal_fun(col_var_vector),
          label = ~ htmltools::htmlEscape(label_var_vector),
          popup = popup,
          radius = radius,
          stroke = stroke,
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        ) 
    }
    
    map %>% 
      addLegend(
        layerId = legend_id,
        colors = pal,
        labels = labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = 1,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
    
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolylines(
          data = data,
          color = ~ pal_fun(col_var_vector),
          popup = popup,
          label = ~ htmltools::htmlEscape(label_var_vector),
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
      
      map <- leafletProxy(map_id) %>%
        addPolylines(
          data = data,
          color = ~ pal_fun(col_var_vector),
          popup = popup,
          label = ~ htmltools::htmlEscape(label_var_vector),
          fillOpacity = 1,
          opacity = 1,
          weight = weight
        ) 
    }
    
    map %>% 
      addLegend(
        layerId = legend_id,
        colors = pal,
        labels = labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = 1,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
    
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (shiny == FALSE) {
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolygons(
          data = data,
          color = ~ pal_fun(col_var_vector),
          popup = popup,
          label = ~ htmltools::htmlEscape(label_var_vector),
          fillOpacity = opacity, opacity = 1,
          weight = weight
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
      
      map <- leafletProxy(map_id) %>%
        addPolygons(
          data = data,
          color = ~ pal_fun(col_var_vector),
          popup = popup,
          label = ~ htmltools::htmlEscape(label_var_vector),
          fillOpacity = opacity, opacity = 1,
          weight = weight
        ) 
    }
    
    map %>% 
      addLegend(
        layerId = legend_id,
        colors = pal,
        labels = labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = opacity,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
  }
}

