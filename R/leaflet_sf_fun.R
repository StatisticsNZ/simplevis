# leaflet sf functions

#' @title Map of simple features in leaflet.
#' @description Map of simple features in leaflet that is not coloured. 
#' @param data An sf object of geometry type point/multipoint, linestring/multilinestring or polygon/multipolygon geometry type. Required input.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param popup HTML strings for use in popup. Defaults to making a leafpop::popupTable of all attribute columns in the sf object. 
#' @param radius Radius of points. Defaults to 1.
#' @param weight Stroke border size. Defaults to 2.
#' @param opacity The opacity of the fill. Defaults to 0.1. Only applicable to polygons.
#' @param stroke TRUE or FALSE of whether to draw a border around the features. Defaults to T.
#' @param title A title string that will be wrapped into the legend. Defaults to "Title"
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param legend_labels A vector of legend label values. Defaults to "Feature".
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param shiny TRUE or FALSE for whether the map is being run within a shiny app. Defaults to FALSE.
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
                       shiny = FALSE,
                       basemap = "light",
                       map_id = "map") {
  if (class(data)[1] != "sf")
    stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)))
    stop("Please assign a coordinate reference system")
  
  if (sf::st_is_longlat(data) == FALSE)
    data <- sf::st_transform(data, 4326)
  geometry_type <- unique(sf::st_geometry_type(data))
  
  if (is.null(pal))
    pal <- pal_snz
  
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
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." if categorical colour variable, NULL results in "category". If numeric variable, defaults to "quantile". Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param bin_cuts A vector of bin cuts applicable where col_method of "bin" is selected. The first number in the vector should be either -Inf or 0, and the final number Inf. If NULL, 'pretty' breaks are used. Only applicable where col_method equals "bin".
#' @param quantile_cuts A vector of probability cuts applicable where col_method of "quantile" is selected. The first number in the vector should 0 and the final number 1. Defaults to quartiles. Only applicable where col_method equals "quantile".
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param rev_pal Reverses the palette. Defaults to F.
#' @param col_scale_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to F.
#' @param popup HTML strings for use in popup. Defaults to making a leafpop::popupTable of all attribute columns in the sf object. 
#' @param radius Radius of points. Defaults to 1.
#' @param weight Stroke border size. Defaults to 2.
#' @param stroke TRUE or FALSE of whether to draw a border around the features. Defaults to T.
#' @param opacity The opacity of polygons. Defaults to 0.9.
#' @param remove_na TRUE or FALSE of whether to remove NAs of the colour variable. Defaults to F.
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param title A title string that will be wrapped into the legend. Defaults to "Title".
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param shiny TRUE or FALSE for whether the map is being run within a shiny app. Defaults to FALSE.
#' @param map_id The shiny map id for a leaflet map within a shiny app. For standard single-map apps, id "map" should be used. For dual-map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_sf_col(example_sf_nz_livestock, dairydens,
#'      col_method = "quantile", quantile_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1),
#'      title = "Dairy density in count per km\u00b2, 2017")
#'
#' leaflet_sf_col(example_sf_nz_livestock, dairydens,
#'      col_method = "bin", bin_cuts = c(0, 10, 50, 100, 150, 200, Inf), legend_digits = 0,
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
                           col_method = NULL,
                           bin_cuts = NULL,
                           quantile_cuts = c(0, 0.25, 0.5, 0.75, 1),
                           pal = NULL,
                           rev_pal = FALSE,
                           col_scale_drop = FALSE,
                           popup = leafpop::popupTable(sentence_spaced_colnames(data)),
                           radius = 1,
                           weight = 2,
                           opacity = 0.9,
                           stroke = TRUE,
                           remove_na = FALSE,
                           title = "[Title]",
                           legend_digits = 1,
                           legend_labels = NULL,
                           basemap = "light",
                           shiny = FALSE,
                           map_id = "map") {
  if (class(data)[1] != "sf")
    stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)))
    stop("Please assign a coordinate reference system")
  
  if (sf::st_is_longlat(data) == FALSE)
    data <- sf::st_transform(data, 4326)
  col_var <- rlang::enquo(col_var)
  
  if (remove_na == TRUE)
    data <- dplyr::filter_at(data, vars(!!col_var), dplyr::all_vars(!is.na(.data)))
  
  col_var_vector <- dplyr::pull(data, !!col_var)
  
  if (is.null(col_method) &
      !is.numeric(col_var_vector))
    col_method <- "category"
  if (is.null(col_method) &
      is.numeric(col_var_vector))
    col_method <- "quantile"
  
  if (col_method == "category") {
    if (is.null(legend_labels)){
      if (is.factor(col_var_vector) &  col_scale_drop == FALSE) labels <- levels(col_var_vector)
      else if (is.character(col_var_vector) | col_scale_drop == TRUE) labels <- sort(unique(col_var_vector))
    }
    else if (!is.null(legend_labels)) labels <- legend_labels
    
    n_col_var_values <- length(labels)
    
    if (is.null(pal))
      pal <- pal_point_set1[1:n_col_var_values]
    else if (!is.null(pal))
      pal <- pal[1:n_col_var_values]
    if (rev_pal == TRUE)
      pal <- rev(pal)
    pal <- stringr::str_sub(pal, 1, 7)
    
    pal_fun <-
      colorFactor(palette = pal,
                  domain = col_var_vector,
                  na.color = "#A8A8A8")
  }
  else if (col_method == "bin") {
    if (!is.null(bin_cuts)) {
      if (!(dplyr::first(bin_cuts) %in% c(0,-Inf)))
        warning(
          "The first element of the bin_cuts vector should generally be 0 (or -Inf if there are negative values)"
        )
      if (dplyr::last(bin_cuts) != Inf)
        warning("The last element of the bin_cuts vector should generally be Inf")
      if (is.null(pal))
        pal <- viridis::viridis(length(bin_cuts) - 1)
      else if (!is.null(pal))
        pal <- pal[1:(length(bin_cuts) - 1)]
      if (rev_pal == TRUE)
        pal <- rev(pal)
      pal <- stringr::str_sub(pal, 1, 7)
      pal_fun <-
        colorBin(
          palette = pal,
          domain = col_var_vector,
          bins = bin_cuts,
          right = FALSE,
          na.color = "#A8A8A8"
        )
      if (is.null(legend_labels))
        labels <- numeric_legend_labels(bin_cuts, legend_digits)
      else if (!is.null(legend_labels))
        labels <- legend_labels
    }
    else if (is.null(bin_cuts)) {
      bin_cuts <- pretty(col_var_vector)
      if (is.null(pal))
        pal <- viridis::viridis(length(bin_cuts) - 1)
      else if (!is.null(pal))
        pal <- pal[1:(length(bin_cuts) - 1)]
      if (rev_pal == TRUE)
        pal <- rev(pal)
      pal <- stringr::str_sub(pal, 1, 7)
      pal_fun <-
        colorBin(
          palette = pal,
          domain = col_var_vector,
          pretty = TRUE,
          right = FALSE,
          na.color = "#A8A8A8"
        )
      if (is.null(legend_labels))
        labels <- numeric_legend_labels(bin_cuts, legend_digits)
      else if (!is.null(legend_labels))
        labels <- legend_labels
    }
  }
  else if (col_method == "quantile") {
    if (dplyr::first(quantile_cuts) != 0)
      warning("The first element of the quantile_cuts vector generally always be 0")
    if (dplyr::last(quantile_cuts) != 1)
      warning("The last element of the quantile_cuts vector should generally be 1")
    if (is.null(pal))
      pal <- viridis::viridis(length(quantile_cuts) - 1)
    else if (!is.null(pal))
      pal <- pal[1:(length(quantile_cuts) - 1)]
    if (rev_pal == TRUE)
      pal <- rev(pal)
    pal <- stringr::str_sub(pal, 1, 7)
    bin_cuts <-
      quantile(col_var_vector, probs = quantile_cuts, na.rm = TRUE)
    if (anyDuplicated(bin_cuts) > 0)
      stop("quantile_cuts do not provide unique breaks")
    pal_fun <-
      colorBin(
        palette = pal,
        domain = col_var_vector,
        bins = bin_cuts,
        right = FALSE,
        na.color = "#A8A8A8"
      )
    if (is.null(legend_labels))
      labels <- numeric_legend_labels(bin_cuts, legend_digits)
    else if (!is.null(legend_labels))
      labels <- legend_labels
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
          label = ~ as.character(col_var_vector),
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
          label = ~ as.character(col_var_vector),
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
          label = ~ as.character(col_var_vector),
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
          label = ~ as.character(col_var_vector),
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
          label = ~ as.character(col_var_vector),
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
          label = ~ as.character(col_var_vector),
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
