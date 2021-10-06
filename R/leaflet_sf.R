# leaflet sf functions

#' @title Simple feature leaflet map.
#' @description Map of simple features in leaflet that is not coloured. 
#' @param data An sf object of geometry type point/multipoint, linestring/multilinestring or polygon/multipolygon geometry type. Required input.
#' @param popup_vars_vctr Vector of quoted variable names to include in the popup. If NULL, defaults to making a leafpop::popupTable of all columns.
#' @param pal Character vector of hex codes. 
#' @param size_point Size of points (i.e. radius). Defaults to 2.
#' @param size_line Size of lines around features (i.e. weight). Defaults to 2.
#' @param alpha The opacity of features. Defaults to 1 for points/lines, or 0.75 for polygons.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param title A title string that will be wrapped into the legend. 
#' @param map_id The shiny map id for a leaflet map within a shiny app. For standard single-map apps, id "map" should be used. For dual-map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_sf(example_sf_point)
#' 
#' leaflet_sf(example_sf_polygon)
#' 
leaflet_sf <- function(data,
                       popup_vars_vctr = NULL,
                       pal = NULL,
                       size_point = 2,
                       size_line = 2,
                       alpha = NULL,
                       basemap = "light",
                       title = NULL,
                       map_id = "map")
{
  
  data <- dplyr::ungroup(data)
  shiny <- shiny::isRunning()
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if (sf::st_is_longlat(data) == FALSE) data <- sf::st_transform(data, 4326)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  col_id <- paste0(map_id, "_legend")
  
  if (shiny == FALSE) {
    
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
  }
  
  if(is.null(popup_vars_vctr)){
    popup_data <- data %>%
      dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
      janitor::clean_names(case = "sentence") 
  }
  else {
    popup_data <- data %>%
      dplyr::select(popup_vars_vctr) %>%
      dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
      janitor::clean_names(case = "sentence") 
  }
  
  popup <- leafpop::popupTable(popup_data, zcol = 1:ncol(popup_data) - 1, row.numbers = FALSE, feature.id = FALSE)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    if (is.null(alpha)) alpha <- 1
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addCircleMarkers(
          data = data,
          popup = ~ popup,
          color = pal[1],
          radius = size_point,
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
      
      map <- leafletProxy(map_id) %>%
        addCircleMarkers(
          data = data,
          popup = ~ popup,
          color = pal[1],
          radius = size_point,
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    if (is.null(alpha)) alpha <- 1
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolylines(
          data = data,
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
      
      map <- leafletProxy(map_id) %>%
        addPolylines(
          data = data,
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha)) alpha <- 0.75
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolygons(
          data = data,
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha, 
          opacity = alpha,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
      
      map <- leafletProxy(map_id) %>%
        addPolygons(
          data = data,
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha, 
          opacity = alpha,
          weight = size_line
        )
    }
  }
  
  map <- map %>% 
    addLegend(
      layerId = col_id,
      colors = pal[1],
      labels =  "Feature", 
      title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
      position = "bottomright",
      opacity = alpha
    )
  
  return(map)
}

#' @title Simple feature leaflet map that is coloured.
#' @description Map of simple features in leaflet that is coloured. 
#' @param data An sf object of geometry type point/multipoint, linestring/multilinestring or polygon/multipolygon geometry type. Required input.
#' @param col_var Unquoted variable to colour the features by. Required input.
#' @param label_var Unquoted variable to label the features by. If NULL, defaults to using the colour variable.
#' @param popup_vars_vctr Vector of quoted variable names to include in the popup. If NULL, defaults to making a leafpop::popupTable of all columns.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param size_point Size of points (i.e. radius). Defaults to 2.
#' @param size_line Size of lines around features (i.e. weight). Defaults to 2.
#' @param alpha The opacity of features. Defaults to 1 for points/lines, or 0.95 for polygons.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param title A title string that will be wrapped into the legend. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels A vector to modify colour scale labels.  
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "bin".
#' @param col_na TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.
#' @param col_pretty_n For a numeric colour variable of "bin" col_method, the desired number of intervals on the colour scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param col_right_closed For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param map_id The shiny map id for a leaflet map within a shiny app. For standard single-map apps, id "map" should be used. For dual-map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_sf_col(example_sf_point, 
#'                col_var = trend_category)
#' 
#' leaflet_sf_col(example_sf_polygon, 
#'                col_var = density, 
#'                col_method = "quantile", 
#'                col_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1))
#'
#' leaflet_sf_col(example_sf_polygon, 
#'                col_var = density, 
#'                col_method = "bin", 
#'                col_cuts = c(0, 10, 50, 100, 150, 200, Inf))
#'
leaflet_sf_col <- function(data,
                           col_var,
                           label_var = NULL,
                           popup_vars_vctr = NULL,
                           pal = NULL,
                           pal_na = "#7F7F7F",
                           pal_rev = FALSE,
                           size_point = 2,
                           size_line = 2,
                           alpha = NULL,
                           basemap = "light",
                           title = NULL,
                           col_cuts = NULL,
                           col_labels = NULL,
                           col_method = NULL,
                           col_na = TRUE,
                           col_pretty_n = 4,
                           col_right_closed = TRUE, 
                           map_id = "map"
) {
  
  data <- dplyr::ungroup(data)
  shiny <- shiny::isRunning()
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if (sf::st_is_longlat(data) == FALSE) data <- sf::st_transform(data, 4326)
  
  col_var <- rlang::enquo(col_var)
  label_var <- rlang::enquo(label_var)
  if(is.null(rlang::get_expr(label_var))) label_var <- col_var
  
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  label_var_vctr <- dplyr::pull(data, !!label_var)
  
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "bin"
  }
  
  if(col_method %in% c("quantile", "bin")) {
    if (col_method == "bin") {
      if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_pretty_n)
      else if (!is.null(col_cuts)) {
        if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
        if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      }
      
      if (is.null(pal)) pal <- pal_viridis_reorder(length(col_cuts) - 1)
      else if (!is.null(pal)) pal <- pal[1:(length(col_cuts) - 1)]
      if (pal_rev == TRUE) pal <- rev(pal)
      pal <- stringr::str_sub(pal, 1, 7)
      
      pal_fun <- colorBin(
        palette = pal,
        domain = col_var_vctr,
        bins = col_cuts,
        pretty = FALSE,
        right = col_right_closed,
        na.color = pal_na
      )
      
      if (is.null(col_labels)) col_labels <- sv_interval_labels_num(col_cuts, right_closed = col_right_closed)  
    }
    else if (col_method == "quantile") {
      if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
      else {
        if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
        if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
      }  
      if (is.null(pal)) pal <- pal_viridis_reorder(length(col_cuts) - 1)
      else if (!is.null(pal)) pal <- pal[1:(length(col_cuts) - 1)]
      if (pal_rev == TRUE) pal <- rev(pal)
      pal <- stringr::str_sub(pal, 1, 7)
      
      col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
    }
    if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
    
    pal_fun <- colorBin(
      palette = pal,
      domain = col_var_vctr,
      bins = col_cuts,
      right = col_right_closed,
      na.color = pal_na
    )
    
    if (is.null(col_labels)) col_labels <- sv_interval_labels_num(col_cuts, right_closed = col_right_closed)  
  }
  else if (col_method == "category") {
    if (is.null(col_labels)) {
      if (is.factor(col_var_vctr)) col_labels <- levels(col_var_vctr)
      else if (is.character(col_var_vctr)) col_labels <- sort(unique(col_var_vctr))
    }
    
    col_n <- length(col_labels)
    
    if (is.null(pal)) pal <- pal_d3_reorder(col_n)
    else if (!is.null(pal)) pal <- pal[1:col_n]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    pal <- stringr::str_sub(pal, 1, 7)
    
    pal_fun <- colorFactor(palette = pal,
                           domain = col_var_vctr,
                           na.color = pal_na)
  }
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  col_id <- paste0(map_id, "_legend")
  
  if (shiny == FALSE) {
    
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
  }
  
  if(is.null(popup_vars_vctr)){
    popup_data <- data %>%
      dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
      janitor::clean_names(case = "sentence") 
  }
  else {
    popup_data <- data %>%
      dplyr::select(popup_vars_vctr) %>%
      dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
      janitor::clean_names(case = "sentence") 
  }
  
  popup <- leafpop::popupTable(popup_data, zcol = 1:ncol(popup_data) - 1, row.numbers = FALSE, feature.id = FALSE)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    if (is.null(alpha)) alpha <- 1
        
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addCircleMarkers(
          data = data,
          color = ~ pal_fun(col_var_vctr),
          label = ~ htmltools::htmlEscape(label_var_vctr),
          popup = ~ popup,
          radius = size_point,
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        )
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
      
      map <- leafletProxy(map_id) %>%
        addCircleMarkers(
          data = data,
          color = ~ pal_fun(col_var_vctr),
          label = ~ htmltools::htmlEscape(label_var_vctr),
          popup = ~ popup,
          radius = size_point,
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    if (is.null(alpha)) alpha <- 1
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolylines(
          data = data,
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ htmltools::htmlEscape(label_var_vctr),
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
      
      map <- leafletProxy(map_id) %>%
        addPolylines(
          data = data,
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ htmltools::htmlEscape(label_var_vctr),
          fillOpacity = alpha,
          opacity = alpha,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha)) alpha <- 0.95
    
    if (shiny == FALSE) {
      map <- leaflet() %>%
        addProviderTiles(basemap_name) %>%
        addPolygons(
          data = data,
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ htmltools::htmlEscape(label_var_vctr),
          fillOpacity = alpha, 
          opacity = alpha,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
      
      map <- leafletProxy(map_id) %>%
        addPolygons(
          data = data,
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ htmltools::htmlEscape(label_var_vctr),
          fillOpacity = alpha, 
          opacity = alpha,
          weight = size_line
        ) 
    }
  }
  
  map <- map %>% 
    addLegend(
      layerId = col_id,
      colors = pal,
      labels = col_labels,
      title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
      position = "bottomright",
      opacity = alpha)
  
  return(map)
}

