# leaflet sf functions

#' @title Simple feature leaflet map.
#' @description Map of simple features in leaflet that is not coloured. 
#' @param data An sf object of geometry type point/multipoint, linestring/multilinestring or polygon/multipolygon geometry type. Required input.
#' @param popup TRUE or FALSE of whether to have a popup.
#' @param popup_vars_vctr Vector of quoted variable names to include in the popup. If NULL, defaults to making a leafpop::popupTable of all columns.
#' @param popup_numeric_format A function to format all numeric variables within the popup column. Defaults to non-scientific. Use function(x) x to leave as is.
#' @param popup_vars_rename Function to rename column names for the popup. Defaults to snakecase::to_sentence_case. Use function(x) x to leave column names untransformed.
#' @param pal Character vector of hex codes.
#' @param alpha_point The opacity of the points. 
#' @param alpha_line The opacity of the outline. 
#' @param alpha_fill The opacity of the fill. 
#' @param size_point Size of points (i.e. radius). Defaults to 2.
#' @param size_line Size of lines around features (i.e. weight). Defaults to 2.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param layer_id_var Unquoted variable to be used as a shiny id, such that in the event where a feature is clicked on, the applicable value of this is available as input$map_marker_click$id or input$map_shape_click$id. 
#' @param map_id The shiny map id for a leaflet map within a shiny app. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaf_sf(example_point)
#' 
#' leaf_sf(example_polygon)
#' 
leaf_sf <- function(data,
                    popup = TRUE,
                    popup_vars_vctr = NULL,
                    popup_numeric_format = function(x) prettyNum(x, big.mark = "", scientific = FALSE),
                    popup_vars_rename = snakecase::to_sentence_case,
                    pal = pal_viridis_reorder(1),
                    size_point = 2,
                    size_line = 2,
                    alpha_point = NULL,
                    alpha_line = NULL,
                    alpha_fill = NULL,
                    basemap = "light",
                    layer_id_var = NULL,
                    map_id = "map")
{
  #ungroup
  data <- dplyr::ungroup(data)
  
  #shiny
  shiny <- shiny::isRunning()
  
  #warnings
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system")
  
  #transform
  if (sf::st_is_longlat(data) == FALSE) data <- sf::st_transform(data, 4326)
  
  #geometry
  geometry_type <- unique(sf::st_geometry_type(data))
  
  #colour
  pal <- pal[1]

  #basemap
  if (shiny == FALSE) {
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
  }
  
  #popup
  if (popup == TRUE) {
    if (is.null(popup_vars_vctr)){
      popup_data <- data %>%
        dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
        dplyr::rename_with(popup_vars_rename) 
    }
    else {
      popup_data <- data %>%
        dplyr::select(popup_vars_vctr) %>%
        dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
        dplyr::rename_with(popup_vars_rename) 
    }
    
    popup_data <- popup_data %>%
        dplyr::mutate_if(.predicate = is.numeric, .funs = popup_numeric_format)
    
    popup <- leafpop::popupTable(popup_data, zcol = 1:ncol(popup_data) - 1, row.numbers = FALSE, feature.id = FALSE)
  }
  else popup <- NULL
  
  #layer id
  if (!is.null(rlang::get_expr(layer_id_var))) {
    layer_id_var <- rlang::enquo(layer_id_var)
    
    layer_id_var <- dplyr::pull(data, !!layer_id_var)  
  }
  
  #fundamentals
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    if (is.null(alpha_point)) {
      if(is.null(alpha_line) & is.null(alpha_fill)) {
        alpha_line <- 1
        alpha_fill <- 1
      }
    } else {
      alpha_line <- alpha_point
      alpha_fill <- alpha_point
    }
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                   title = "Reset View", 
                                                   onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
        htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
        addProviderTiles(basemap_name) %>%
        addCircleMarkers(
          data = data, 
          layerId = ~ layer_id_var, 
          popup = ~ popup,
          color = pal[1],
          radius = size_point,
          fillOpacity = alpha_fill,
          opacity = alpha_line,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearPopups() %>% clearShapes() %>% clearImages() %>% clearControls()
      
      map <- leafletProxy(map_id) %>%
        addCircleMarkers(
          data = data, 
          layerId = ~ layer_id_var, 
          popup = ~ popup,
          color = pal[1],
          radius = size_point,
          fillOpacity = alpha_fill,
          opacity = alpha_line,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    if (is.null(alpha_line)) alpha_line <- 1
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                   title = "Reset View", 
                                                   onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
        htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
        addProviderTiles(basemap_name) %>%
        addPolylines(
          data = data, 
          layerId = ~ layer_id_var, 
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha_line,
          opacity = alpha_line,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearPopups() %>% clearShapes() %>% clearImages() %>% clearControls()
      
      map <- leafletProxy(map_id) %>%
        addPolylines(
          data = data, 
          layerId = ~ layer_id_var, 
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha_line,
          opacity = alpha_line,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha_line)) alpha_line <- 1
    if (is.null(alpha_fill)) alpha_fill <- 1
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                   title = "Reset View", 
                                                   onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
        htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
        addProviderTiles(basemap_name) %>%
        addPolygons(
          data = data, 
          layerId = ~ layer_id_var, 
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha_fill, 
          opacity = alpha_line,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearPopups() %>% clearShapes() %>% clearImages() %>% clearControls()
      
      map <- leafletProxy(map_id) %>%
        addPolygons(
          data = data, 
          layerId = ~ layer_id_var, 
          popup = ~ popup,
          color = pal[1],
          fillOpacity = alpha_fill, 
          opacity = alpha_line,
          weight = size_line
        )
    }
  }
  
  return(map)
}

#' @title Simple feature leaflet map that is coloured.
#' @description Map of simple features in leaflet that is coloured. 
#' @param data An sf object of geometry type point/multipoint, linestring/multilinestring or polygon/multipolygon geometry type. Required input.
#' @param col_var Unquoted variable to colour the features by. Required input.
#' @param label_var Unquoted variable to label the features by. If NULL, defaults to using the colour variable.
#' @param popup TRUE or FALSE of whether to have a popup.
#' @param popup_vars_vctr Vector of quoted variable names to include in the popup. If NULL, defaults to making a leafpop::popupTable of all columns.
#' @param popup_numeric_format A function to format all numeric variables within the popup column. Defaults to non-scientific. Use function(x) x to leave as is.
#' @param popup_vars_rename Function to rename column names for the popup. Defaults to snakecase::to_sentence_case. Use function(x) x to leave column names untransformed.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_point The opacity of the points. 
#' @param alpha_line The opacity of the outline. 
#' @param alpha_fill The opacity of the fill. 
#' @param size_point Size of points (i.e. radius). Defaults to 2.
#' @param size_line Size of lines around features (i.e. weight). Defaults to 2.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param col_breaks_n For a numeric colour variable, the desired number of intervals on the colour scale. 
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels A function or named vector to modify the colour scale labels. Defaults to snakecase::to_sentence_case if categorical, and scales::label_comma() if numeric. Use function(x) x to keep labels untransformed.
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.  
#' @param col_method The method of colouring features, either "bin", "quantile", "continuous", or "category." If numeric, defaults to "bin".
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title A title string that will be wrapped into the legend. 
#' @param label_numeric_format A function to format the numeric labels. Defaults to adding a comma seperator. Use function(x) x to leave as is.
#' @param layer_id_var Unquoted variable to be used as a shiny id, such that in the event where a feature is clicked on, the applicable value of this is available as input$map_marker_click$id or input$map_shape_click$id. 
#' @param map_id The shiny map id for a leaflet map within a shiny app. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaf_sf_col(example_point,
#'               col_var = trend_category)
#'
#' leaf_sf_col(example_polygon,
#'               col_var = density)
#'
#' leaf_sf_col(example_polygon,
#'               col_var = density,
#'               col_method = "bin",
#'               col_breaks_n = 5)
#'
#' leaf_sf_col(example_polygon,
#'               col_var = density,
#'               col_method = "bin",
#'               col_cuts = c(0, 10, 50, 100, 150, 200, Inf))
#'
#' leaf_sf_col(example_polygon,
#'               col_var = density,
#'               col_method = "quantile",
#'               col_breaks_n = 4)
#'
#' leaf_sf_col(example_polygon,
#'               col_var = density,
#'               col_method = "quantile",
#'               col_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1))
#'
leaf_sf_col <- function(data,
                        col_var,
                        label_var = NULL,
                        popup = TRUE,
                        popup_vars_vctr = NULL,
                        popup_numeric_format = function(x) prettyNum(x, big.mark = "", scientific = FALSE),
                        popup_vars_rename = snakecase::to_sentence_case,
                        pal = NULL,
                        pal_na = "#7F7F7F",
                        pal_rev = FALSE,
                        alpha_point = NULL,
                        alpha_line = NULL,
                        alpha_fill = NULL,
                        size_point = 2,
                        size_line = 2,
                        basemap = "light",
                        col_breaks_n = 4,
                        col_cuts = NULL,
                        col_intervals_right = TRUE,
                        col_labels = NULL,
                        col_legend_none = FALSE,
                        col_method = NULL,
                        col_na_rm = FALSE,
                        col_title = NULL,
                        label_numeric_format = function(x) prettyNum(x, big.mark = ",", scientific = FALSE),
                        layer_id_var = NULL,
                        map_id = "map") {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #shiny
  shiny <- shiny::isRunning()
  
  #warnings
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system")
  
  if (!is.null(col_method)) {
    if (!col_method %in% c("continuous", "bin", "quantile", "category")) stop("Please use a colour method of 'continuous', 'bin', 'quantile' or 'category'")
  }
  
  #transform
  if (sf::st_is_longlat(data) == FALSE) data <- sf::st_transform(data, 4326)
  
  #geometry
  geometry_type <- unique(sf::st_geometry_type(data))
  
  #quote
  col_var <- rlang::enquo(col_var)
  label_var <- rlang::enquo(label_var)
  if (is.null(rlang::get_expr(label_var))) label_var <- col_var

  #na's
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  #vectors
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  label_var_vctr <- data %>% 
    dplyr::select(!!label_var) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = label_numeric_format) %>% 
    dplyr::pull(!!label_var)

  #logical to factor
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  #colour
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "continuous"
  }
  
  if (col_method == "continuous") {
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
    if (is.null(pal)) pal <- viridis::viridis(20)
    if (pal_rev == TRUE) pal <- rev(pal)
    
    pal_fun <- colorNumeric(
      palette = pal,
      domain = col_var_vctr,
      na.color = pal_na
    )
  }
  else if (col_method %in% c("quantile", "bin")) {
    if (col_method == "bin") {
      if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
      else if (!is.null(col_cuts)) {
        if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
        if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      }
    }
    else if (col_method == "quantile") {
      if (is.null(col_cuts)) col_cuts <- seq(0, 1, 1 / col_breaks_n)
      else {
        if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
        if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
      }  
      
      col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
    }
    if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
    
    if (is.null(pal)) pal <- pal_viridis_reorder(length(col_cuts) - 1)
    else if (!is.null(pal)) pal <- pal[1:(length(col_cuts) - 1)]
    if (pal_rev == TRUE) pal <- rev(pal)
    
    pal_fun <- colorBin(
      palette = pal,
      domain = col_var_vctr,
      bins = col_cuts,
      right = col_intervals_right,
      na.color = pal_na
    )
    
    if (is.function(col_labels) | is.null(col_labels)) {
      col_labels <- sv_interval_labels_num(col_cuts, format = col_labels, right_closed = col_intervals_right)
    }
  }
  else if (col_method == "category") {
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      col_labels2 <- levels(col_var_vctr)
      col_n <- length(col_labels2)
    }
    else ({
      col_labels2 <- unique(col_var_vctr)
      col_labels2 <- sort(col_labels2[!is.na(col_labels2)])
      col_n <- length(col_labels2)
    }) 
    
    if (is.null(pal)) pal <- pal_d3_reorder(col_n)
    else pal <- pal[1:col_n]
    
    if (is.function(col_labels)) col_labels <- col_labels(col_labels2)
    else if (is.null(col_labels)) col_labels <- snakecase::to_sentence_case(col_labels2)
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    pal_fun <- colorFactor(palette = pal,
                           domain = col_var_vctr,
                           na.color = pal_na)
  }
  
  #basemap
  if (shiny == FALSE) {
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
  }
  
  #popup
  if (popup == TRUE) {
    if (is.null(popup_vars_vctr)){
      popup_data <- data %>%
        dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
        dplyr::rename_with(popup_vars_rename)
    }
    else {
      popup_data <- data %>%
        dplyr::select(popup_vars_vctr) %>%
        dplyr::relocate(.data$geometry, .after = tidyselect::last_col()) %>%
        dplyr::rename_with(popup_vars_rename) 
    }
    
    popup_data <- popup_data %>%
      dplyr::mutate_if(.predicate = is.numeric, .funs = popup_numeric_format)
    
    popup <- leafpop::popupTable(popup_data, zcol = 1:ncol(popup_data) - 1, row.numbers = FALSE, feature.id = FALSE)
  }
  else popup <- NULL
  
  #layer id
  layer_id_var <- rlang::enquo(layer_id_var)
  
  if (!is.null(rlang::get_expr(layer_id_var))) {
    layer_id_var <- dplyr::pull(data, !!layer_id_var) 
  }
  
  #fundamentals
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    if (is.null(alpha_point)) {
      if(is.null(alpha_line) & is.null(alpha_fill)) {
        alpha_line <- 1
        alpha_fill <- 1
      }
    } else {
      alpha_line <- alpha_point
      alpha_fill <- alpha_point
    }
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                   title = "Reset View", 
                                                   onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
        htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
        addProviderTiles(basemap_name) %>%
        addCircleMarkers(
          data = data, 
          layerId = ~ layer_id_var, 
          color = ~ pal_fun(col_var_vctr),
          label = ~ label_var_vctr,
          popup = ~ popup,
          radius = size_point,
          fillOpacity = alpha_fill,
          opacity = alpha_line,
          weight = size_line
        )
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearPopups() %>% clearShapes() %>% clearImages() %>% clearControls()
      
      map <- leafletProxy(map_id) %>%
        addCircleMarkers(
          data = data, 
          layerId = ~ layer_id_var, 
          color = ~ pal_fun(col_var_vctr),
          label = ~ label_var_vctr,
          popup = ~ popup,
          radius = size_point,
          fillOpacity = alpha_fill,
          opacity = alpha_line,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    if (is.null(alpha_line)) alpha_line <- 1
    
    if (shiny == FALSE) {
      
      map <- leaflet() %>%
        leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                   title = "Reset View", 
                                                   onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
        htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
        addProviderTiles(basemap_name) %>%
        addPolylines(
          data = data, 
          layerId = ~ layer_id_var, 
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ label_var_vctr,
          fillOpacity = alpha_fill,
          opacity = alpha_line,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearPopups() %>% clearShapes() %>% clearImages() %>% clearControls()
      
      map <- leafletProxy(map_id) %>%
        addPolylines(
          data = data, 
          layerId = ~ layer_id_var, 
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ label_var_vctr,
          fillOpacity = alpha_fill,
          opacity = alpha_line,
          weight = size_line
        ) 
    }
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha_line)) alpha_line <- 1
    if (is.null(alpha_fill)) alpha_fill <- 1
    
    if (shiny == FALSE) {
      map <- leaflet() %>%
        leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                   title = "Reset View", 
                                                   onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
        htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
        addProviderTiles(basemap_name) %>%
        addPolygons(
          data = data, 
          layerId = ~ layer_id_var, 
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ label_var_vctr,
          fillOpacity = alpha_fill, 
          opacity = alpha_line,
          weight = size_line
        ) 
    }
    else if (shiny == TRUE) {
      leafletProxy(map_id) %>% clearMarkers() %>% clearPopups() %>% clearShapes() %>% clearImages() %>% clearControls()
      
      map <- leafletProxy(map_id) %>%
        addPolygons(
          data = data, 
          layerId = ~ layer_id_var, 
          color = ~ pal_fun(col_var_vctr),
          popup = ~ popup,
          label = ~ label_var_vctr,
          fillOpacity = alpha_fill, 
          opacity = alpha_line,
          weight = size_line
        ) 
    }
  }
  
  #legend NA
  if (col_method %in% c("bin", "quantile", "category")) {
    if (col_na_rm == FALSE) {
      if(any(is.na(col_var_vctr))) {
        pal <- c(pal, pal_na)
        col_labels <- c(col_labels, "NA")
      }
    }
  }
  
  #titles
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #legend
  if (col_legend_none == FALSE) {
    if (col_method == "continuous") {
      map <- map %>% 
        addLegend(
          pal = pal_fun,
          values = col_var_vctr,
          bins = col_cuts,
          title = stringr::str_replace_all(stringr::str_wrap(col_title, 20), "\n", "</br>"),
          position = "bottomright",
          opacity = alpha_line)  # ideally have fillOpacity = alpha_fill argument here
    }
    else if (col_method %in% c("bin", "quantile", "category")) {
      map <- map %>% 
        addLegend(
          colors = pal,
          labels = col_labels,
          title = stringr::str_replace_all(stringr::str_wrap(col_title, 20), "\n", "</br>"),
          position = "bottomright",
          opacity = alpha_line) # ideally have fillOpacity = alpha_fill argument here
    }
  }
  
  return(map)
}

