# leaflet sf functions

#' @title Stars leaflet map.
#' @description Map of stars in leaflet that is not coloured. 
#' @param data A stars object. Required input.
#' @param pal Character vector of hex codes. 
#' @param alpha The opacity of the array values. 
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param map_id The shiny map id for a leaflet map within a shiny app. For standard single-map apps, id "map" should be used. For dual-map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' library(simplevis)
#' library(stars)
#' 
#' leaflet_stars(example_stars) 
#'   
leaflet_stars <- function(data,
                          pal = NULL,
                          alpha = 0.5,
                          basemap = "light",
                          map_id = "map")
{
  
  shiny <- shiny::isRunning()
  
  if (class(data) != "stars") stop("Please use an stars object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system")
  
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
  
  if (shiny == FALSE) {
    
    map <- leaflet() %>%
      leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                 title = "Reset View", 
                                                 onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
      htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
      addProviderTiles(basemap_name) %>%
      leafem::addStarsImage(
        x = data,
        colors = pal[1],
        opacity = alpha,
        project = TRUE
      ) 
  }
  else if (shiny == TRUE) {
    
    leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
    
    map <- leafletProxy(map_id) %>%
      leafem::addStarsImage(
        x = data,
        colors = pal[1],
        opacity = alpha,
        project = TRUE
      ) 
  }
  
  return(map)
}

#' @title Stars leaflet map that is coloured.
#' @description Map of stars in leaflet that is coloured. 
#' @param data A stars object. Required input.
#' @param col_var Unquoted attribute to colour the features by. Required input.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha The opacity of features. Defaults to 1.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param col_breaks_n For a numeric colour variable of "bin" col_method, the desired number of intervals on the colour scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "bin".
#' @param col_labels A function or named vector to modify the colour scale labels. Defaults to stringr::str_to_sentence if categorical, and scales::label_comma if numeric.
#' @param col_na_rm TRUE or FALSE of whether to visualise col_var NA values. Defaults to FALSE.
#' @param col_title A title string that will be wrapped into the legend. 
#' @param map_id The shiny map id for a leaflet map within a shiny app. For standard single-map apps, id "map" should be used. For dual-map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' library(simplevis)
#' library(stars)
#' 
#' leaflet_stars_col(example_stars, 
#'                   col_var = nitrate)
#' 
leaflet_stars_col <- function(data,
                              col_var,
                              pal = NULL,
                              pal_na = "#7F7F7F",
                              pal_rev = FALSE,
                              alpha = 1,
                              basemap = "light",
                              col_breaks_n = 5,
                              col_cuts = NULL,
                              col_intervals_right = TRUE, 
                              col_labels = NULL,
                              col_method = NULL,
                              col_na_rm = FALSE,
                              col_title = NULL,
                              map_id = "map"
) {
  
  shiny <- shiny::isRunning()
  
  if (class(data) != "stars") stop("Please use a stars object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system")
  
  col_var <- rlang::enquo(col_var)
  
  data <- data %>% 
    dplyr::select(!!col_var)
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (col_na_rm == TRUE) pal_na <- "transparent"
  
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "bin"
  }
  
  if (col_method %in% c("quantile", "bin")) {
    if (col_method == "bin") {
      if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
      else if (!is.null(col_cuts)) {
        if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
        if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      }
    }
    else if (col_method == "quantile") {
      if (is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
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
  
  col_id <- paste0(map_id, "_legend")
  
  if (shiny == FALSE) {
    
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
  }
  
  if (shiny == FALSE) {
    
    map <- leaflet() %>%
      leaflet::addEasyButton(leaflet::easyButton(icon = "ion-arrow-shrink", 
                                                 title = "Reset View", 
                                                 onClick = htmlwidgets::JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"))) %>% 
      htmlwidgets::onRender(htmlwidgets::JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")) %>% 
      addProviderTiles(basemap_name) %>%
      leafem::addStarsImage(
        x = data,
        colors = pal_fun,
        opacity = alpha,
        project = TRUE
      )
  }
  else if (shiny == TRUE) {
    
    leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(col_id)
    
    map <- leafletProxy(map_id) %>%
      leafem::addStarsImage(
        x = data,
        colors = pal_fun,
        opacity = alpha,
        project = TRUE
      )
  }
  
  if (col_na_rm == FALSE) {
    if(any(is.na(col_var_vctr))) {
      pal <- c(pal, pal_na)
      col_labels <- c(col_labels, "NA")
    }
  }
  
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  map <- map %>%
    addLegend(
      layerId = col_id,
      colors = pal,
      labels = col_labels,
      title = stringr::str_replace_all(stringr::str_wrap(col_title, 20), "\n", "</br>"),
      position = "bottomright",
      opacity = alpha)
  
  return(map)
}


