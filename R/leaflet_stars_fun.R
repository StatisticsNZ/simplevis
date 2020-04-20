# leaflet stars functions

#' @title Map of an array in leaflet.
#' @description Map of an array in leaflet. 
#' @param data A stars object with dimensions x and y with crs in wgs84 (epsg4326). Required input.
#' @param pal Character vector of hex codes, or provided objects with pal_ prefixes.
#' @param opacity Sets the opacity of the grid cells. Defaults to 0.1.
#' @param title A title string that will be wrapped into the legend. Defaults to "Title".
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param legend_labels A vector of legend label values. Defaults to "[Array]".
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param shiny TRUE or FALSE for whether the map is being run within a shiny app. Defaults to FALSE.
#' @param map_id This argument is only relevant for within apps. For single map shiny apps, the id "map" can be used. For dual map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_stars(example_stars_nz_no3n)
leaflet_stars <- function(data,
                          pal = NULL,
                          opacity = 0.5,
                          title = "[Title]",
                          legend_digits = 1,
                          legend_labels = "[Array]",
                          basemap = "light",
                          shiny = FALSE,
                          map_id = "map") {
  if (class(data) != "stars")
    stop("Please use an stars object as data input")
  if (is.na(sf::st_crs(data)))
    stop("Please assign a coordinate reference system")
  
  data <- methods::as(data, "Raster")
  data <- projectRasterForLeaflet(data, method = "ngb")
  
  if (is.null(pal))
    pal <- pal_snz[1]
  pal_fun <-
    colorBin(
      palette = pal,
      domain = c(0, 1),
      bins = c(0, 1),
      right = FALSE,
      na.color = pal
    )
  
  if (shiny == FALSE) {
    
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
    
    leaflet() %>%
      addProviderTiles(basemap_name) %>%
      addRasterImage(
        x = data,
        colors = pal_fun,
        opacity = opacity,
        project = FALSE
      ) %>%
      addLegend(
        colors = pal,
        labels = legend_labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = opacity,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
  }
  else if (shiny == TRUE) {
    legend_id <- paste0(map_id, "_legend")
    leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
    
    leafletProxy(map_id) %>%
      addRasterImage(
        x = data,
        colors = pal_fun,
        opacity = opacity,
        project = FALSE
      ) %>%
      addLegend(
        layerId = legend_id,
        colors = pal,
        labels = legend_labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = opacity,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
  }
}

#' @title Map of an array in leaflet that is coloured.
#' @description Map of an array in leaflet that is coloured. 
#' @param data A stars object with dimensions x and y, and 1 attribute layer with crs in wgs84 (epsg4326). Required input.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." Defaults to "quantile". Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param bin_cuts A vector of bin cuts applicable where col_method of "bin" is selected. The first number in the vector should be either -Inf or 0, and the final number Inf. If NULL, 'pretty' breaks are used. Only applicable where col_method equals "bin".
#' @param quantile_cuts A vector of probability cuts applicable where col_method of "quantile" is selected. The first number in the vector should 0 and the final number 1. Defaults to 0-0.05, 0.05-0.25, 0.25-0.5, 0.5-0.75, 0.75-0.95, and 0.95-1. Only applicable where col_method equals "quantile".
#' @param pal Character vector of hex codes, or provided objects with pal_ prefixes. Defaults to viridis.
#' @param rev_pal Reverses the palette. Defaults to F.
#' @param opacity Sets the opacity of the grid cells. Defaults to 0.9.
#' @param legend_digits Select the appropriate number of decimal places for the auto legend. Defaults to 1.
#' @param title A title string that will be wrapped into the legend. Defaults to "Title".
#' @param legend_labels A vector of legend label values. Defaults to NULL, which results in automatic labels.
#' @param basemap The underlying basemap. Either "light", "dark", "satellite", "street", or "ocean". Defaults to "light". Only applicable where shiny equals FALSE.
#' @param shiny TRUE or FALSE for whether the map is being run within a shiny app. Defaults to FALSE.
#' @param map_id This argument is only relevant for within apps. For single map shiny apps, the id "map" can be used. For dual map apps, "map1" and "map2" should be used. Defaults to "map".
#' @return A leaflet object.
#' @export
#' @examples
#' leaflet_stars_col(example_stars_nz_no3n,
#'    title = "River modelled median nitrate-nitrogen concentrations in g/m\u00b3, 2013\u201317")
leaflet_stars_col <- function(data,
                              col_method = "quantile",
                              quantile_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                              bin_cuts = NULL,
                              pal = NULL,
                              rev_pal = FALSE,
                              opacity = 1,
                              legend_digits = 1,
                              title = "[Title]",
                              legend_labels = NULL,
                              basemap = "light",
                              shiny = FALSE,
                              map_id = "map") {
  if (class(data) != "stars")
    stop("Please use an stars object as data input")
  if (is.na(sf::st_crs(data)))
    stop("Please assign a coordinate reference system")
  
  # data <- data %>% dplyr::select(1)
  
  col_var_vector <- data %>% dplyr::pull()
  
  if (col_method == "category") {
    no_bins <-
      max(col_var_vector, na.rm = TRUE) - min(col_var_vector, na.rm = TRUE) + 1
    max_bin_cut <- max(col_var_vector, na.rm = TRUE) + 1
    bin_cuts <- seq(min(col_var_vector, na.rm = TRUE), max_bin_cut, 1)
    if (is.null(pal))
      pal <- pal_point_set1[1:(length(bin_cuts) - 1)]
    else if (!is.null(pal))
      pal <- pal[1:(length(bin_cuts) - 1)]
    if (rev_pal == TRUE)
      pal <- rev(pal)
    pal_fun <-
      colorBin(
        palette = pal,
        domain = col_var_vector,
        bins = bin_cuts,
        right = FALSE,
        na.color = "transparent"
      )
    pal <- stringr::str_sub(pal, 1, 7)
    if (is.null(legend_labels))
      labels <- LETTERS[1:length(bin_cuts) - 1]
    else if (!is.null(legend_labels))
      labels <- legend_labels
  }
  else if (col_method == "bin") {
    if (!is.null(bin_cuts)) {
      if (!(dplyr::first(bin_cuts) %in% c(0, -Inf)))
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
      pal_fun <-
        colorBin(
          palette = pal,
          domain = col_var_vector,
          bins = bin_cuts,
          right = FALSE,
          na.color = "transparent"
        )
      pal <- stringr::str_sub(pal, 1, 7)
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
      pal_fun <-
        colorBin(
          palette = pal,
          domain = col_var_vector,
          pretty = TRUE,
          right = FALSE,
          na.color = "transparent"
        )
      pal <- stringr::str_sub(pal, 1, 7)
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
    bin_cuts <-
      stats::quantile(col_var_vector, probs = quantile_cuts, na.rm = TRUE)
    pal_fun <-
      colorBin(
        palette = pal,
        domain = col_var_vector,
        bins = bin_cuts,
        right = FALSE,
        na.color = "transparent"
      )
    pal <- stringr::str_sub(pal, 1, 7)
    if (is.null(legend_labels))
      labels <- numeric_legend_labels(bin_cuts, legend_digits)
    else if (!is.null(legend_labels))
      labels <- legend_labels
  }
  
  data <- methods::as(data, "Raster")
  if (col_method %in% c("quantile", "bin"))
    data <- projectRasterForLeaflet(data, method = "bilinear")
  else if (col_method == "category")
    data <- projectRasterForLeaflet(data, method = "ngb")
  
  if (shiny == FALSE) {
    
    if(basemap == "light") basemap_name <- "CartoDB.PositronNoLabels"
    else if(basemap == "dark") basemap_name <- "CartoDB.DarkMatterNoLabels"
    else if(basemap == "satellite") basemap_name <- "Esri.WorldImagery"
    else if(basemap == "ocean") basemap_name <- "Esri.OceanBasemap"
    else if(basemap == "street") basemap_name <- "OpenStreetMap.Mapnik"
    else basemap_name <- "CartoDB.PositronNoLabels"
    
    leaflet() %>%
      addProviderTiles(basemap_name) %>%
      addRasterImage(
        x = data,
        colors = pal_fun,
        opacity = opacity,
        project = FALSE
      ) %>%
      addLegend(
        colors = pal,
        labels = labels,
        title = stringr::str_replace_all(stringr::str_wrap(title, 20), "\n", "</br>"),
        position = "bottomright",
        opacity = opacity,
        labFormat = labelFormat(between = "&ndash;", digits = legend_digits)
      )
  }
  else if (shiny == TRUE) {
    legend_id <- paste0(map_id, "_legend")
    leafletProxy(map_id) %>% clearMarkers() %>% clearShapes() %>% clearImages() %>% removeControl(legend_id)
    
    leafletProxy(map_id) %>%
      addRasterImage(
        x = data,
        colors = pal_fun,
        opacity = opacity,
        project = FALSE
      ) %>%
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
