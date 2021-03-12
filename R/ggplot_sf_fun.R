# ggplot sf functions

#' @title Theme for ggplot maps of simple features.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#' 
#' ggplot() +
#'   theme_sf("Courier", 9, 7) +
#'   ggtitle("This is a title of a selected font family and size")
theme_sf <-
  function(font_family = "Helvetica",
           font_size_title = 11,
           font_size_body = 10) {
    list(
      theme(
        plot.title = element_text(
          family = font_family,
          colour = "#000000",
          size = font_size_title,
          face = "bold",
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          family = font_family,
          colour = "#000000",
          size = font_size_body,
          face = "plain",
          hjust = 0.5
        ),
        plot.caption = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          face = "plain",
          hjust = 0.99
        ),
        plot.margin = margin(
          t = 5,
          l = 5,
          b = 5,
          r = 20
        ),
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        strip.background = element_rect(colour = "white", fill = "white"),
        text = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body
        ),
        strip.text = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          margin = margin(r = 10),
          hjust = 0
        ),
        legend.title = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          margin = margin(r = 20)
        ),
        legend.position = "bottom",
        legend.key = element_rect(fill = "white"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm")
      )
    )
  }

#' @title Map of simple features in ggplot.
#' @description Map of simple features in ggplot that is not coloured and not facetted. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param size Size of features (or shape outlines if polygon). Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 0.1. Only applicable to polygons.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param boundary A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. 
#' @param boundary_behind TRUE or FALSE  as to whether the boundary is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param boundary_pal Colour of the boundary. Defaults to "#7F7F7F".
#' @param boundary_size Size of the boundary. Defaults to 0.2.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_sf(data = example_sf_point, boundary = nz)
ggplot_sf <- function(data,
                      size = 0.5,
                      alpha = 0.1,
                      pal = NULL,
                      boundary = NULL,
                      boundary_behind = TRUE,
                      boundary_pal = "#7f7f7f",
                      boundary_size = 0.2,
                      title = "[Title]",
                      subtitle = NULL,
                      caption = NULL,
                      font_family = "Helvetica",
                      font_size_title = NULL,
                      font_size_body = NULL,
                      title_wrap = 70,
                      subtitle_wrap = 80,
                      caption_wrap = 80,
                      isMobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }

  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(boundary)) {
    if (sf::st_is_longlat(data) == FALSE) boundary <- sf::st_transform(boundary, sf::st_crs(data))
    if (boundary_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }

  if (is.null(pal)) pal <- pal_snz
  
  if (unique(sf::st_geometry_type(data)) %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf(size = size, col = pal[1])
  }
  else if (unique(sf::st_geometry_type(data)) %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(
        size = size,
        col = pal[1],
        fill = pal[1],
        alpha = alpha
      )
  }
  
  if (!is.null(boundary)) {
    if (boundary_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }

  if (isMobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      )
  }
  else if (isMobile == TRUE) {
    plot <- plot +
      theme(plot.title.position = "plot") +
      theme(plot.caption.position = "plot") +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        caption = stringr::str_wrap(caption, 50)
      )
  }
  
  return(plot)
}

#' @title Map of simple features in ggplot that is coloured.
#' @description Map of simple features in ggplot that is coloured, but not facetted. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." NULL results in "category", if categorical or "quantile" if numeric col_var. Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param size Size of features (or shape outlines if polygon). Defaults to 0.5.
#' @param alpha The opacity of polygons. Defaults to 0.9.
#' @param boundary A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. 
#' @param boundary_behind TRUE or FALSE  as to whether the boundary is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param boundary_pal Colour of the boundary. Defaults to "#7F7F7F".
#' @param boundary_size Size of the boundary. Defaults to 0.2.
#' @param legend_ncol The number of columns in the legend.
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param caption Caption title string. Defaults to NULL.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_sf_col(data = example_sf_polygon, col_var = density, boundary = nz,
#'      col_method = "bin", col_cuts = c(0, 10, 50, 100, 150, 200, Inf), legend_digits = 0,
#'      title = "Density, 2017")
#'
#' ggplot_sf_col(data = example_sf_polygon, col_var = density, boundary = nz,
#'      col_method = "quantile", col_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1),
#'      title = "Density, 2017")
#'
#'  pal <- c("#4575B4", "#D3D3D3", "#D73027")
#'
#' ggplot_sf_col(data = example_sf_point, col_var = trend_category, boundary = nz, 
#'    pal = pal, col_method = "category",
#'    title = "Monitored river nitrate-nitrogen trends, 2008-17")
ggplot_sf_col <- function(data,
                          col_var,
                          col_method = NULL,
                          col_cuts = NULL,
                          pal = NULL,
                          pal_rev = FALSE,
                          size = 0.5,
                          alpha = 0.9,
                          boundary = NULL,
                          boundary_behind = TRUE,
                          boundary_pal = "#7f7f7f",
                          boundary_size = 0.2,
                          legend_ncol = 3,
                          legend_digits = 1,
                          title = "[Title]",
                          subtitle = NULL,
                          col_title = "",
                          caption = NULL,
                          legend_labels = NULL,
                          font_family = "Helvetica",
                          font_size_title = NULL,
                          font_size_body = NULL,
                          title_wrap = 70,
                          subtitle_wrap = 80,
                          wrap_col_title = 25,
                          caption_wrap = 80,
                          isMobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  
  col_var <- rlang::enquo(col_var)
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(boundary)) {
    if (sf::st_is_longlat(data) == FALSE) boundary <- sf::st_transform(boundary, sf::st_crs(data))
    if (boundary_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(col_method) & !is.numeric(col_var_vctr)) col_method <- "category"
  if (is.null(col_method) & is.numeric(col_var_vctr)) col_method <- "quantile"
  
  if (col_method == "category") {
    if (is.null(pal)) pal <- pal_point_set1
    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
  }
  else if (col_method == "bin") {
    if (!is.null(col_cuts)) {
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
    }
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
    if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
    if (!is.null(legend_labels)) labels <- legend_labels
  }
  else if (col_method == "quantile") {
    if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
    else {
      if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
      if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
    }  
    col_cuts <- quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
    if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
    if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
    if (!is.null(legend_labels)) labels <- legend_labels
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf(
        aes(col = !!col_var),
        size = size,
        key_glyph = draw_key_rect,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(
        aes(fill = !!col_var),
        size = size,
        col = NA,
        key_glyph = draw_key_rect,
        alpha = alpha,
        data = data
      )
  }
  
  if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      scale_color_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.value = "#A8A8A8"
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.value = "#A8A8A8"
      )
  }
  
  if (!is.null(boundary)) {
    if (boundary_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }
  
  if (isMobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(col = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title))) +
      guides(fill = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title)))
  }
  else if (isMobile == TRUE) {
    plot <- plot +
      theme(plot.title.position = "plot") +
      theme(plot.caption.position = "plot") +
      theme(legend.justification = "left") +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        caption = stringr::str_wrap(caption, 50)
      )  +
      guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15))) +
      guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15)))
  }
  
  return(plot)
}

#' @title Map of simple features in ggplot that is facetted.
#' @description Map of simple features in ggplot that is facetted, but not coloured. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param size Size of features (or shape outlines if polygon). Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 0.1. Only applicable to polygons.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param facet_nrow The number of rows of facetted plots. 
#' @param boundary A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. 
#' @param boundary_behind TRUE or FALSE  as to whether the boundary is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param boundary_pal Colour of the boundary. Defaults to "#7F7F7F".
#' @param boundary_size Size of the boundary. Defaults to 0.2.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_sf_facet(data = example_sf_point, facet_var = trend_category, 
#'   boundary = nz,
#'   title = "Trends, 1990-2017")
ggplot_sf_facet <- function(data,
                            facet_var,
                            size = 0.5,
                            alpha = 0.1,
                            pal = NULL,
                            facet_nrow = NULL,
                            boundary = NULL,
                            boundary_behind = TRUE,
                            boundary_pal = "#7f7f7f",
                            boundary_size = 0.2,
                            title = "[Title]",
                            subtitle = NULL,
                            caption = NULL,
                            font_family = "Helvetica",
                            font_size_title = NULL,
                            font_size_body = NULL,
                            title_wrap = 70,
                            subtitle_wrap = 80,
                            caption_wrap = 80) {
  
  data <- dplyr::ungroup(data)
  
  facet_var <- rlang::enquo(facet_var) #categorical var
  
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if(is.null(font_size_title)) font_size_title <- 11
  if(is.null(font_size_body)) font_size_body <- 10
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(boundary)) {
    if (sf::st_is_longlat(data) == FALSE) boundary <- sf::st_transform(boundary, sf::st_crs(data))
    if (boundary_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(pal)) pal <- pal_snz
  
  if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf(
        col = pal[1],
        size = size,
        key_glyph = draw_key_rect,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(
        fill = pal[1],
        size = size,
        col = NA,
        key_glyph = draw_key_rect,
        alpha = alpha,
        data = data
      )
  }
  
  if (!is.null(boundary)) {
    if (boundary_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(facet_nrow) & length(unique(facet_var_vctr)) <= 3) facet_nrow <- 1
  if (is.null(facet_nrow) & length(unique(facet_var_vctr)) > 3) facet_nrow <- 2
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, 50)
    ) +
    facet_wrap(vars(!!facet_var), scales = "fixed", nrow = facet_nrow)
  
  return(plot)
}

#' @title Map of simple features in ggplot that is coloured and facetted.
#' @description Map of simple features in ggplot that is coloured and facetted. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." NULL results in "category", if categorical or "quantile" if numeric col_var. Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_quantile_by_facet TRUE of FALSE  whether quantiles should be calculated for each group of the facet variable. Defaults to TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param size Size of features (or shape outlines if polygon). Defaults to 0.5.
#' @param alpha The opacity of polygons. Defaults to 0.9.
#' @param boundary A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. 
#' @param boundary_behind TRUE or FALSE  as to whether the boundary is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param boundary_pal Colour of the boundary. Defaults to "#7F7F7F".
#' @param boundary_size Size of the boundary. Defaults to 0.2.
#' @param facet_nrow The number of rows of facetted plots. 
#' @param legend_ncol The number of columns in the legend.
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param caption Caption title string. Defaults to NULL.
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @return A ggplot object.
#' @export
#' @examples
#'  pal <- c("#4575B4", "#D3D3D3", "#D73027")
#'
#' ggplot_sf_col_facet(data = example_sf_point, col_var = trend_category, facet_var = trend_category,
#'  boundary = nz, pal = pal,
#'  title = "Trends, 1990-2017")
ggplot_sf_col_facet <- function(data,
                                col_var,
                                facet_var,
                                col_method = NULL,
                                col_cuts = NULL,
                                col_quantile_by_facet = TRUE,
                                pal = NULL,
                                pal_rev = FALSE,
                                size = 0.5,
                                alpha = 0.9,
                                facet_nrow = NULL,
                                legend_ncol = 3,
                                legend_digits = 1,
                                boundary = NULL,
                                boundary_behind = TRUE,
                                boundary_pal = "#7f7f7f",
                                boundary_size = 0.2, 
                                title = "[Title]",
                                subtitle = NULL,
                                col_title = "",
                                caption = NULL,
                                legend_labels = NULL,
                                font_family = "Helvetica",
                                font_size_title = NULL,
                                font_size_body = NULL,
                                title_wrap = 70,
                                subtitle_wrap = 80,
                                wrap_col_title = 25,
                                caption_wrap = 80) {
  
  data <- dplyr::ungroup(data)
  col_var <- rlang::enquo(col_var)
  facet_var <- rlang::enquo(facet_var) #categorical var
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if(is.null(font_size_title)) font_size_title <- 11
  if(is.null(font_size_body)) font_size_body <- 10
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(boundary)) {
    if (sf::st_is_longlat(data) == FALSE) boundary <- sf::st_transform(boundary, sf::st_crs(data))
    if (boundary_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(col_method) & !is.numeric(col_var_vctr)) col_method <- "category"
  if (is.null(col_method) & is.numeric(col_var_vctr)) col_method <- "quantile"
  
  if (col_method == "category") {
    if (is.null(pal)) pal <- pal_point_set1
    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
  }
  else if (col_method == "bin") {
    if (!is.null(col_cuts)) {
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
    }
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
    if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
    if (!is.null(legend_labels)) labels <- legend_labels
  }
  else if (col_method == "quantile") {
    if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
    else {
      if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
      if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
    }  
    if (col_quantile_by_facet == TRUE) {
      data <- data %>%
        dplyr::group_by(dplyr::across(!!facet_var)) %>%
        dplyr::mutate(dplyr::across(!!col_var, ~percent_rank(.x))) %>%
        dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      
      if (is.null(legend_labels)) labels <- paste0(numeric_legend_labels(col_cuts * 100, 0), "\u1D57\u02B0 percentile")
      if (!is.null(legend_labels)) labels <- legend_labels
    }
    else if (col_quantile_by_facet == FALSE) {
      col_cuts <- quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
      if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      
      data <- data %>% 
        dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, 2)
      if (!is.null(legend_labels)) labels <- legend_labels
    }
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf(
        aes(col = !!col_var),
        size = size,
        key_glyph = draw_key_rect,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(
        aes(fill = !!col_var),
        size = size,
        col = NA,
        key_glyph = draw_key_rect,
        alpha = alpha,
        data = data
      )
  }
  
  if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      scale_color_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.value = "#A8A8A8"
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.value = "#A8A8A8"
      )
  }
  
  if (!is.null(boundary)) {
    if (boundary_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = boundary,
          size = boundary_size, 
          colour = boundary_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(facet_nrow) & length(unique(facet_var_vctr)) <= 3) facet_nrow <- 1
  if (is.null(facet_nrow) & length(unique(facet_var_vctr)) > 3) facet_nrow <- 2
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    guides(col = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title))) +
    guides(fill = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title))) +
    facet_wrap(vars(!!facet_var), scales = "fixed", nrow = facet_nrow)

  return(plot)
}
