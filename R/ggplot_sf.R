#' @title Map of simple features in ggplot.
#' @description Map of simple features in ggplot that is not coloured and not facetted. 
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param size_point Size of points. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_behind TRUE or FALSE  as to whether the borders is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#7F7F7F".
#' @param borders_size Size of the borders. Defaults to 0.2.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_sf(data = example_sf_point, borders = nz)
ggplot_sf <- function(data,
                      text_var = NULL,
                      size_point = 1,
                      size_line = 0.5,
                      alpha = 1,
                      pal = NULL,
                      borders = NULL,
                      borders_behind = TRUE,
                      borders_pal = "#7f7f7f",
                      borders_size = 0.2,
                      title = "[Title]",
                      title_wrap = 70,
                      subtitle = NULL,
                      subtitle_wrap = 80,
                      caption = NULL,
                      caption_wrap = 80,
                      font_family = "Helvetica",
                      font_size_title = NULL,
                      font_size_body = NULL,
                      mobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  text_var <- rlang::enquo(text_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(borders)) {
    if (sf::st_is_longlat(data) == FALSE) borders <- sf::st_transform(borders, sf::st_crs(data))
    if (borders_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }

  if (is.null(pal)) pal <- sv_pal(1)
  else pal <- pal[1]
  
  if (unique(sf::st_geometry_type(data)) %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf(aes(text = !!text_var), size = size_point, col = pal)
  }
  else if (unique(sf::st_geometry_type(data)) %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf(aes(text = !!text_var), size = size_line, col = pal)
  }
  else if (unique(sf::st_geometry_type(data)) %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(aes(text = !!text_var), 
        size = size_line,
        col = pal,
        fill = pal,
        alpha = alpha
      )
  }
  
  if (!is.null(borders)) {
    if (borders_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }

  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      )
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        caption = stringr::str_wrap(caption, 50)
      ) + 
      theme_mobile_map()
  }
  
  return(plot)
}

#' @title Map of simple features in ggplot that is coloured.
#' @description Map of simple features in ggplot that is coloured, but not facetted. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param size_point Size of points. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param alpha The opacity of polygons. Defaults to 0.9.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_behind TRUE or FALSE  as to whether the borders is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#7F7F7F".
#' @param borders_size Size of the borders. Defaults to 0.2.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels_dp Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." NULL results in "category", if categorical or "quantile" if numeric col_var. Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_sf_col(data = example_sf_polygon, col_var = density, borders = nz,
#'      col_method = "bin", col_cuts = c(0, 10, 50, 100, 150, 200, Inf), col_labels_dp = 0,
#'      title = "Density, 2017")
#'
#' ggplot_sf_col(data = example_sf_polygon, col_var = density, borders = nz,
#'      col_method = "quantile", col_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1),
#'      title = "Density, 2017")
#'
#'  pal <- c("#4575B4", "#D3D3D3", "#D73027")
#'
#' ggplot_sf_col(data = example_sf_point, col_var = trend_category, borders = nz, 
#'    pal = pal, col_method = "category",
#'    title = "Monitored trends, 2008-17")
ggplot_sf_col <- function(data,
                          col_var,
                          text_var = NULL,
                          pal = NULL,
                          pal_rev = FALSE,
                          size_point = 1,
                          size_line = 0.5,
                          alpha = 1,
                          borders = NULL,
                          borders_behind = TRUE,
                          borders_pal = "#7f7f7f",
                          borders_size = 0.2,
                          title = "[Title]",
                          title_wrap = 70,
                          subtitle = NULL,
                          subtitle_wrap = 80,
                          col_cuts = NULL,
                          col_labels = NULL,
                          col_labels_dp = 1,
                          col_legend_ncol = NULL,
                          col_legend_nrow = NULL,
                          col_method = NULL,
                          col_na = TRUE,
                          col_title = "",
                          col_title_wrap = 25,
                          caption = NULL,
                          caption_wrap = 80,
                          font_family = "Helvetica",
                          font_size_title = NULL,
                          font_size_body = NULL,
                          mobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  col_var <- rlang::enquo(col_var)
  text_var <- rlang::enquo(text_var)
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(borders)) {
    if (sf::st_is_longlat(data) == FALSE) borders <- sf::st_transform(borders, sf::st_crs(data))
    if (borders_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(col_method) & !is.numeric(col_var_vctr)) col_method <- "category"
  if (is.null(col_method) & is.numeric(col_var_vctr)) col_method <- "quantile"
  
  if (col_method == "quantile") {
    if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
    else {
      if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
      if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
    }  
    col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
    if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    n_col <- length(col_cuts) - 1
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (is.null(col_labels)) labels <-  sv_labels_from_cuts(col_cuts, col_labels_dp)
    else labels <- col_labels
  }
  else if (col_method == "bin") {
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
    else({
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
    })
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    n_col <- length(col_cuts) - 1
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (is.null(col_labels)) labels <-  sv_labels_from_cuts(col_cuts, col_labels_dp)
    else labels <- col_labels
  }
  else if (col_method == "category") {
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (is.null(col_labels)) labels <- waiver()
    else labels <- col_labels
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_point,
        # key_glyph = draw_key_polygon,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_line,
        # key_glyph = draw_key_polygon,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, fill = !!col_var, text = !!text_var),
        size = size_line,
        # key_glyph = draw_key_polygon,
        alpha = alpha,
        data = data
      )
  }
  
  plot <- plot +
    scale_color_manual(
      values = pal,
      drop = FALSE,
      labels = labels,
      na.translate = col_na, 
      na.value = "#A8A8A8"
    )
  
    if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na, 
        na.value = "#A8A8A8"
      )
  }
  
  if (!is.null(borders)) {
    if (borders_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }
  
  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(col = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) +
      guides(fill = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap)))
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        caption = stringr::str_wrap(caption, 50)
      )  +
      guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15))) +
      guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15))) +
      theme_mobile_map()
  }
  
  return(plot)
}

#' @title Map of simple features in ggplot that is facetted.
#' @description Map of simple features in ggplot that is facetted, but not coloured. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param size_point Size of points. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_behind TRUE or FALSE  as to whether the borders is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#7F7F7F".
#' @param borders_size Size of the borders. Defaults to 0.2.
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
#'   borders = nz,
#'   title = "Trends, 1990-2017")
ggplot_sf_facet <- function(data,
                            facet_var,
                            text_var = NULL,
                            size_point = 1,
                            size_line = 0.5,
                            alpha = 1,
                            pal = NULL,
                            facet_ncol = NULL,
                            facet_nrow = NULL,
                            borders = NULL,
                            borders_behind = TRUE,
                            borders_pal = "#7f7f7f",
                            borders_size = 0.2,
                            title = "[Title]",
                            title_wrap = 70,
                            subtitle = NULL,
                            subtitle_wrap = 80,
                            caption = NULL,
                            caption_wrap = 80,
                            font_family = "Helvetica",
                            font_size_title = NULL,
                            font_size_body = NULL) {
  
  data <- dplyr::ungroup(data)
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(borders)) {
    if (sf::st_is_longlat(data) == FALSE) borders <- sf::st_transform(borders, sf::st_crs(data))
    if (borders_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(pal)) pal <- sv_pal(1)
  else pal <- pal[1]

  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf(
        aes(text = !!text_var), 
        col = pal,
        size = size_point,
        # key_glyph = draw_key_polygon,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf(
        aes(text = !!text_var), 
        col = pal,
        size = size_line,
        # key_glyph = draw_key_polygon,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(
        aes(text = !!text_var), 
        col = pal,
        fill = pal,
        size = size_line,
        # key_glyph = draw_key_polygon,
        alpha = alpha,
        data = data
      )
  }
  
  if (!is.null(borders)) {
    if (borders_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, 50)
    ) +
    facet_wrap(vars(!!facet_var), scales = "fixed", ncol = facet_ncol, nrow = facet_nrow)
  
  return(plot)
}

#' @title Map of simple features in ggplot that is coloured and facetted.
#' @description Map of simple features in ggplot that is coloured and facetted. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param size_point Size of points. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param alpha The opacity of polygons. Defaults to 0.9.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_behind TRUE or FALSE  as to whether the borders is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#7F7F7F".
#' @param borders_size Size of the borders. Defaults to 0.2.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels_dp Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." NULL results in "category", if categorical or "quantile" if numeric col_var. Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#'  pal <- c("#4575B4", "#D3D3D3", "#D73027")
#'
#' ggplot_sf_col_facet(data = example_sf_point, col_var = trend_category, facet_var = trend_category,
#'  borders = nz, pal = pal,
#'  title = "Trends, 1990-2017")
ggplot_sf_col_facet <- function(data,
                                col_var,
                                facet_var,
                                text_var = NULL,
                                pal = NULL,
                                pal_rev = FALSE,
                                size_point = 1,
                                size_line = 0.5,
                                alpha = 1,
                                borders = NULL,
                                borders_behind = TRUE,
                                borders_pal = "#7f7f7f",
                                borders_size = 0.2, 
                                title = "[Title]",
                                title_wrap = 70,
                                subtitle = NULL,
                                subtitle_wrap = 80,
                                col_cuts = NULL,
                                col_labels_dp = 1,
                                col_labels = NULL,
                                col_method = NULL,
                                col_na = TRUE,
                                col_legend_ncol = NULL,
                                col_legend_nrow = NULL,
                                col_title = "",
                                col_title_wrap = 25,
                                facet_ncol = NULL,
                                facet_nrow = NULL,
                                caption = NULL,
                                caption_wrap = 80,
                                font_family = "Helvetica",
                                font_size_title = NULL,
                                font_size_body = NULL) {
  
  data <- dplyr::ungroup(data)
  col_var <- rlang::enquo(col_var)
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_sf(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (!is.null(borders)) {
    if (sf::st_is_longlat(data) == FALSE) borders <- sf::st_transform(borders, sf::st_crs(data))
    if (borders_behind == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }
  
  if (is.null(col_method) & !is.numeric(col_var_vctr)) col_method <- "category"
  if (is.null(col_method) & is.numeric(col_var_vctr)) col_method <- "quantile"
  
  if (col_method == "category") {
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (is.null(col_labels)) labels <- waiver()
    else labels <- col_labels
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
    if (is.null(col_labels)) labels <-  sv_labels_from_cuts(col_cuts, col_labels_dp)
    if (!is.null(col_labels)) labels <- col_labels
  }
  else if (col_method == "quantile") {
    if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
    else {
      if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
      if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
    }  
    data <- data %>%
      dplyr::group_by(dplyr::across(!!facet_var)) %>%
      dplyr::mutate(dplyr::across(!!col_var, ~percent_rank(.x))) %>%
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
    
    if (is.null(col_labels)) labels <- paste0( sv_labels_from_cuts(col_cuts * 100, 0), "\u1D57\u02B0 percentile")
    if (!is.null(col_labels)) labels <- col_labels
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf(
        aes(col = !!col_var, text = !!text_var),
        size = size_point,
        # key_glyph = draw_key_polygon,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_line,
        # key_glyph = draw_key_polygon,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(
        aes(col = !!col_var, fill = !!col_var, text = !!text_var),
        size = size_line,
        # key_glyph = draw_key_polygon,
        alpha = alpha,
        data = data
      )
  }
  
  plot <- plot +
    scale_color_manual(
      values = pal,
      drop = FALSE,
      labels = labels,
      na.translate = col_na, 
      na.value = "#A8A8A8"
    )
  
  if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na, 
        na.value = "#A8A8A8"
      )
  }
  
  if (!is.null(borders)) {
    if (borders_behind == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = borders_size, 
          colour = borders_pal,
          fill = "transparent"
        )
    }
  }
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    guides(col = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) +
    guides(fill = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) +
    facet_wrap(vars(!!facet_var), scales = "fixed", ncol = facet_ncol, nrow = facet_nrow) 

  return(plot)
}
