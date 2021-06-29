#' @title Simple feature ggplot map.
#' @description Map of simple features in ggplot that is not coloured and not facetted. 
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param size_point Size of points. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 0.9. 
#' @param pal Character vector of hex codes. 
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_behind TRUE or FALSE  as to whether the borders is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#7F7F7F".
#' @param borders_size Size of the borders. Defaults to 0.2.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf(example_sf_point, 
#'      borders = nz)
#'      
gg_sf <- function(data,
                  text_var = NULL,
                  size_point = 1,
                  size_line = 0.5,
                  alpha = 0.9,
                  pal = NULL,
                  borders = NULL,
                  borders_behind = TRUE,
                  borders_pal = "#7f7f7f",
                  borders_size = 0.2,
                  title = NULL,
                  title_wrap = 100,
                  subtitle = NULL,
                  subtitle_wrap = 100,
                  caption = NULL,
                  caption_wrap = 80,
                  font_family = "",
                  font_size_title = NULL,
                  font_size_body = NULL,
                  mobile = FALSE
) {
  
  data <- dplyr::ungroup(data)
  text_var <- rlang::enquo(text_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  plot <- ggplot(data) +
    theme_map(
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

  if (is.null(pal)) pal <- pal_viridis_reorder(1)
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
      theme_mobile_extra_map()
  }
  
  return(plot)
}

#' @title Simple feature ggplot map that is coloured.
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
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels A function or vector to modify colour scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case, and numeric variable labels to pretty labels with an internal function. Use ggplot2::waiver() to keep colour labels untransformed.   
#' @param col_labels_dp For numeric colour variables and where col_labels equals NULL, the number of decimal places. Defaults to 1 for "quantile" col_method, and the lowest dp within the col_cuts vector for "bin".
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." NULL results in "category", if categorical or "quantile" if numeric col_var. Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_na TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf_col(example_sf_point, 
#'           col_var = trend_category, 
#'           borders = nz)
#'    
#' gg_sf_col(example_sf_polygon, 
#'           col_var = density, 
#'           borders = nz, 
#'           col_method = "bin", 
#'           col_cuts = c(0, 10, 50, 100, 150, 200, Inf))
#'
#' gg_sf_col(example_sf_polygon, 
#'           col_var = density, 
#'           borders = nz, 
#'           col_method = "quantile", 
#'           col_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1))
#'           
gg_sf_col <- function(data,
                      col_var,
                      text_var = NULL,
                      pal = NULL,
                      pal_rev = FALSE,
                      size_point = 1,
                      size_line = 0.5,
                      alpha = 0.9,
                      borders = NULL,
                      borders_behind = TRUE,
                      borders_pal = "#7f7f7f",
                      borders_size = 0.2,
                      title = NULL,
                      title_wrap = 100,
                      subtitle = NULL,
                      subtitle_wrap = 100,
                      col_cuts = NULL,
                      col_labels = NULL,
                      col_labels_dp = NULL,
                      col_legend_ncol = NULL,
                      col_legend_nrow = NULL,
                      col_na = TRUE,
                      col_method = NULL,
                      col_title = NULL,
                      col_title_wrap = 25,
                      caption = NULL,
                      caption_wrap = 80,
                      font_family = "",
                      font_size_title = NULL,
                      font_size_body = NULL,
                      mobile = FALSE
) {
  
  data <- dplyr::ungroup(data)
  col_var <- rlang::enquo(col_var)
  text_var <- rlang::enquo(text_var)
  
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }

  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_map(
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
  
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "quantile"
  }
  
  if(col_method %in% c("quantile", "bin")) {
    if (col_method == "quantile") {
      if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
      else {
        if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
        if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
      }  
      col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
      if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      if(is.null(col_labels_dp)) col_labels_dp <- 1
    }
    else if (col_method == "bin") {
      if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
      else({
        if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
        if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      })
      if(is.null(col_labels_dp)) col_labels_dp <- sv_max_dp(col_cuts)
    }
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if(is.null(col_labels)) col_labels <- sv_numeric_bin_labels(col_cuts, col_labels_dp)
    
    col_n <- length(col_cuts) - 1
    if (is.null(pal)) pal <- pal_viridis_reorder(col_n)
    else pal <- pal[1:col_n]
  }
  else if (col_method == "category") {
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      col_n <- length(levels(col_var_vctr))
    }
    else col_n <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- pal_d3_reorder(col_n)
    else pal <- pal[1:col_n]
    
    if(is.null(col_labels)) col_labels <- function(x) stringr::str_to_sentence(x)
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)

  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_point,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_line,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, fill = !!col_var, text = !!text_var),
        size = size_line,
        alpha = alpha,
        data = data
      )
  }
  
  plot <- plot +
    scale_color_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na()
    )
  
    if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na()
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
      guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 20))) +
      guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 20))) +
      theme_mobile_extra_map()
  }
  
  return(plot)
}

#' @title Simple feature ggplot map that is facetted.
#' @description Map of simple features in ggplot that is facetted, but not coloured. 
#' @param data A sf object with defined coordinate reference system. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param size_point Size of points. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 0.9. 
#' @param pal Character vector of hex codes. 
#' @param facet_labels As per the ggplot2 labeller argument within the ggplot facet_wrap function. If NULL, defaults to ggplot2::as_labeller(stringr::str_to_sentence). Use facet_labels = ggplot2::label_value to turn off default sentence case transformation.
#' @param facet_na TRUE or FALSE of whether to include facet_var NA values. Defaults to TRUE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_behind TRUE or FALSE  as to whether the borders is to be behind the sf object defined in the data argument. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#7F7F7F".
#' @param borders_size Size of the borders. Defaults to 0.2.
#' @param title Title string. Defaults to NULL.
#' @param subtitle Subtitle string. 
#' @param caption Caption title string. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf_facet(example_sf_point, 
#'             facet_var = trend_category, 
#'             borders = nz)
#' 
gg_sf_facet <- function(data,
                        facet_var,
                        text_var = NULL,
                        size_point = 1,
                        size_line = 0.5,
                        alpha = 0.9,
                        pal = NULL,
                        facet_labels = NULL,
                        facet_na = TRUE,
                        facet_ncol = NULL,
                        facet_nrow = NULL,
                        borders = NULL,
                        borders_behind = TRUE,
                        borders_pal = "#7f7f7f",
                        borders_size = 0.2,
                        title = NULL,
                        title_wrap = 100,
                        subtitle = NULL,
                        subtitle_wrap = 100,
                        caption = NULL,
                        caption_wrap = 80,
                        font_family = "",
                        font_size_title = NULL,
                        font_size_body = NULL
                        
) {
  
  data <- dplyr::ungroup(data)
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  if (facet_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if(is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_map(
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
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]

  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf(
        aes(text = !!text_var), 
        col = pal,
        size = size_point,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf(
        aes(text = !!text_var), 
        col = pal,
        size = size_line,
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
  
  if(is.null(facet_labels)) facet_labels <- as_labeller(stringr::str_to_sentence)
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, 50)
    ) +
    facet_wrap(vars(!!facet_var), labeller = facet_labels, scales = "fixed", ncol = facet_ncol, nrow = facet_nrow)

  return(plot)
}

#' @title Simple feature ggplot map that is coloured and facetted.
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
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param facet_labels As per the ggplot2 labeller argument within the ggplot facet_wrap function. If NULL, defaults to ggplot2::as_labeller(stringr::str_to_sentence). Use facet_labels = ggplot2::label_value to turn off default sentence case transformation.
#' @param col_labels A function or vector to modify colour scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case, and numeric variable labels to pretty labels with an internal function. Use ggplot2::waiver() to keep colour labels untransformed.   
#' @param col_labels_dp For numeric colour variables and where col_labels equals NULL, the number of decimal places. Defaults to 1 for "quantile" col_method, and the lowest dp within the col_cuts vector for "bin".
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." NULL results in "category", if categorical or "quantile" if numeric col_var. Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_na TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_na TRUE or FALSE of whether to include facet_var NA values. Defaults to TRUE.
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_ncol The number of columns of facetted plots.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf_col_facet(example_sf_point, 
#'                 col_var = trend_category, 
#'                 facet_var = trend_category, 
#'                 borders = nz)
#'  
gg_sf_col_facet <- function(data,
                            col_var,
                            facet_var,
                            text_var = NULL,
                            pal = NULL,
                            pal_rev = FALSE,
                            size_point = 1,
                            size_line = 0.5,
                            alpha = 0.9,
                            borders = NULL,
                            borders_behind = TRUE,
                            borders_pal = "#7f7f7f",
                            borders_size = 0.2,
                            title = NULL,
                            title_wrap = 100,
                            subtitle = NULL,
                            subtitle_wrap = 100,
                            col_cuts = NULL,
                            col_labels = NULL,
                            col_labels_dp = NULL,
                            col_method = NULL,
                            col_legend_ncol = NULL,
                            col_legend_nrow = NULL,
                            col_na = TRUE,
                            col_title = NULL,
                            col_title_wrap = 25,
                            facet_labels = NULL,
                            facet_na = TRUE,
                            facet_ncol = NULL,
                            facet_nrow = NULL,
                            caption = NULL,
                            caption_wrap = 80,
                            font_family = "",
                            font_size_title = NULL,
                            font_size_body = NULL)
{
  
  data <- dplyr::ungroup(data)
  col_var <- rlang::enquo(col_var)
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  if (facet_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  if(is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  plot <- ggplot(data) +
    theme_map(
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
  
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "quantile"
  }
  
  if(col_method %in% c("quantile", "bin")) {
    if (col_method == "quantile") {
      if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
      else {
        if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
        if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
      }  
      col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
      if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      if(is.null(col_labels_dp)) col_labels_dp <- 1
    }
    else if (col_method == "bin") {
      if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
      else({
        if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
        if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      })
      if(is.null(col_labels_dp)) col_labels_dp <- sv_max_dp(col_cuts)
    }
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if(is.null(col_labels)) col_labels <- sv_numeric_bin_labels(col_cuts, col_labels_dp)
    
    col_n <- length(col_cuts) - 1
    if (is.null(pal)) pal <- pal_viridis_reorder(col_n)
    else pal <- pal[1:col_n]
  }
  else if (col_method == "category") {
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      col_n <- length(levels(col_var_vctr))
    }
    else col_n <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- pal_d3_reorder(col_n)
    else pal <- pal[1:col_n]
    
    if(is.null(col_labels)) col_labels <- function(x) stringr::str_to_sentence(x)
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    plot <- plot +
      geom_sf(
        aes(col = !!col_var, text = !!text_var),
        size = size_point,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_line,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      geom_sf(
        aes(col = !!col_var, fill = !!col_var, text = !!text_var),
        size = size_line,
        alpha = alpha,
        data = data
      )
  }
  
  plot <- plot +
    scale_color_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na()
    )
  
  if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na()
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
  
  if(is.null(facet_labels)) facet_labels <- as_labeller(stringr::str_to_sentence)
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    guides(col = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) +
    guides(fill = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) +
    facet_wrap(vars(!!facet_var), labeller = facet_labels, scales = "fixed", ncol = facet_ncol, nrow = facet_nrow)
  
  
  return(plot)
}
