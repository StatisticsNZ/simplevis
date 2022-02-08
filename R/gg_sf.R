#' @title Simple feature ggplot map.
#' @description Map of simple features in ggplot that is not coloured and not facetted. 
#' @param data A sf object with defined coordinate reference system in a structure to be plotted untransformed. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_on_top TRUE or FALSE  as to whether the borders are on top of the sf object supplied to the data argument. Defaults to TRUE for points and lines, but FALSE for polygons..
#' @param pal Character vector of hex codes. 
#' @param pal_borders Colour of the borders. Defaults to "#7F7F7F".
#' @param alpha_fill The opacity of the fill.
#' @param alpha_line The alpha of lines and outlines. 
#' @param alpha_point The alpha of points. 
#' @param alpha_borders Opacity of the borders. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param size_point Size of points. Defaults to 1.5.
#' @param size_borders Size of the borders. Defaults to 0.2.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf(example_point, 
#'      borders = example_borders)
#'      
gg_sf <- function(data,
                  text_var = NULL,
                  borders = NULL,
                  borders_on_top = NULL,
                  pal = pal_viridis_reorder(1),
                  pal_borders = "#7F7F7F",
                  alpha_fill = NULL,
                  alpha_line = 1,
                  alpha_point = 1,
                  alpha_borders = 0.5,
                  size_line = 0.5,
                  size_point = 1.5,
                  size_borders = 0.2,
                  title = NULL,
                  title_wrap = 80,
                  subtitle = NULL,
                  subtitle_wrap = 80,
                  caption = NULL,
                  caption_wrap = 80,
                  theme = gg_theme(void = TRUE),
                  mobile = FALSE
) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  text_var <- rlang::enquo(text_var)
  
  #warnings
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system to data input")
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  if (!geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")) {
    stop("Please use an sf object with geometry type of POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON")
  }
  
  if (!is.null(borders)) {
    if (class(borders)[1] != "sf") stop("Please use an sf object as borders input")
    if (is.na(sf::st_crs(borders)$proj4string)) stop("Please assign a coordinate reference system to borders object")
  }
  
  #fundamentals
  plot <- ggplot(data) +
    theme
  
  #borders
  if (!is.null(borders)) {
    pal_borders <- scales::alpha(pal_borders, alpha = alpha_borders)
    
    if (is.null(borders_on_top)) {
      if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
        borders_on_top <- FALSE
      } else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
        borders_on_top <- TRUE
      }
    }
    
    if (sf::st_crs(data) != sf::st_crs(borders)) {
      borders <- sf::st_transform(borders, sf::st_crs(data))
    }

    if (borders_on_top == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }

  #colour
  pal <- pal[1]
  
  #fundamentals
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    pal_point <- scales::alpha(pal, alpha = alpha_point)

    plot <- plot +
      geom_sf(aes(text = !!text_var), 
              size = size_point, 
              col = pal_point)
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    pal_line <- scales::alpha(pal, alpha = alpha_line)

    plot <- plot +
      geom_sf(aes(text = !!text_var), 
              size = size_line, 
              col = pal_line)
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha_fill)) alpha_fill <- 1
    pal_fill <- scales::alpha(pal, alpha = alpha_fill)
    pal_line <- scales::alpha(pal, alpha = alpha_line)

    plot <- plot +
      geom_sf(aes(text = !!text_var), 
        size = size_line,
        col = pal_line,
        fill = pal_fill
      )
  }
  
  #borders
  if (!is.null(borders)) {
    if (borders_on_top == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }
  
  #titles
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
      theme_mobile_extra(void = TRUE)
  }
  
  return(plot)
}

#' @title Simple feature ggplot map that is coloured.
#' @description Map of simple features in ggplot that is coloured, but not facetted. 
#' @param data A sf object with defined coordinate reference system in a structure to be plotted untransformed. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_on_top TRUE or FALSE  as to whether the borders are on top of the sf object supplied to the data argument. Defaults to TRUE for points and lines, but FALSE for polygons..
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param pal_borders Colour of the borders. Defaults to "#7F7F7F".
#' @param alpha_fill The opacity of the fill.
#' @param alpha_line The alpha of lines and outlines. 
#' @param alpha_point The alpha of points. 
#' @param alpha_borders Opacity of the borders. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param size_point Size of points. Defaults to 1.5.
#' @param size_borders Size of the borders. Defaults to 0.2.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param col_breaks_n For a numeric colour variable, the desired number of intervals on the colour scale. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case for categorical colour variables and scales::comma for numeric colour variables. Use ggplot2::waiver() to keep colour labels untransformed.   
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_method The method of colouring features, either "bin", "quantile", "continuous", or "category." If numeric, defaults to "bin".
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf_col(example_point,
#'           col_var = trend_category, 
#'           borders = example_borders)
#' 
#' gg_sf_col(example_polygon,
#'           col_var = density, 
#'           borders = example_borders)
#' 
#' gg_sf_col(example_polygon,
#'           col_var = density,
#'           col_method = "bin",
#'           col_breaks_n = 5, 
#'           borders = example_borders)
#' 
#' gg_sf_col(example_polygon,
#'           col_var = density,
#'           col_method = "bin",
#'           col_cuts = c(0, 10, 50, 100, 150, 200, Inf),
#'           borders = example_borders)
#'           
#' gg_sf_col(example_polygon,
#'           col_var = density,
#'           col_method = "quantile",
#'           col_breaks_n = 4, 
#'           borders = example_borders)
#' 
#' gg_sf_col(example_polygon,
#'           col_var = density,
#'           col_method = "quantile",
#'           col_cuts = c(0, 0.25, 0.5, 0.75, 0.95, 1), 
#'           borders = example_borders)
#' 
gg_sf_col <- function(data,
                      col_var,
                      text_var = NULL,
                      borders = NULL,
                      borders_on_top = NULL,
                      pal = NULL,
                      pal_na = "#7F7F7F",
                      pal_rev = FALSE,
                      pal_borders = "#7F7F7F",
                      alpha_fill = NULL,
                      alpha_line = 1,
                      alpha_point = 1,
                      alpha_borders = 0.5,
                      size_line = 0.5,
                      size_point = 1.5,
                      size_borders = 0.2,
                      title = NULL,
                      title_wrap = 80,
                      subtitle = NULL,
                      subtitle_wrap = 80,
                      col_breaks_n = 4,
                      col_cuts = NULL,
                      col_intervals_right = TRUE,
                      col_labels = NULL,
                      col_legend_none = FALSE,
                      col_na_rm = FALSE,
                      col_method = NULL,
                      col_title = NULL,
                      col_title_wrap = 25,
                      caption = NULL,
                      caption_wrap = 80,
                      theme = gg_theme(void = TRUE),
                      mobile = FALSE
) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  col_var <- rlang::enquo(col_var)
  text_var <- rlang::enquo(text_var)
  
  #na's
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  #warnings
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system to data input")
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  if (!geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")) {
    stop("Please use an sf object with geometry type of POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON")
  }
  
  if (!is.null(borders)) {
    if (class(borders)[1] != "sf") stop("Please use an sf object as borders input")
    if (is.na(sf::st_crs(borders)$proj4string)) stop("Please assign a coordinate reference system to borders object")
  }
  
  if (!is.null(col_method)) {
    if (!col_method %in% c("continuous", "bin", "quantile", "category")) stop("Please use a colour method of 'continuous', 'bin', 'quantile' or 'category'")
  }

  #logical to factor
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  #titles
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #fundamentals
  plot <- ggplot(data) +
    theme
  
  #borders
  if (!is.null(borders)) {
    pal_borders <- scales::alpha(pal_borders, alpha = alpha_borders)
    
    if (is.null(borders_on_top)) {
      if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
        borders_on_top <- FALSE
      } else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
        borders_on_top <- TRUE
      }
    }
    
    if (sf::st_crs(data) != sf::st_crs(borders)) {
      borders <- sf::st_transform(borders, sf::st_crs(data))
    }
    
    if (borders_on_top == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }
  
  #colour
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "continuous"
  }
  
  if (col_method == "continuous") {
    if (is.null(pal)) pal <- viridis::viridis(20)
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
    if (is.null(col_labels)) col_labels <- scales::label_comma()
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    if (col_method %in% c("quantile", "bin")) {
      if (col_method == "quantile") {
        if (is.null(col_cuts)) col_cuts <- seq(0, 1, 1 / col_breaks_n)
        else {
          if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
          if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
        }  
        col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
        if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      }
      else if (col_method == "bin") {
        if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
        else {
          if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
          if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
        }
      }
      
      if (is.null(col_labels)) col_labels <- scales::label_comma()
      
      if (is.function(col_labels)) {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE, format_fun = col_labels)))
        
        col_labels <- sv_interval_labels_chr
      }
      else {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE)))
      }
      
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
      
      if (is.null(col_labels)) col_labels <- snakecase::to_sentence_case
    }
  }  
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  #fundamentals
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    pal_point <- scales::alpha(pal, alpha = alpha_point)
    pal_na_point <- scales::alpha(pal_na, alpha = alpha_point)

    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_point,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    pal_line <- scales::alpha(pal, alpha = alpha_line)
    pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
    
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_line,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha_fill)) alpha_fill <- 1
    pal_fill <- scales::alpha(pal, alpha = alpha_fill)
    pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)
    pal_line <- scales::alpha(pal, alpha = alpha_line)
    pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
    
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, fill = !!col_var, text = !!text_var),
        size = size_line,
        data = data
      )
  }
  
  #colour
  if (mobile == TRUE) col_title_wrap <- 20
  
  if (col_method == "continuous") {
    if (geometry_type %in% c("POINT", "MULTIPOINT")) {
      plot <- plot +
        scale_colour_gradientn(
          colors = pal_point,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_point,
          name = stringr::str_wrap(col_title, col_title_wrap)) 
    }
    else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
      plot <- plot +
        scale_colour_gradientn(
          colors = pal_line,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)) 
    }
    else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
      plot <- plot +
        scale_colour_gradientn(
          colors = pal_line,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)) +
        scale_fill_gradientn(
          colors = pal_fill,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_fill,
          name = stringr::str_wrap(col_title, col_title_wrap)) +
        guides(colour = "none")
    }
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    if (geometry_type %in% c("POINT", "MULTIPOINT")) {
      plot <- plot +
        scale_colour_manual(
          values = pal_point,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_point,
          name = stringr::str_wrap(col_title, col_title_wrap)
        )       
    }
    else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
      plot <- plot +
        scale_colour_manual(
          values = pal_line,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)
        )       
    }
    else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
      plot <- plot +
        scale_colour_manual(
          values = pal_line,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)) +      
        scale_fill_manual(
          values = pal_fill,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_fill,
          name = stringr::str_wrap(col_title, col_title_wrap))
    }

    if (mobile == TRUE) {
      plot <- plot +
        guides(col = guide_legend(ncol = 1),
               fill = guide_legend(ncol = 1))
    }
  }
  
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")
  
  #borders
  if (!is.null(borders)) {
    if (borders_on_top == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }
  
  #titles
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
      )  +
      theme_mobile_extra(void = TRUE)
  }
  
  return(plot)
}

#' @title Simple feature ggplot map that is facetted.
#' @description Map of simple features in ggplot that is facetted, but not coloured. 
#' @param data A sf object with defined coordinate reference system in a structure to be plotted untransformed. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_on_top TRUE or FALSE  as to whether the borders are on top of the sf object supplied to the data argument. Defaults to TRUE for points and lines, but FALSE for polygons..
#' @param alpha_fill The opacity of features. 
#' @param pal Character vector of hex codes. 
#' @param alpha_fill The opacity of the fill.
#' @param alpha_line The alpha of lines and outlines. 
#' @param alpha_point The alpha of points. 
#' @param size_line Size of lines. Defaults to 0.5.
#' @param size_point Size of points. Defaults to 1.5.
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param pal_borders Colour of the borders. Defaults to "#7F7F7F".
#' @param alpha_borders Opacity of the borders. Defaults to 0.5.
#' @param size_borders Size of the borders. Defaults to 0.2.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80.
#' @param theme A ggplot2 theme. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf_facet(example_point, 
#'             facet_var = trend_category, 
#'             borders = example_borders)
#' 
gg_sf_facet <- function(data,
                        facet_var,
                        text_var = NULL,
                        pal = pal_viridis_reorder(1),
                        pal_borders = "#7F7F7F",
                        borders = NULL,
                        borders_on_top = NULL,
                        alpha_fill = NULL,
                        alpha_line = 1,
                        alpha_point = 1,
                        alpha_borders = 0.5,
                        size_line = 0.5,
                        size_point = 1.5,
                        size_borders = 0.2,
                        facet_labels = snakecase::to_sentence_case,
                        facet_na_rm = FALSE,
                        facet_ncol = NULL,
                        facet_nrow = NULL,
                        facet_rev = FALSE,
                        title = NULL,
                        title_wrap = 80,
                        subtitle = NULL,
                        subtitle_wrap = 80,
                        caption = NULL,
                        caption_wrap = 80,
                        theme = gg_theme(void = TRUE)) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  #na's
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #vectors
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  #warnings
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system to data input")
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  if (!geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")) {
    stop("Please use an sf object with geometry type of POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON")
  }
  
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if (!is.null(borders)) {
    if (class(borders)[1] != "sf") stop("Please use an sf object as borders input")
    if (is.na(sf::st_crs(borders)$proj4string)) stop("Please assign a coordinate reference system to borders object")
  }
  
  #logical to factor
  if (is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #reverse
  if (facet_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!facet_var, ~forcats::fct_rev(.x)))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #fundamentals
  plot <- ggplot(data) +
    theme
  
  #borders
  if (!is.null(borders)) {
    pal_borders <- scales::alpha(pal_borders, alpha = alpha_borders)
    
    if (is.null(borders_on_top)) {
      if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
        borders_on_top <- FALSE
      } else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
        borders_on_top <- TRUE
      }
    }
    
    if (sf::st_crs(data) != sf::st_crs(borders)) {
      borders <- sf::st_transform(borders, sf::st_crs(data))
    }
    
    if (borders_on_top == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }
  
  #colour
  pal <- pal[1]
  
  #fundamentals
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    pal_point <- scales::alpha(pal, alpha = alpha_point)
    
    plot <- plot +
      geom_sf(aes(text = !!text_var), 
              size = size_point, 
              col = pal_point)
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    pal_line <- scales::alpha(pal, alpha = alpha_line)
    
    plot <- plot +
      geom_sf(aes(text = !!text_var), 
              size = size_line, 
              col = pal_line)
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha_fill)) alpha_fill <- 1
    pal_fill <- scales::alpha(pal, alpha = alpha_fill)
    pal_line <- scales::alpha(pal, alpha = alpha_line)
    
    plot <- plot +
      geom_sf(aes(text = !!text_var), 
              size = size_line,
              col = pal_line,
              fill = pal_fill)
  }
  
  #borders
  if (!is.null(borders)) {
    if (borders_on_top == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }
  
  #titles & facetting
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, 50)
    ) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = "fixed", ncol = facet_ncol, nrow = facet_nrow)

  return(plot)
}

#' @title Simple feature ggplot map that is coloured and facetted.
#' @description Map of simple features in ggplot that is coloured and facetted. 
#' @param data A sf object with defined coordinate reference system in a structure to be plotted untransformed. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_on_top TRUE or FALSE  as to whether the borders are on top of the sf object supplied to the data argument. Defaults to TRUE for points and lines, but FALSE for polygons..
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param pal_borders Colour of the borders. Defaults to "#7F7F7F".
#' @param alpha_fill The opacity of the fill.
#' @param alpha_line The alpha of lines and outlines. 
#' @param alpha_point The alpha of points. 
#' @param alpha_borders Opacity of the borders. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5.
#' @param size_point Size of points. Defaults to 1.5.
#' @param size_borders Size of the borders. Defaults to 0.2.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param col_breaks_n For a numeric colour variable. If "bin" col_method, the intervals on the colour scale for the pretty algorithm to aim for. If "quantile" col_method, the number of equal quantiles. Defaults to 4.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case for categorical colour variables and scales::comma for numeric colour variables. Use ggplot2::waiver() to keep colour labels untransformed.   
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_method The method of colouring features, either "bin", "quantile", "continuous", or "category." If numeric, defaults to "bin".
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' gg_sf_col_facet(example_point, 
#'                 col_var = trend_category, 
#'                 facet_var = trend_category, 
#'                 borders = example_borders)
#'  
gg_sf_col_facet <- function(data,
                            col_var,
                            facet_var,
                            text_var = NULL,
                            borders = NULL,
                            borders_on_top = NULL,
                            pal = NULL,
                            pal_na = "#7F7F7F",
                            pal_rev = FALSE,
                            pal_borders = "#7F7F7F",
                            alpha_fill = NULL,
                            alpha_line = 1,
                            alpha_point = 1,
                            alpha_borders = 0.5,
                            size_line = 0.5,
                            size_point = 1.5,
                            size_borders = 0.2,
                            title = NULL,
                            title_wrap = 80,
                            subtitle = NULL,
                            subtitle_wrap = 80,
                            col_breaks_n = 4,
                            col_cuts = NULL,
                            col_intervals_right = TRUE,
                            col_labels = NULL,
                            col_legend_none = FALSE,
                            col_method = NULL,
                            col_na_rm = FALSE,
                            col_title = NULL,
                            col_title_wrap = 25,
                            facet_labels = snakecase::to_sentence_case,
                            facet_na_rm = FALSE,
                            facet_ncol = NULL,
                            facet_nrow = NULL,
                            facet_rev = FALSE,
                            caption = NULL,
                            caption_wrap = 80,
                            theme = gg_theme(void = TRUE))
{
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  col_var <- rlang::enquo(col_var)
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  #na's
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #vectors
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  #warnings
  if (class(data)[1] != "sf") stop("Please use an sf object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system to data input")
  
  geometry_type <- unique(sf::st_geometry_type(data))
  
  if (!geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")) {
    stop("Please use an sf object with geometry type of POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON")
  }
  
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable")
  
  if (!is.null(borders)) {
    if (class(borders)[1] != "sf") stop("Please use an sf object as borders input")
    if (is.na(sf::st_crs(borders)$proj4string)) stop("Please assign a coordinate reference system to borders object")
  }
  
  if (!is.null(col_method)) {
    if (!col_method %in% c("continuous", "bin", "quantile", "category")) stop("Please use a colour method of 'continuous', 'bin', 'quantile' or 'category'")
  }

  #logical to factor
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  if (is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #titles
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #reverse
  if (facet_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!facet_var, ~forcats::fct_rev(.x)))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #fundamentals
  plot <- ggplot(data) +
    theme
  
  #borders
  if (!is.null(borders)) {
    pal_borders <- scales::alpha(pal_borders, alpha = alpha_borders)
    
    if (is.null(borders_on_top)) {
      if (geometry_type %in% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")) {
        borders_on_top <- FALSE
      } else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
        borders_on_top <- TRUE
      }
    }
    
    if (sf::st_crs(data) != sf::st_crs(borders)) {
      borders <- sf::st_transform(borders, sf::st_crs(data))
    }
    
    if (borders_on_top == FALSE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }
  
  #colour
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "continuous"
  }
  
  if (col_method == "continuous") {
    if (is.null(pal)) pal <- viridis::viridis(20)
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
    if (is.null(col_labels)) col_labels <- scales::label_comma()
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    if (col_method %in% c("quantile", "bin")) {
      if (col_method == "quantile") {
        if (is.null(col_cuts)) col_cuts <- seq(0, 1, 1 / col_breaks_n)
        else {
          if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
          if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
        }  
        col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
        if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      }
      else if (col_method == "bin") {
        if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
        else {
          if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
          if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
        }
      }
      
      if (is.null(col_labels)) col_labels <- scales::label_comma()
      
      if (is.function(col_labels)) {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE, format_fun = col_labels)))
        
        col_labels <- sv_interval_labels_chr
      }
      else {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE)))
      }
      
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
      
      if (is.null(col_labels)) col_labels <- snakecase::to_sentence_case
    }
  }  
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  if (geometry_type %in% c("POINT", "MULTIPOINT")) {
    pal_point <- scales::alpha(pal, alpha = alpha_point)
    pal_na_point <- scales::alpha(pal_na, alpha = alpha_point)
    
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_point,
        data = data
      )
  }
  else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    pal_line <- scales::alpha(pal, alpha = alpha_line)
    pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
    
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, text = !!text_var),
        size = size_line,
        data = data
      )
  }
  else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    if (is.null(alpha_fill)) alpha_fill <- 1
    pal_fill <- scales::alpha(pal, alpha = alpha_fill)
    pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)
    pal_line <- scales::alpha(pal, alpha = alpha_line)
    pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
    
    plot <- plot +
      geom_sf( 
        aes(col = !!col_var, fill = !!col_var, text = !!text_var),
        size = size_line,
        data = data
      )
  }
  
  #colour
  if (col_method == "continuous") {
    if (geometry_type %in% c("POINT", "MULTIPOINT")) {
      plot <- plot +
        scale_colour_gradientn(
          colors = pal_point,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_point,
          name = stringr::str_wrap(col_title, col_title_wrap)) 
    }
    else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
      plot <- plot +
        scale_colour_gradientn(
          colors = pal_line,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)) 
    }
    else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
      plot <- plot +
        scale_colour_gradientn(
          colors = pal_line,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)) +
        scale_fill_gradientn(
          colors = pal_fill,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_fill,
          name = stringr::str_wrap(col_title, col_title_wrap)) +
        guides(colour = "none")
    }
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    if (geometry_type %in% c("POINT", "MULTIPOINT")) {
      plot <- plot +
        scale_colour_manual(
          values = pal_point,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_point,
          name = stringr::str_wrap(col_title, col_title_wrap)
        )       
    }
    else if (geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
      plot <- plot +
        scale_colour_manual(
          values = pal_line,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)
        )       
    }
    else if (geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
      plot <- plot +
        scale_colour_manual(
          values = pal_line,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_line,
          name = stringr::str_wrap(col_title, col_title_wrap)) +      
        scale_fill_manual(
          values = pal_fill,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_fill,
          name = stringr::str_wrap(col_title, col_title_wrap))
    }
  }
  
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")
  
  #borders
  if (!is.null(borders)) {
    if (borders_on_top == TRUE) {
      plot <- plot +
        geom_sf(
          data = borders,
          size = size_borders, 
          colour = pal_borders, 
          fill = "transparent"
        )
    }
  }
  
  #titles & facetting
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = "fixed", ncol = facet_ncol, nrow = facet_nrow)

  return(plot)
}
