#' @title Stars ggplot map.
#' @description Map of an array in ggplot that is not coloured and not facetted. 
#' @param data A stars object with defined coordinate reference system. Note, it cannot be a stars_proxy object. Required input.
#' @param downsample downsampling rate: e.g. 3 keeps rows and cols 1, 4, 7, 10 etc. A value of 0 does not downsample. It can be specified for each dimension. E.g. c(5,5,0) to downsample the first two dimensions but not the third.
#' @param pal Character vector of hex codes. 
#' @param alpha The opacity of the array. Defaults to 0.5.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_on_top TRUE or FALSE  as to whether the borders are on top of the stars array. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#323232".
#' @param borders_size Size of the borders. Defaults to 0.2.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(stars)
#' 
#' gg_stars(example_stars, 
#'      borders = nz)
#'      
gg_stars <- function(data,
                  downsample = 0,
                  pal = NULL,
                  alpha = 0.5,
                  borders = NULL,
                  borders_on_top = TRUE,
                  borders_pal = "#323232",
                  borders_size = 0.2,
                  title = NULL,
                  title_wrap = 80,
                  subtitle = NULL,
                  subtitle_wrap = 80,
                  caption = NULL,
                  caption_wrap = 80,
                  font_family = "",
                  font_size_title = NULL,
                  font_size_body = NULL,
                  mobile = FALSE
) {
  
  if (class(data) != "stars") stop("Please use a stars object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system to data input")
  
  if (!is.null(borders)) {
    if (class(borders)[1] != "sf") stop("Please use an sf object as borders input")
    if (is.na(sf::st_crs(borders)$proj4string)) stop("Please assign a coordinate reference system to borders object")
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  plot <- ggplot() +
    theme_map(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    scale_x_continuous(expand = c(0, 0), name = NULL) +
    scale_y_continuous(expand = c(0, 0), name = NULL) +
    coord_equal()

  if (!is.null(borders)) {
    if (sf::st_crs(data) != sf::st_crs(borders)) borders <- sf::st_transform(borders, sf::st_crs(data))
    if (borders_on_top == FALSE) {
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
  
  plot <- plot +
    stars::geom_stars(aes(x = .data$x, y = .data$y), fill = pal, alpha = alpha, downsample = downsample, data = data)

  if (!is.null(borders)) {
    if (borders_on_top == TRUE) {
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

#' @title Stars ggplot map that is coloured.
#' @description Map of an array in ggplot that is coloured, but not facetted. 
#' @param data A stars object with defined coordinate reference system. Note, it cannot be a stars_proxy object. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param downsample downsampling rate: e.g. 3 keeps rows and cols 1, 4, 7, 10 etc. A value of 0 does not downsample. It can be specified for each dimension. E.g. c(5,5,0) to downsample the first two dimensions but not the third.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the colorbrewer Set1 or viridis.
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha The opacity of features. Defaults to 1 for points/lines, or 0.95 for polygons.
#' @param borders A sf object as administrative boundaries (or coastlines). Defaults to no boundaries added. The rnaturalearth package is a useful source of country and state boundaries.
#' @param borders_on_top TRUE or FALSE  as to whether the borders are on top of the stars array. Defaults to TRUE.
#' @param borders_pal Colour of the borders. Defaults to "#7F7F7F".
#' @param borders_size Size of the borders. Defaults to 0.2.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_label_digits If numeric colour method, the number of digits to round the labels to. Only applicable where col_labels equals NULL.
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to stringr::str_to_sentence for categorical colour variables and scales::comma for numeric colour variables. Use ggplot2::waiver() to keep colour labels untransformed.   
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "bin".
#' @param col_pretty_n For a numeric colour variable of "bin" col_method, the desired number of intervals on the colour scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param col_na_rm TRUE or FALSE of whether to visualise col_var NA values. Defaults to FALSE.
#' @param col_right_closed For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(stars)
#' 
#' gg_stars_col(example_stars,
#'              col_var = nitrate,
#'              col_method = "quantile",
#'              col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
#'              col_na_rm = TRUE,
#'              borders = nz)
#'           
gg_stars_col <- function(data,
                      col_var,
                      downsample = 0,
                      pal = NULL,
                      pal_na = "#7F7F7F",
                      pal_rev = FALSE,
                      alpha = 1,
                      borders = NULL,
                      borders_on_top = TRUE,
                      borders_pal = "#7F7F7F",
                      borders_size = 0.2,
                      title = NULL,
                      title_wrap = 80,
                      subtitle = NULL,
                      subtitle_wrap = 80,
                      col_cuts = NULL,
                      col_label_digits = NULL,
                      col_labels = NULL,
                      col_na_rm = FALSE,
                      col_pretty_n = 5,
                      col_method = NULL,
                      col_right_closed = TRUE,
                      col_title = NULL,
                      col_title_wrap = 25,
                      caption = NULL,
                      caption_wrap = 80,
                      font_family = "",
                      font_size_title = NULL,
                      font_size_body = NULL,
                      mobile = FALSE
) {
  
  col_var <- rlang::enquo(col_var)

  if (col_na_rm == TRUE) {
    na_translate <- FALSE
    pal_na <- "transparent"
  } else na_translate <- TRUE
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (class(data) != "stars") stop("Please use a stars object as data input")
  if (is.na(sf::st_crs(data)$proj4string)) stop("Please assign a coordinate reference system to data input")
  
  if (!is.null(borders)) {
    if (class(borders)[1] != "sf") stop("Please use an sf object as borders input")
    if (is.na(sf::st_crs(borders)$proj4string)) stop("Please assign a coordinate reference system to borders object")
  }
  
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  plot <- ggplot() +
    theme_map(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    scale_x_continuous(expand = c(0, 0), name = NULL) +
    scale_y_continuous(expand = c(0, 0), name = NULL) +
    coord_equal()
  
  if (!is.null(borders)) {
    if (sf::st_crs(data) != sf::st_crs(borders)) borders <- sf::st_transform(borders, sf::st_crs(data))
    if (borders_on_top == FALSE) {
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
    else if (is.numeric(col_var_vctr)) col_method <- "bin"
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
    }
    else if (col_method == "bin") {
      if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_pretty_n)
      else({
        if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
        if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      })
    }
    
    if (is.null(col_labels)) {
      if (is.null(col_label_digits)) {
        col_labels <- scales::comma
      }
      else {
        col_labels <- scales::comma_format(accuracy = 10 ^ -col_label_digits)
      }
    }
    
    if (is.function(col_labels)) {
      data <- data %>% 
        dplyr::mutate(dplyr::across(!!col_var, ~cut_format(.x, col_cuts, 
                                                           right = col_right_closed, 
                                                           include.lowest = TRUE, 
                                                           dig.lab = 50, 
                                                           ordered_result = TRUE, 
                                                           format_fun = col_labels)))
      
      col_labels <- sv_interval_labels_chr
    } else {
      data <- data %>% 
        dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, 
                                                    right = col_right_closed, 
                                                    include.lowest = TRUE, 
                                                    dig.lab = 50, 
                                                    ordered_result = TRUE)))
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
    
    if(is.null(col_labels)) col_labels <- stringr::str_to_sentence
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  plot <- plot +
    stars::geom_stars(
      aes(x = .data$x, y = .data$y, fill = !!col_var),
      alpha = alpha,
      downsample = downsample,
      data = data
    )

  if (mobile == TRUE) col_title_wrap <- 20
  
  plot <- plot + 
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na,
      name = stringr::str_wrap(col_title, col_title_wrap), 
      na.translate = na_translate
    )
  
  if (!is.null(borders)) {
    if (borders_on_top == TRUE) {
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
      guides(col = guide_legend(byrow = TRUE)) +
      guides(fill = guide_legend(byrow = TRUE))
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        caption = stringr::str_wrap(caption, 50)
      )  +
      guides(col = guide_legend(ncol = 1)) +
      guides(col = guide_legend(ncol = 1)) +
      theme_mobile_extra_map()
  }
  
  return(plot)
}


