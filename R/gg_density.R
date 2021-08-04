#' @title Smoothed density ggplot.
#' 
#' @description Smoothed density ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#' @param pal Character vector of hex codes. 
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_pretty_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
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
#' library(palmerpenguins)
#' 
#' gg_density(penguins, 
#'            x_var = body_mass_g)
#' 
gg_density <- function(data,
                    x_var,
                    density_bw = "nrd0",
                    density_adjust = 1,
                    density_kernel = "gaussian",
                    density_n = 512,
                    density_trim = FALSE,
                    pal = NULL,
                    alpha = 0.1,
                    size_line = 0.5,
                    title = NULL,
                    title_wrap = 100,
                    subtitle = NULL,
                    subtitle_wrap = 100,
                    x_balance = FALSE,
                    x_expand = NULL,
                    x_labels = NULL,
                    x_pretty_n = 6,
                    x_title = NULL,
                    x_title_wrap = 50,
                    x_zero = FALSE,
                    x_zero_line = NULL,
                    y_expand = NULL,
                    y_gridlines_minor = FALSE,
                    y_labels = scales::number,
                    y_pretty_n = 5,
                    y_title = NULL,
                    y_title_wrap = 50,
                    caption = NULL,
                    caption_wrap = 80,
                    font_family = "",
                    font_size_title = NULL,
                    font_size_body = NULL,
                    mobile = FALSE
) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var)

  x_var_vctr <- dplyr::pull(data, !!x_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")

  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  
  if (is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if (is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  plot <- ggplot(data) +
    theme_y_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    stat_density(aes(x = !!x_var, y = .data$..density..), 
                 bw = density_bw, adjust = density_adjust, kernel = density_kernel, n = density_n, trim = density_trim,
                 col = pal, 
                 fill = pal, 
                 alpha = alpha, 
                 size = size_line) 
    
  if (is.numeric(x_var_vctr)) {
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    if (is.null(x_labels)) x_labels <- waiver()

    if (mobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
    }
  }
  
  if (is.numeric(x_var_vctr)) {
    plot <- plot +
      scale_x_continuous(expand = x_expand,
                         breaks = x_breaks,
                         limits = x_limits,
                         labels = x_labels,
                         oob = scales::squish)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  
  y_var_vctr <- c(0, sv_density_max(data, !!x_var))

  if (is.null(y_expand)) y_expand <- c(0, 0)
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, pretty_n = y_pretty_n, trans = "identity", zero = TRUE)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(
        expand = y_expand,
        breaks = y_breaks,
        limits = y_limits,
        trans = "identity",
        labels = y_labels,
        oob = scales::squish
      )
  })
  
  if (y_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.2))
  }

  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      )
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(x_title, 20),
        y = stringr::str_wrap(y_title, 30),
        caption = stringr::str_wrap(caption, 50)
      ) +
      theme_mobile_extra()
  }
  
  return(plot)
}

#' @title Smoothed density ggplot that is coloured.
#' 
#' @description Smoothed density ggplot that is coloured but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha The alpha of the fill. Defaults to 0.1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_pretty_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_labels A function or named vector to modify colour scale labels. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_na TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
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
#' library(palmerpenguins)
#' 
#' gg_density_col(penguins, 
#'                x_var = body_mass_g, 
#'                col_var = species)
#' 
gg_density_col <- function(data,
                           x_var,
                           col_var,
                           density_bw = "nrd0",
                           density_adjust = 1,
                           density_kernel = "gaussian",
                           density_n = 512,
                           density_trim = FALSE,
                           pal = NULL,
                           pal_na = "#7F7F7FFF",
                           pal_rev = FALSE,
                           alpha = 0.1,
                           size_line = 0.5,
                           title = NULL,
                           title_wrap = 100,
                           subtitle = NULL,
                           subtitle_wrap = 100,
                           x_balance = FALSE,
                           x_expand = NULL,
                           x_labels = NULL,
                           x_pretty_n = 6,
                           x_title = NULL,
                           x_title_wrap = 50,
                           x_zero = FALSE,
                           x_zero_line = NULL,
                           y_expand = NULL,
                           y_gridlines_minor = FALSE,
                           y_labels = scales::number,
                           y_pretty_n = 5,
                           y_title = NULL,
                           y_title_wrap = 50,
                           col_labels = stringr::str_to_sentence,
                           col_na = TRUE,
                           col_rev = FALSE,
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
  x_var <- rlang::enquo(x_var)
  col_var <- rlang::enquo(col_var) #categorical var
  
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a density plot")
  
  # if (!is.null(position)) {
  #   if (!position %in% c("identity", "stack")) stop("Please use a position of either 'identity' or 'stack'")
  # }
    
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (col_rev == TRUE){
    if (is.factor(col_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
    }
    else if (is.character(col_var_vctr) | is.logical(col_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(factor(.x))))
    }
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if (is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  # if (is.null(alpha)) {
  #   if (position == "identity") alpha <- 0.1   
  #   else alpha <- 1    
  # }

  plot <- ggplot(data) +
    theme_y_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    stat_density(aes(x = !!x_var, y = .data$..density.., col = !!col_var, fill = !!col_var), 
                 position = "identity",
                 bw = density_bw, adjust = density_adjust, kernel = density_kernel, n = density_n, trim = density_trim,
                 alpha = alpha, 
                 size = size_line) 
  
  if (is.numeric(x_var_vctr)) {
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    if (is.null(x_labels)) x_labels <- waiver()
    
    if (mobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
    }
  }
  
  if (is.numeric(x_var_vctr)) {
    plot <- plot +
      scale_x_continuous(expand = x_expand,
                         breaks = x_breaks,
                         limits = x_limits,
                         labels = x_labels,
                         oob = scales::squish)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  
  y_var_vctr <- c(0, sv_density_max_col(data, !!x_var, !!col_var))
  # if (position == "identity") y_var_vctr <- c(0, sv_density_max_col(data, !!x_var, !!col_var))
  # if (position == "stack") y_var_vctr <- c(0, sv_density_max_col_stack(data, !!x_var, !!col_var))
  # if (position == "fill") y_var_vctr <- c(0, 1)
  
  if (is.null(y_expand)) y_expand <- c(0, 0)
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, pretty_n = y_pretty_n, trans = "identity", zero = TRUE)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(
        expand = y_expand,
        breaks = y_breaks,
        limits = y_limits,
        trans = "identity",
        labels = y_labels,
        oob = scales::squish
      )
  })
  
  if (mobile == TRUE) col_title_wrap <- 20
  
  plot <- plot +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) +
    scale_colour_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) 
  
  if (y_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.2))
  }

  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(fill = guide_legend(byrow = TRUE), col = guide_legend(byrow = TRUE))
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(x_title, 20),
        y = stringr::str_wrap(y_title, 30),
        caption = stringr::str_wrap(caption, 50)
      ) +
      guides(fill = guide_legend(ncol = 1), col = guide_legend(ncol = 1)) +
      theme_mobile_extra()
  }
  
  return(plot)
}


#' @title Smoothed density ggplot that is facetted.
#' 
#' @description Smoothed density ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#' @param pal Character vector of hex codes. 
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_pretty_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na TRUE or FALSE of whether to include facet_var NA values. Defaults to TRUE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_density_facet(penguins, 
#'                  x_var = body_mass_g, 
#'                  facet_var = species, 
#'                  x_pretty_n = 2)
#' 
gg_density_facet <- function(data,
                             x_var,
                             facet_var,
                             density_bw = "nrd0",
                             density_adjust = 1,
                             density_kernel = "gaussian",
                             density_n = 512,
                             density_trim = FALSE,
                             pal = NULL,
                             alpha = 0.1,
                             size_line = 0.5,
                             title = NULL,
                             title_wrap = 100,
                             subtitle = NULL,
                             subtitle_wrap = 100,
                             x_balance = FALSE,
                             x_expand = NULL,
                             x_labels = NULL,
                             x_pretty_n = 3,
                             x_title = NULL,
                             x_title_wrap = 50,
                             x_zero = FALSE,
                             x_zero_line = NULL,
                             y_expand = NULL,
                             y_gridlines_minor = FALSE,
                             y_labels = scales::number,
                             y_pretty_n = 4,
                             y_title = NULL,
                             y_title_wrap = 50,
                             facet_labels = stringr::str_to_sentence,
                             facet_na = TRUE,
                             facet_ncol = NULL,
                             facet_nrow = NULL,
                             facet_scales = "fixed",
                             caption = NULL,
                             caption_wrap = 80,
                             font_family = "",
                             font_size_title = NULL,
                             font_size_body = NULL
) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var)
  facet_var <- rlang::enquo(facet_var) #categorical var
  
  if (facet_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a smoothed density plot")
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  
  if (is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if (is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  plot <- ggplot(data) +
    theme_y_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    stat_density(aes(x = !!x_var, y = .data$..density..), 
                 bw = density_bw, adjust = density_adjust, kernel = density_kernel, n = density_n, trim = density_trim,
                 col = pal, 
                 fill = pal, 
                 alpha = alpha, 
                 size = size_line) 
  
  if (facet_scales %in% c("fixed", "free_y")) {
    if (is.numeric(x_var_vctr)) {
      
      x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
      x_zero <- x_zero_list[[1]]
      x_zero_line <- x_zero_list[[2]]
      
      x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_breaks), max(x_breaks))
      if (is.null(x_expand)) x_expand <- c(0, 0)
      if (is.null(x_labels)) x_labels <- waiver()
    }
    
    if (is.numeric(x_var_vctr)) {
      plot <- plot +
        scale_x_continuous(expand = x_expand,
                           breaks = x_breaks,
                           limits = x_limits,
                           labels = x_labels,
                           oob = scales::squish)
      
      if (x_zero_line == TRUE) {
        plot <- plot +
          geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
      }
    }
  }
  
  if (is.null(y_expand)) y_expand <- c(0, 0)
  
  if (facet_scales %in% c("fixed", "free_x")) {
    y_var_vctr <- c(0, sv_density_max_facet(data, !!x_var, !!facet_var))
    
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, pretty_n = y_pretty_n, trans = "identity", zero = TRUE)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(
          expand = y_expand,
          breaks = y_breaks,
          limits = y_limits,
          trans = "identity",
          labels = y_labels,
          oob = scales::squish
        )
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand,
                         labels = y_labels,
                         oob = scales::squish)
  }
  
  if (y_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.2))
  }
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(x_title, x_title_wrap),
      y = stringr::str_wrap(y_title, y_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
  
  return(plot)
}

#' @title Smoothed density ggplot that is coloured and facetted.
#' 
#' @description Smoothed density ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha The alpha of the fill. Defaults to 0.1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_pretty_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_labels A function or named vector to modify colour scale labels. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_na TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na TRUE or FALSE of whether to include facet_var NA values. Defaults to TRUE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_density_col_facet(penguins, 
#'                      x_var = body_mass_g, 
#'                      col_var = sex, 
#'                      facet_var = species, 
#'                      x_pretty_n = 2, 
#'                      col_na = FALSE)
#' 
gg_density_col_facet <- function(data,
                                 x_var,
                                 col_var,
                                 facet_var,
                                 density_bw = "nrd0",
                                 density_adjust = 1,
                                 density_kernel = "gaussian",
                                 density_n = 512,
                                 density_trim = FALSE,
                                 pal = NULL,
                                 pal_na = "#7F7F7FFF",
                                 pal_rev = FALSE,
                                 alpha = 0.1,
                                 size_line = 0.5,
                                 title = NULL,
                                 title_wrap = 100,
                                 subtitle = NULL,
                                 subtitle_wrap = 100,
                                 x_balance = FALSE,
                                 x_expand = NULL,
                                 x_labels = NULL,
                                 x_pretty_n = 3,
                                 x_title = NULL,
                                 x_title_wrap = 50,
                                 x_zero = FALSE,
                                 x_zero_line = NULL,
                                 y_expand = NULL,
                                 y_gridlines_minor = FALSE,
                                 y_labels = scales::number,
                                 y_pretty_n = 4,
                                 y_title = NULL,
                                 y_title_wrap = 50,
                                 col_labels = stringr::str_to_sentence,
                                 col_na = TRUE,
                                 col_rev = FALSE,
                                 col_title = NULL,
                                 col_title_wrap = 25,
                                 facet_labels = stringr::str_to_sentence,
                                 facet_na = TRUE,
                                 facet_ncol = NULL,
                                 facet_nrow = NULL,
                                 facet_scales = "fixed",
                                 caption = NULL,
                                 caption_wrap = 80,
                                 font_family = "",
                                 font_size_title = NULL,
                                 font_size_body = NULL
) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var)
  col_var <- rlang::enquo(col_var) #categorical var
  facet_var <- rlang::enquo(facet_var) #categorical var
  
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  if (facet_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a density plot")
  
  # if (!is.null(position)) {
  #   if (!position %in% c("identity", "stack")) stop("Please use a position of either 'identity' or 'stack'")
  # }
  
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (col_rev == TRUE){
    if (is.factor(col_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
    }
    else if (is.character(col_var_vctr) | is.logical(col_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(factor(.x))))
    }
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if (is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  # if (is.null(alpha)) {
  #   if (position == "identity") alpha <- 0.1   
  #   else alpha <- 1    
  # }
  
  plot <- ggplot(data) +
    theme_y_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    stat_density(aes(x = !!x_var, y = .data$..density.., col = !!col_var, fill = !!col_var), 
                 position = "identity",
                 bw = density_bw, adjust = density_adjust, kernel = density_kernel, n = density_n, trim = density_trim,
                 alpha = alpha, 
                 size = size_line) 
  
  if (facet_scales %in% c("fixed", "free_y")) {
    if (is.numeric(x_var_vctr)) {
      
      x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
      x_zero <- x_zero_list[[1]]
      x_zero_line <- x_zero_list[[2]]
      
      x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_breaks), max(x_breaks))
      if (is.null(x_expand)) x_expand <- c(0, 0)
      if (is.null(x_labels)) x_labels <- waiver()
    }
    
    if (is.numeric(x_var_vctr)) {
      plot <- plot +
        scale_x_continuous(expand = x_expand,
                           breaks = x_breaks,
                           limits = x_limits,
                           labels = x_labels,
                           oob = scales::squish)
      
      if (x_zero_line == TRUE) {
        plot <- plot +
          geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
      }
    }
  }
  
  if (is.null(y_expand)) y_expand <- c(0, 0)
  
  if (facet_scales %in% c("fixed", "free_x")) {
    
    if(rlang::as_name(col_var) == rlang::as_name(facet_var)) {
      y_var_vctr <- c(0, sv_density_max_facet(data, !!x_var, !!facet_var))      
    }
    else({
      y_var_vctr <- c(0, sv_density_max_col_facet(data, !!x_var, !!col_var, !!facet_var))      
    })
    # if (position == "identity") y_var_vctr <- c(0, sv_density_max_col_facet(data, !!x_var, !!col_var, !!facet_var))
    # if (position == "stack") y_var_vctr <- c(0, sv_density_max_col_facet_stack(data, !!x_var, !!col_var, !!facet_var))
    # if (position == "fill") y_var_vctr <- c(0, 1)
    
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, pretty_n = y_pretty_n, trans = "identity", zero = TRUE)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(
          expand = y_expand,
          breaks = y_breaks,
          limits = y_limits,
          trans = "identity",
          labels = y_labels,
          oob = scales::squish
        )
    })
    
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand,
                         labels = y_labels,
                         oob = scales::squish)
  }
  
  if (y_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.2))
  }
  
  plot <- plot +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) +
    scale_colour_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(x_title, x_title_wrap),
      y = stringr::str_wrap(y_title, y_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    guides(fill = guide_legend(byrow = TRUE), col = guide_legend(byrow = TRUE)) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
  
  return(plot)
}
