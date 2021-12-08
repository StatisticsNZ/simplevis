#' @title Smoothed ggplot.
#' @description Smoothed ggplot that is not coloured and not facetted.
#' @param data An ungrouped tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param alpha The opacity of ribbons. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80.
#' @param theme A ggplot2 theme.  
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @param alg_method Smoothing algorithm to use. See ggplot2::geom_smooth for further details.
#' @param alg_formula Formula to use in smoothing function. See ggplot2::geom_smooth for further details.
#' @param alg_se TRUE or FALSE of whether to show condidence as a ribbon. 
#' @param alg_level The level of confidence to calculate for the ribbon.
#' @param alg_n Number of points at which to evaluate smoother. 
#' @param alg_span Controls the amount of smoothing for the default loess smoother. See ggplot2::geom_smooth for further details.

#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_smooth(penguins, 
#'          x_var = bill_length_mm, 
#'          y_var = body_mass_g)
#' 
gg_smooth <- function(data,
                     x_var,
                     y_var,
                     text_var = NULL,
                     pal = pal_viridis_reorder(1),
                     alpha = 0.5,
                     size_line = 0.5,
                     title = NULL,
                     title_wrap = 80,
                     subtitle = NULL,
                     subtitle_wrap = 80,
                     x_balance = FALSE,
                     x_breaks_n = 5,
                     x_expand = NULL,
                     x_labels = scales::label_comma(),
                     x_title = NULL,
                     x_title_wrap = 50,
                     x_trans = "identity",
                     x_zero = FALSE,
                     x_zero_line = NULL,
                     y_balance = FALSE,
                     y_breaks_n = 5,
                     y_expand = c(0, 0),
                     y_labels = scales::label_comma(),
                     y_title = NULL,
                     y_title_wrap = 50,
                     y_trans = "identity",
                     y_zero = FALSE,
                     y_zero_line = NULL,
                     caption = NULL,
                     caption_wrap = 80,
                     theme = gg_theme(),
                     alg_method = NULL,
                     alg_formula = NULL,
                     alg_se = TRUE,
                     alg_level = 0.95,
                     alg_span = 0.75, 
                     alg_n = 80,
                     mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  #na's
  data <- data %>% 
    dplyr::filter(!is.na(!!x_var), !is.na(!!y_var))
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  #warnings
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a smoothed plot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a smoothed plot")
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  #colour
  pal <- pal[1]
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_smooth(aes(!!x_var, !!y_var, group = 1),
                col = pal,
                fill = "#D3D3D3", size = size_line, alpha = alpha,
                se = alg_se, level = alg_level, 
                method = alg_method, formula = alg_formula, span = alg_span, n = alg_n) 

  #x scale  
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, trans = "identity", zero = x_zero, mobile = mobile)
  x_limits <- c(min(x_breaks), max(x_breaks))
  if (is.null(x_expand)) x_expand <- c(0, 0)
  
  if (mobile == TRUE) {
    x_breaks <- x_limits
    if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
  }
  
  plot <- plot +
    scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, trans = "identity", oob = scales::oob_squish)
  
  if (x_zero_line == TRUE) {
    plot <- plot +
      geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #y scale
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, trans = y_trans, zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, trans = y_trans, labels = y_labels, oob = scales::oob_squish)
  })
  
  if (y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #titles 
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
      theme_mobile_extra() #extra mobile theme components
  }
  
  return(plot)
}

#' @title Smoothed ggplot that is coloured.
#' @description Smoothed ggplot that is coloured, but not facetted.
#' @param data An ungrouped tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha The opacity of ribbons. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels A function or named vector to modify colour scale labels. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.  
#' @param alg_method Smoothing algorithm to use. See ggplot2::geom_smooth for further details.
#' @param alg_formula Formula to use in smoothing function. See ggplot2::geom_smooth for further details.
#' @param alg_se TRUE or FALSE of whether to show condidence as a ribbon. 
#' @param alg_level The level of confidence to calculate for the ribbon.
#' @param alg_n Number of points at which to evaluate smoother. 
#' @param alg_span Controls the amount of smoothing for the default loess smoother. See ggplot2::geom_smooth for further details.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_smooth_col(penguins, 
#'              x_var = bill_length_mm, 
#'              y_var = body_mass_g, 
#'              col_var = species)
#' 
gg_smooth_col <- function(data,
                         x_var,
                         y_var,
                         col_var,
                         text_var = NULL,
                         pal = NULL,
                         pal_na = "#7F7F7F",
                         pal_rev = FALSE,
                         alpha = 0.5,
                         size_line = 0.5,
                         title = NULL,
                         title_wrap = 80,
                         subtitle = NULL,
                         subtitle_wrap = 80,
                         x_balance = FALSE,
                         x_breaks_n = 5,
                         x_expand = NULL,
                         x_labels = scales::label_comma(),
                         x_title = NULL,
                         x_title_wrap = 50,
                         x_trans = "identity",
                         x_zero = FALSE,
                         x_zero_line = NULL,
                         y_balance = FALSE,
                         y_breaks_n = 5,
                         y_expand = c(0, 0),
                         y_labels = scales::label_comma(),
                         y_title = NULL,
                         y_title_wrap = 50,
                         y_trans = "identity",
                         y_zero = FALSE,
                         y_zero_line = NULL,
                         col_labels = snakecase::to_sentence_case,
                         col_na_rm = FALSE,
                         col_title = NULL,
                         col_title_wrap = 25,
                         caption = NULL,
                         caption_wrap = 80,
                         theme = gg_theme(),
                         alg_method = NULL,
                         alg_formula = NULL,
                         alg_se = TRUE,
                         alg_level = 0.95,
                         alg_span = 0.75, 
                         alg_n = 80,
                         mobile = FALSE){
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) #numeric var
  col_var <- rlang::enquo(col_var)
  text_var <- rlang::enquo(text_var)
  
  #na's
  data <- data %>% 
    dplyr::filter(!is.na(!!x_var), !is.na(!!y_var))
  
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  #warnings
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a smoothed plot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a smoothed plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a line plot")
  
  #logical to factor
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }

  ##titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #colour
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)

  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_smooth(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var),
                fill = "#D3D3D3", size = size_line, alpha = alpha,
                se = alg_se, level = alg_level, 
                method = alg_method, formula = alg_formula, span = alg_span, n = alg_n) 
  
  #x scale  
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, trans = "identity", zero = x_zero, mobile = mobile)
  x_limits <- c(min(x_breaks), max(x_breaks))
  if (is.null(x_expand)) x_expand <- c(0, 0)
  
  if (mobile == TRUE) {
    x_breaks <- x_limits
    if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
  }
  
  plot <- plot +
    scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, trans = "identity", oob = scales::oob_squish)
  
  if (x_zero_line == TRUE) {
    plot <- plot +
      geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #y scale
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, trans = y_trans, zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, trans = y_trans, labels = y_labels, oob = scales::oob_squish)
  })
  
  if (y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #colour
  if (mobile == TRUE) col_title_wrap <- 20
  
  plot <- plot +
    scale_colour_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na, 
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) 
  
  #titles
  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(col = guide_legend(byrow = TRUE))
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(x_title, 20),
        y = stringr::str_wrap(y_title, 30),
        caption = stringr::str_wrap(caption, 50)
      )  +
      guides(col = guide_legend(ncol = 1)) +
      theme_mobile_extra() #extra mobile theme components
  }
  
  return(plot)
}

#' @title Smoothed ggplot that is facetted.
#' @description Smoothed ggplot that is facetted, but not coloured.
#' @param data An ungrouped tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param alpha The opacity of ribbons. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.  
#' @param alg_method Smoothing algorithm to use. See ggplot2::geom_smooth for further details.
#' @param alg_formula Formula to use in smoothing function. See ggplot2::geom_smooth for further details.
#' @param alg_se TRUE or FALSE of whether to show condidence as a ribbon. 
#' @param alg_level The level of confidence to calculate for the ribbon.
#' @param alg_n Number of points at which to evaluate smoother. 
#' @param alg_span Controls the amount of smoothing for the default loess smoother. See ggplot2::geom_smooth for further details.
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_smooth_facet(penguins, 
#'                x_var = bill_length_mm, 
#'                y_var = body_mass_g, 
#'                facet_var = species)
#' 
gg_smooth_facet <- function(data,
                           x_var,
                           y_var,
                           facet_var,
                           text_var = NULL,
                           pal = pal_viridis_reorder(1),
                           alpha = 0.5,
                           size_line = 0.5,
                           title = NULL,
                           title_wrap = 80,
                           subtitle = NULL,
                           subtitle_wrap = 80,
                           x_balance = FALSE,
                           x_breaks_n = 2,
                           x_expand = NULL,
                           x_labels = scales::label_comma(),
                           x_title = NULL,
                           x_title_wrap = 50,
                           x_trans = "identity",
                           x_zero = FALSE,
                           x_zero_line = NULL,
                           y_balance = FALSE,
                           y_breaks_n = 3,
                           y_expand = c(0, 0),
                           y_labels = scales::label_comma(),
                           y_title = NULL,
                           y_title_wrap = 50,
                           y_trans = "identity",
                           y_zero = FALSE,
                           y_zero_line = NULL,
                           facet_labels = snakecase::to_sentence_case,
                           facet_na_rm = FALSE,
                           facet_ncol = NULL,
                           facet_nrow = NULL,
                           facet_scales = "fixed",
                           caption = NULL,
                           caption_wrap = 80,
                           theme = gg_theme(), 
                           alg_method = NULL,
                           alg_formula = NULL,
                           alg_se = TRUE,
                           alg_level = 0.95,
                           alg_span = 0.75, 
                           alg_n = 80) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) #numeric var
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  #na's
  data <- data %>% 
    dplyr::filter(!is.na(!!x_var), !is.na(!!y_var))
  
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  #warnings
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a smoothed plot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a smoothed plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a point plot")
  
  #logical to factor
  if (is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  #colour
  pal <- pal[1]
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_smooth(aes(!!x_var, !!y_var, group = 1),
                col = pal,
                fill = "#D3D3D3", size = size_line, alpha = alpha,
                se = alg_se, level = alg_level, 
                method = alg_method, formula = alg_formula, span = alg_span, n = alg_n) 
  
  #x scale
  if (facet_scales %in% c("fixed", "free_y")) {
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, trans = "identity", zero = x_zero, mobile = FALSE)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    
    plot <- plot +
      scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, trans = "identity", oob = scales::oob_squish)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  
  #y scale
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  if (facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (facet_scales %in% c("fixed", "free_x")) {
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, trans = y_trans, zero = y_zero)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, trans = y_trans, labels = y_labels, oob = scales::oob_squish)
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, trans = y_trans, labels = y_labels, oob = scales::oob_squish)
  }
  
  if (y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #titles & facetting
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

#' @title Smoothed ggplot that is coloured and facetted.
#' @description Smoothed ggplot that is coloured and facetted.
#' @param data An ungrouped tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha The opacity of ribbons. Defaults to 0.5.
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels A function or named vector to modify colour scale labels. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.  
#' @param alg_method Smoothing algorithm to use. See ggplot2::geom_smooth for further details.
#' @param alg_formula Formula to use in smoothing function. See ggplot2::geom_smooth for further details.
#' @param alg_se TRUE or FALSE of whether to show condidence as a ribbon. 
#' @param alg_level The level of confidence to calculate for the ribbon.
#' @param alg_n Number of points at which to evaluate smoother. 
#' @param alg_span Controls the amount of smoothing for the default loess smoother. See ggplot2::geom_smooth for further details.
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_smooth_col_facet(penguins, 
#'                    x_var = bill_length_mm, 
#'                    y_var = body_mass_g, 
#'                    col_var = sex, 
#'                    facet_var = species, 
#'                    col_na_rm = TRUE)
#' 
gg_smooth_col_facet <- function(data,
                               x_var,
                               y_var,
                               col_var,
                               facet_var,
                               text_var = NULL,
                               pal = NULL,
                               pal_na = "#7F7F7F",
                               pal_rev = FALSE,
                               alpha = 0.5,
                               size_line = 0.5,
                               title = NULL,
                               title_wrap = 80,
                               subtitle = NULL,
                               subtitle_wrap = 80,
                               x_balance = FALSE,
                               x_breaks_n = 2,
                               x_expand = NULL,
                               x_labels = scales::label_comma(),
                               x_title = NULL,
                               x_title_wrap = 50,
                               x_trans = "identity",
                               x_zero = FALSE,
                               x_zero_line = NULL,
                               y_balance = FALSE,
                               y_breaks_n = 3,
                               y_expand = c(0, 0),
                               y_labels = scales::label_comma(),
                               y_title = NULL,
                               y_title_wrap = 50,
                               y_trans = "identity",
                               y_zero = FALSE,
                               y_zero_line = NULL,
                               col_labels = snakecase::to_sentence_case,
                               col_na_rm = FALSE,
                               col_title = NULL,
                               col_title_wrap = 25,
                               facet_labels = snakecase::to_sentence_case,
                               facet_na_rm = FALSE,
                               facet_ncol = NULL,
                               facet_nrow = NULL,
                               facet_scales = "fixed",
                               caption = NULL,
                               caption_wrap = 80,
                               theme = gg_theme(), 
                               alg_method = NULL,
                               alg_formula = NULL,
                               alg_se = TRUE,
                               alg_level = 0.95,
                               alg_span = 0.75, 
                               alg_n = 80) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) #numeric var
  col_var <- rlang::enquo(col_var)
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  #na's
  data <- data %>% 
    dplyr::filter(!is.na(!!x_var), !is.na(!!y_var))
  
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  #warnings
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a smoothed plot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a smoothed plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a line plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a point plot")
  
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
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #colour
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_smooth(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var),
                fill = "#D3D3D3", size = size_line, alpha = alpha,
                se = alg_se, level = alg_level, 
                method = alg_method, formula = alg_formula, span = alg_span, n = alg_n) 

  #x scale
  if (facet_scales %in% c("fixed", "free_y")) {
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, trans = "identity", zero = x_zero, mobile = FALSE)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    
    plot <- plot +
      scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, trans = "identity", oob = scales::oob_squish)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  
  #y scale
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  if (facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (facet_scales %in% c("fixed", "free_x")) {
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, trans = y_trans, zero = y_zero)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, trans = y_trans, labels = y_labels, oob = scales::oob_squish)
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, trans = y_trans, labels = y_labels, oob = scales::oob_squish)
  }
  
  if (y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }

  #colour, titles & facetting
  plot <- plot +
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
    guides(col = guide_legend(byrow = TRUE)) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
  
  
  return(plot)
}
