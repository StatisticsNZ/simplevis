#' @title Density ggplot.
#' 
#' @description Density ggplot that is not coloured and not facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be transformed to density statistics. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param pal Character vector of hex codes. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2.  
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the  
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_breaks_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' 
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
                       pal = pal_viridis_reorder(1),
                       alpha_fill = 0.2,
                       alpha_line = 1,
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
                       x_zero = FALSE,
                       x_zero_line = NULL,
                       y_breaks_n = 5,
                       y_expand = c(0, 0),
                       y_labels = scales::label_number(big.mark = ","),
                       y_title = NULL,
                       y_title_wrap = 50,
                       caption = NULL,
                       caption_wrap = 80,
                       theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE),
                       model_bw = "nrd0",
                       model_adjust = 1,
                       model_kernel = "gaussian",
                       model_n = 512,
                       model_trim = FALSE,
                       mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var)
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  
  #warning
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  
  #colour
  pal <- pal[1]
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)

  #fundamentals
  plot <- ggplot(data) +
    theme +
    geom_density(aes(x = !!x_var), 
                 bw = model_bw, adjust = model_adjust, kernel = model_kernel, n = model_n, trim = model_trim,
                 col = pal_line, 
                 fill = pal_fill, 
                 size = size_line) 
  #x scale  
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = mobile)
  x_limits <- c(min(x_breaks), max(x_breaks))
  if (is.null(x_expand)) x_expand <- c(0, 0)
  
  if (mobile == TRUE) {
    x_breaks <- x_limits
    if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
  }
  
  plot <- plot +
    scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, oob = scales::oob_squish)
  
  if (x_zero_line == TRUE) {
    plot <- plot +
      geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #y scale
  y_var_vctr <- c(0, sv_density_max(data, !!x_var))
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, breaks_n = y_breaks_n, zero = TRUE)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels, oob = scales::oob_squish)
  })
  
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
      theme_mobile_extra()
  }
  
  return(plot)
}

#' @title Density ggplot that is coloured.
#' 
#' @description Density ggplot that is coloured but not facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be transformed to density statistics. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_fill The opacity of the fill. Defaults to 0.2.  
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the  
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_breaks_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_labels A function or named vector to modify colour scale labels. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_density_col(penguins, 
#'                x_var = body_mass_g, 
#'                col_var = sex, 
#'                col_na_rm = TRUE)
#' 
gg_density_col <- function(data,
                           x_var,
                           col_var,
                           pal = NULL,
                           pal_na = "#7F7F7F",
                           pal_rev = FALSE,
                           alpha_fill = 0.2,
                           alpha_line = 1,
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
                           x_zero = FALSE,
                           x_zero_line = NULL,
                           y_breaks_n = 5,
                           y_expand = c(0, 0),
                           y_labels = scales::label_number(big.mark = ","),
                           y_title = NULL,
                           y_title_wrap = 50,
                           col_labels = snakecase::to_sentence_case,
                           col_legend_none = FALSE,
                           col_na_rm = FALSE,
                           col_title = NULL,
                           col_title_wrap = 25,
                           caption = NULL,
                           caption_wrap = 80,
                           theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE),
                           model_bw = "nrd0",
                           model_adjust = 1,
                           model_kernel = "gaussian",
                           model_n = 512,
                           model_trim = FALSE,
                           mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var)
  col_var <- rlang::enquo(col_var) #categorical var
  
  #na's
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  #warnings
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a density plot")
  
  #logical to factor
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #colour
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)
  pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    geom_density(aes(x = !!x_var, col = !!col_var, fill = !!col_var), 
                 position = "identity",
                 bw = model_bw, adjust = model_adjust, kernel = model_kernel, n = model_n, trim = model_trim,
                 size = size_line) 
  
  #x scale  
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = mobile)
  x_limits <- c(min(x_breaks), max(x_breaks))
  if (is.null(x_expand)) x_expand <- c(0, 0)
  
  if (mobile == TRUE) {
    x_breaks <- x_limits
    if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
  }
  
  plot <- plot +
    scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, oob = scales::oob_squish)
  
  if (x_zero_line == TRUE) {
    plot <- plot +
      geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #y scale
  y_var_vctr <- c(0, sv_density_max_col(data, !!x_var, !!col_var))
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, breaks_n = y_breaks_n, zero = TRUE)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels, oob = scales::oob_squish)
  })
  
  #colour
  if (mobile == TRUE) col_title_wrap <- 20
  
  plot <- plot +
    scale_colour_manual(
      values = pal_line,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na_line,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) +
    scale_fill_manual(
      values = pal_fill,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na_fill,
      name = stringr::str_wrap(col_title, col_title_wrap)
    )
  
  if (mobile == TRUE & col_legend_none == TRUE) {
    plot <- plot +
      guides(col = guide_legend(ncol = 1), 
             fill = guide_legend(ncol = 1))
  }
  
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")

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
      theme_mobile_extra()
  }
  
  return(plot)
}

#' @title Density ggplot that is facetted.
#' 
#' @description Density ggplot that is facetted, but not coloured.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be transformed to density statistics. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param pal Character vector of hex codes. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2.  
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_breaks_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_density_facet(penguins, 
#'                  x_var = body_mass_g, 
#'                  facet_var = species)
#' 
gg_density_facet <- function(data,
                             x_var,
                             facet_var,
                             pal = pal_viridis_reorder(1),
                             alpha_fill = 0.2,
                             alpha_line = 1,
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
                             x_zero = FALSE,
                             x_zero_line = NULL,
                             y_breaks_n = 3,
                             y_expand = c(0, 0),
                             y_labels = scales::label_number(big.mark = ","),
                             y_title = NULL,
                             y_title_wrap = 50,
                             facet_labels = snakecase::to_sentence_case,
                             facet_na_rm = FALSE,
                             facet_ncol = NULL,
                             facet_nrow = NULL,
                             facet_rev = FALSE,
                             facet_scales = "fixed",
                             caption = NULL,
                             caption_wrap = 80,
                             theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE), 
                             model_bw = "nrd0",
                             model_adjust = 1,
                             model_kernel = "gaussian",
                             model_n = 512,
                             model_trim = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var)
  facet_var <- rlang::enquo(facet_var) #categorical var
  
  #na's
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  #warnings
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a smoothed density plot")
  
  #titles
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  
  #reverse
  if (facet_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!facet_var, ~forcats::fct_rev(.x)))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #colour
  pal <- pal[1]
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    geom_density(aes(x = !!x_var), 
                 bw = model_bw, adjust = model_adjust, kernel = model_kernel, n = model_n, trim = model_trim,
                 col = pal_line, 
                 fill = pal_fill, 
                 size = size_line) 
  
  #x scale
  if (facet_scales %in% c("fixed", "free_y")) {
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = FALSE)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    
    plot <- plot +
      scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, oob = scales::oob_squish)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  
  #y scale
  if (facet_scales %in% c("fixed", "free_x")) {
    y_var_vctr <- c(0, sv_density_max_facet(data, !!x_var, !!facet_var))
    
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, breaks_n = y_breaks_n, zero = TRUE)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels, oob = scales::oob_squish)
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand,
                         labels = y_labels,
                         oob = scales::oob_squish)
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

#' @title Density ggplot that is coloured and facetted.
#' 
#' @description Density ggplot that is coloured and facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be transformed to density statistics. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_fill The opacity of the fill. Defaults to 0.2.  
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of density areas.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_breaks_n For a numeric y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_labels A function or named vector to modify colour scale labels. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' 
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
#'                      col_na_rm = TRUE)
#'                      
gg_density_col_facet <- function(data,
                                 x_var,
                                 col_var,
                                 facet_var,
                                 pal = NULL,
                                 pal_na = "#7F7F7F",
                                 pal_rev = FALSE,
                                 alpha_fill = 0.2,
                                 alpha_line = 1,
                                 size_line = 0.5,
                                 title = NULL,
                                 title_wrap = 80,
                                 subtitle = NULL,
                                 subtitle_wrap = 80,
                                 x_breaks_n = 2,
                                 x_balance = FALSE,
                                 x_expand = NULL,
                                 x_labels = scales::label_comma(),
                                 x_title = NULL,
                                 x_title_wrap = 50,
                                 x_zero = FALSE,
                                 x_zero_line = NULL,
                                 y_breaks_n = 3,
                                 y_expand = c(0, 0),
                                 y_labels = scales::label_number(big.mark = ","),
                                 y_title = NULL,
                                 y_title_wrap = 50,
                                 col_labels = snakecase::to_sentence_case,
                                 col_legend_none = FALSE,
                                 col_na_rm = FALSE,
                                 col_title = NULL,
                                 col_title_wrap = 25,
                                 facet_labels = snakecase::to_sentence_case,
                                 facet_na_rm = FALSE,
                                 facet_ncol = NULL,
                                 facet_nrow = NULL,
                                 facet_rev = FALSE,
                                 facet_scales = "fixed",
                                 caption = NULL,
                                 caption_wrap = 80, 
                                 theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE), 
                                 model_bw = "nrd0",
                                 model_adjust = 1,
                                 model_kernel = "gaussian",
                                 model_n = 512,
                                 model_trim = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var)
  col_var <- rlang::enquo(col_var) #categorical var
  facet_var <- rlang::enquo(facet_var) #categorical var
  
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
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  #warnings
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a density plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a density plot")
  
  #logical to factor
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #reverse
  if (facet_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!facet_var, ~forcats::fct_rev(.x)))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #colour
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)
  pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    geom_density(aes(x = !!x_var, col = !!col_var, fill = !!col_var), 
                 position = "identity",
                 bw = model_bw, adjust = model_adjust, kernel = model_kernel, n = model_n, trim = model_trim,
                 size = size_line) 
  
  #x scale
  if (facet_scales %in% c("fixed", "free_y")) {
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = FALSE)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    
    plot <- plot +
      scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels, oob = scales::oob_squish)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  
  #y scale
  if (facet_scales %in% c("fixed", "free_x")) {
    
    if(rlang::as_name(col_var) == rlang::as_name(facet_var)) {
      y_var_vctr <- c(0, sv_density_max_facet(data, !!x_var, !!facet_var))      
    }
    else({
      y_var_vctr <- c(0, sv_density_max_col_facet(data, !!x_var, !!col_var, !!facet_var))      
    })
    
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = FALSE, breaks_n = y_breaks_n, zero = TRUE)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels, oob = scales::oob_squish)
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand,
                         labels = y_labels,
                         oob = scales::oob_squish)
  }
  
  #colour, titles & facetting
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")
  
  plot <- plot +
    scale_colour_manual(
      values = pal_line,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na_line,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) +
    scale_fill_manual(
      values = pal_fill,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na_fill,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) +
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
