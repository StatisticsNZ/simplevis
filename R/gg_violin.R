#' @title Violin ggplot.
#' @description Violin ggplot that is not coloured and not facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be transformed to density statistics. Required input.
#' @param x_var Unquoted categorical variable to be on the x scale (i.e. character, factor, logical). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. 
#' @param pal Character vector of hex codes. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of violins. Defaults to 0.5.
#' @param size_width Width of boxes. Defaults to 0.75.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions.
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param model_scale Per ggplot2::geom_violin, if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_violin(penguins, 
#'            x_var = species, 
#'            y_var = body_mass_g)
#' 
gg_violin <- function(data,
                      x_var,
                      y_var = NULL,
                      pal = pal_viridis_reorder(1),
                      alpha_fill = 0.2,
                      alpha_line = 1,
                      size_line = 0.5,
                      size_width = 0.75,
                      title = NULL,
                      title_wrap = 80,
                      subtitle = NULL,
                      subtitle_wrap = 80,
                      x_expand = ggplot2::waiver(),
                      x_labels = snakecase::to_sentence_case,
                      x_na_rm = FALSE,
                      x_rev = FALSE,
                      x_title = NULL,
                      x_title_wrap = 50,
                      y_balance = FALSE,
                      y_breaks_n = 5,
                      y_expand = c(0, 0),
                      y_labels = scales::label_comma(),
                      y_title = NULL,
                      y_title_wrap = 50,
                      y_zero = FALSE,
                      y_zero_line = NULL,
                      caption = NULL,
                      caption_wrap = 80,
                      theme = gg_theme(gridlines_h = TRUE),
                      model_scale = "area",
                      model_bw = "nrd0",
                      model_adjust = 1,
                      model_kernel = "gaussian",
                      model_trim = TRUE,
                      mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  
  #na's
  if (x_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)

  #warnings
  if (is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a violin")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a violin")
  
  #logical to factor
  if (is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  # #reverse
  if (x_rev == TRUE) {
    if (is.factor(x_var_vctr) | is.character(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
  #colour
  pal <- pal[1]
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)

  #fundamentals
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme
  
  plot <- plot +
    geom_violin(
      aes(x = !!x_var, y = !!y_var),
      scale = model_scale, bw = model_bw, adjust = model_adjust, kernel = model_kernel, trim = model_trim,
      fill = pal_fill,
      col = pal_line, 
      size = size_line, 
      width = size_width,
    )
    
  #x scale 
  plot <- plot +
    scale_x_discrete(expand = x_expand, labels = x_labels)
  
  #y scale
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels)
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
      theme_mobile_extra()
  }
  
  return(plot)
}

#' Violin ggplot that is coloured
#'
#' @param data An ungrouped summarised tibble or dataframe in a structure to be transformed to density statistics. Required input.
#' @param x_var Unquoted categorical variable to be on the x scale (i.e. character, factor, logical). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. 
#' @param col_var Unquoted categorical variable to colour the fill of the boxes. Required input.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of violins. Defaults to 0.5.
#' @param size_width Width of boxes. Defaults to 0.75.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75.  
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case. Use ggplot2::waiver() to keep colour labels untransformed.
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.  
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. 
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param model_scale Per ggplot2::geom_violin, if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#'
#' @return A ggplot object.
#' @export

#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_violin_col(penguins, 
#'                x_var = species, 
#'                y_var = body_mass_g, 
#'                col_var = sex, 
#'                col_na_rm = TRUE)
#' 
gg_violin_col <- function(data,
                          x_var,
                          y_var = NULL,
                          col_var,
                          pal = NULL,
                          pal_na = "#7F7F7F",
                          pal_rev = FALSE,
                          alpha_fill = 0.2,
                          alpha_line = 1,
                          size_line = 0.5,
                          size_width = 0.75,
                          title = NULL,
                          title_wrap = 80,
                          subtitle = NULL,
                          subtitle_wrap = 80,
                          x_expand = ggplot2::waiver(),
                          x_labels = snakecase::to_sentence_case,
                          x_na_rm = FALSE,
                          x_rev = FALSE,
                          x_title = NULL,
                          x_title_wrap = 50,
                          y_balance = FALSE,
                          y_expand = c(0, 0),
                          y_labels = scales::label_comma(),
                          y_breaks_n = 5,
                          y_title = NULL,
                          y_title_wrap = 50,
                          y_zero = FALSE,
                          y_zero_line = NULL,
                          col_labels = snakecase::to_sentence_case,
                          col_legend_none = FALSE,
                          col_na_rm = FALSE,
                          col_rev = FALSE,
                          col_title = NULL,
                          col_title_wrap = 25,
                          caption = NULL,
                          caption_wrap = 80,
                          theme = gg_theme(gridlines_h = TRUE),
                          model_scale = "area",
                          model_bw = "nrd0",
                          model_adjust = 1,
                          model_kernel = "gaussian",
                          model_trim = TRUE, 
                          mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  col_var <- rlang::enquo(col_var) #categorical var
  
  #na's
  if (x_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  #warnings
  if (is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a violin")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a violin")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a violin")
  
  #logical to factor
  if (is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #reverse
  if (x_rev == TRUE) {
    if (is.factor(x_var_vctr) | is.character(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
  if (col_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
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
    coord_cartesian(clip = "off") +
    theme +
    geom_violin(
      aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var), 
      scale = model_scale, bw = model_bw, adjust = model_adjust, kernel = model_kernel, trim = model_trim,
      size = size_line, 
      width = size_width
    )

  #x scale 
  plot <- plot +
    scale_x_discrete(expand = x_expand, labels = x_labels)
  
  #y scale
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels)
  })
  
  if (y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
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
      guides(col = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))
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

#' @title Violin ggplot that is facetted.
#' @description Violin ggplot that is facetted, but not coloured.
#' @param data An tibble or dataframe. Required input.
#' @param x_var Unquoted categorical variable to be on the x scale (i.e. character, factor, logical). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. 
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param pal Character vector of hex codes. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of violins. Defaults to 0.5.
#' @param size_width Width of boxes. Defaults to 0.75.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. #' 
#' @param theme A ggplot2 theme.
#' @param model_scale Per ggplot2::geom_violin, if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_violin_facet(penguins, 
#'                 x_var = sex, 
#'                 y_var = body_mass_g, 
#'                 facet_var = species, 
#'                 x_na_rm = TRUE)
#'
gg_violin_facet <- function(data,
                            x_var,
                            y_var = NULL,
                            facet_var,
                            pal = pal_viridis_reorder(1),
                            alpha_fill = 0.2,
                            alpha_line = 1,
                            size_line = 0.5,
                            size_width = 0.75,
                            title = NULL,
                            title_wrap = 80,
                            subtitle = NULL,
                            subtitle_wrap = 80,
                            x_expand = ggplot2::waiver(),
                            x_labels = snakecase::to_sentence_case,
                            x_na_rm = FALSE,
                            x_rev = FALSE,
                            x_title = NULL,
                            x_title_wrap = 50,
                            y_balance = FALSE,
                            y_breaks_n = 3,
                            y_expand = c(0, 0),
                            y_labels = scales::label_comma(),
                            y_title = NULL,
                            y_title_wrap = 50,
                            y_zero = FALSE,
                            y_zero_line = NULL,
                            facet_labels = snakecase::to_sentence_case,
                            facet_na_rm = FALSE,
                            facet_ncol = NULL,
                            facet_nrow = NULL,
                            facet_rev = FALSE,
                            facet_scales = "fixed",
                            caption = NULL,
                            caption_wrap = 80,
                            theme = gg_theme(gridlines_h = TRUE),  
                            model_scale = "area",
                            model_bw = "nrd0",
                            model_adjust = 1,
                            model_kernel = "gaussian",
                            model_trim = TRUE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var) #numeric var
  facet_var <- rlang::enquo(facet_var) #categorical var
  
  #na's
  if (x_na_rm == TRUE) {
    data <- data %>%
      dplyr::filter(!is.na(!!x_var))
  }
  if (facet_na_rm == TRUE) {
    data <- data %>%
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)  
  facet_var_vctr <- dplyr::pull(data,!!facet_var)
  
  #warnings
  if (is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a violin")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a violin")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a violin")
  
  #logical to factor
  if (is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if (is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  #reverse
  if (x_rev == TRUE) {
    if (is.factor(x_var_vctr) | is.character(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
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
    coord_cartesian(clip = "off") +
    theme +
    geom_violin(
      aes(x = !!x_var, y = !!y_var), 
      scale = model_scale, bw = model_bw, adjust = model_adjust, kernel = model_kernel, trim = model_trim,
      fill = pal_fill,
      col = pal_line, 
      size = size_line, 
      width = size_width
    )

  #x scale 
  plot <- plot +
    scale_x_discrete(expand = x_expand, labels = x_labels)
  
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
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, zero = y_zero)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels)
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, labels = y_labels)
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

#' Violin ggplot that is coloured and facetted.
#'
#' @param data An ungrouped summarised tibble or dataframe in a structure to be transformed to density statistics. Required input.
#' @param x_var Unquoted categorical variable to be on the x scale (i.e. character, factor, logical). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. 
#' @param col_var Unquoted categorical variable to colour the fill of the boxes. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of violins. Defaults to 0.5.
#' @param size_width Width of boxes. Defaults to 0.75.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. 
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.
#' @param model_scale Per ggplot2::geom_violin, if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to the number of observations. If "width", all violins have the same maximum width.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_trim TRUE or FALSE of whether to trim the tails. Defaults to FALSE.
#' 
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' penguins %>% 
#'   dplyr::mutate(year = as.character(year)) %>% 
#'   gg_violin_col_facet(x_var = year, 
#'                      y_var = body_mass_g, 
#'                      col_var = sex, 
#'                      facet_var = species, 
#'                      col_na_rm = TRUE,  
#'                      x_labels = function(x) stringr::str_sub(x, 3, 4))
#'                              
gg_violin_col_facet <- function(data,
                                x_var,
                                y_var = NULL,
                                col_var,
                                facet_var,
                                pal = NULL,
                                pal_na = "#7F7F7F",
                                pal_rev = FALSE,
                                alpha_fill = 0.2,
                                alpha_line = 1,
                                size_line = 0.5,
                                size_width = 0.75,
                                title = NULL,
                                title_wrap = 80,
                                subtitle = NULL,
                                subtitle_wrap = 80,
                                x_expand = ggplot2::waiver(),
                                x_labels = snakecase::to_sentence_case,
                                x_na_rm = FALSE,
                                x_rev = FALSE,
                                x_title = NULL,
                                x_title_wrap = 50,
                                y_balance = FALSE,
                                y_breaks_n = 3,
                                y_expand = c(0, 0),
                                y_labels = scales::label_comma(),
                                y_title = NULL,
                                y_title_wrap = 50,
                                y_zero = FALSE,
                                y_zero_line = NULL,
                                col_labels = snakecase::to_sentence_case,
                                col_legend_none = FALSE,
                                col_na_rm = FALSE,
                                col_rev = FALSE,
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
                                theme = gg_theme(gridlines_h = TRUE),
                                model_scale = "area",
                                model_bw = "nrd0",
                                model_adjust = 1,
                                model_kernel = "gaussian",
                                model_trim = TRUE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  col_var <- rlang::enquo(col_var) #categorical var
  facet_var <- rlang::enquo(facet_var) #categorical var
  
  #na's
  if (x_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
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
  if (is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a violin")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a violin")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a violin")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a violin")
  
  #logical to factor
  if (is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
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
  
  #reverse
  if (x_rev == TRUE) {
    if (is.factor(x_var_vctr) | is.character(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
  if (col_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
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
    coord_cartesian(clip = "off") +
    theme +
    geom_violin(
      aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var), 
      scale = model_scale, bw = model_bw, adjust = model_adjust, kernel = model_kernel, trim = model_trim,
      size = size_line, 
      width = size_width
    )

  #x scale 
  plot <- plot +
    scale_x_discrete(expand = x_expand, labels = x_labels)
  
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
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, breaks_n = y_breaks_n, zero = y_zero)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = y_breaks, limits = y_limits, labels = y_labels)
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, labels = y_labels)
  }
  
  if (y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
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
