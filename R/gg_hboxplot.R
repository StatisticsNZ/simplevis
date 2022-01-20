#' @title Horizontal boxplot ggplot.
#' @description Horizontal boxplot ggplot that is not coloured and not facetted.
#' @param data An ungrouped summarised tibble or dataframe generally in a structure to be transformed to boxplot statistics (or alternatively in a structure of summary boxplot statistics). Required input.
#' @param x_var Unquoted numeric variable to be on the x scale for when stat = "boxplot" is selected. 
#' @param y_var Unquoted categorical variable to be on the y scale (i.e. character, factor, or logical). Required input.
#' @param pal Character vector of hex codes. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param alpha_point The opacity of the outlier points. Defaults to 1. 
#' @param size_line The size of the outlines of boxplots.
#' @param size_point The size of the outlier points. Defaults to 1.5.
#' @param size_width Width of boxes. Defaults to 0.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 60. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 60. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na_rm TRUE or FALSE of whether to include y_var NA values. Defaults to FALSE.
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 75. 
#' @param theme A ggplot2 theme.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot".
#' @param xmin_var Unquoted numeric variable for minimum of whisker on the x scale for when stat = "identity" is selected. 
#' @param xlower_var Unquoted numeric variable for minimum of box on the x scale for when stat = "identity" is selected. 
#' @param xmiddle_var Unquoted numeric variable for middle of box on the x scale for when stat = "identity" is selected. 
#' @param xupper_var Unquoted numeric variable for maximum of box on the x scale for when stat = "identity" is selected. 
#' @param xmax_var Unquoted numeric variable for maximum of whisker on the x scale for when stat = "identity" is selected. 
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(dplyr)
#' library(palmerpenguins)
#' 
#' gg_hboxplot(penguins, 
#'         x_var = body_mass_g, 
#'         y_var = species)
#' 
#' plot_data <- penguins %>%
#'   group_by(species) %>%
#'   summarise_boxplot_stats(body_mass_g)
#' 
#' outliers <- penguins %>% 
#'   group_by(species) %>% 
#'   summarise_boxplot_outliers(body_mass_g)
#' 
#' gg_hboxplot(plot_data,
#'             xmin_var = min,
#'             xlower_var = lower,
#'             xmiddle_var = middle,
#'             xupper_var = upper,
#'             xmax_var = max,
#'             y_var = species,
#'             stat = "identity",
#'             x_title = "Body mass g",
#'             x_breaks_n = 4) +
#'   ggplot2::geom_point(ggplot2::aes(x = species, y = body_mass_g), 
#'             size = 0.75, col = pal_viridis_reorder(1), 
#'             data = outliers)
#'   
gg_hboxplot <- function(data,
                    x_var = NULL,
                    y_var,
                    pal = pal_viridis_reorder(1),
                    alpha_fill = 0.2,
                    alpha_line = 1,
                    alpha_point = 1,
                    size_line = 0.5,
                    size_point = 1.5,
                    size_width = 0.5,
                    title = NULL,
                    title_wrap = 75,
                    subtitle = NULL,
                    subtitle_wrap = 75,
                    x_balance = FALSE,
                    x_breaks_n = 5,
                    x_expand = c(0, 0),
                    x_labels = scales::label_comma(),
                    x_title = NULL,
                    x_title_wrap = 50,
                    x_zero = FALSE,
                    x_zero_line = NULL,
                    y_expand = ggplot2::waiver(),
                    y_labels = snakecase::to_sentence_case,
                    y_na_rm = FALSE,
                    y_rev = FALSE,
                    y_title = NULL,
                    y_title_wrap = 50,
                    caption = NULL,
                    caption_wrap = 75,
                    theme = gg_theme(gridlines_v = TRUE),
                    stat = "boxplot",
                    xmin_var = NULL,
                    xlower_var = NULL,
                    xmiddle_var = NULL,
                    xupper_var = NULL,
                    xmax_var = NULL,
                    mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote & vectors
  y_var <- rlang::enquo(y_var) 
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  if (stat == "boxplot") {
    x_var <- rlang::enquo(x_var) #numeric var  
    x_var_vctr <- dplyr::pull(data, !!x_var)
    
  }
  else if (stat == "identity") {
    xmin_var <- rlang::enquo(xmin_var) #numeric var
    xlower_var <- rlang::enquo(xlower_var) #numeric var
    xmiddle_var <- rlang::enquo(xmiddle_var) #numeric var
    xupper_var <- rlang::enquo(xupper_var) #numeric var
    xmax_var <- rlang::enquo(xmax_var) #numeric var
    
    x_var_vctr <- c(min(dplyr::pull(data, !!xmin_var), na.rm = TRUE), max(dplyr::pull(data, !!xmax_var), na.rm = TRUE))
  }
  
  #na's
  if (y_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  
  #warnings
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a horizontal boxplot")
  if (stat == "boxplot" & !is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal boxplot when stat = 'boxplot'")
  
  #logical to factor
  if (is.logical(y_var_vctr)) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!y_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) {
    if (stat == "boxplot") x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
    else if (stat == "identity") x_title <- ""
  }
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  #reverse
  if (y_rev == FALSE) {
    if (is.factor(y_var_vctr) | is.character(y_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
  }

  #colour
  pal <- pal[1]
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)
  pal_point <- scales::alpha(pal, alpha = alpha_point)
  
  #fundamentals
  plot <- ggplot(data) +
    coord_flip(clip = "off") +
    theme 
  
  if (stat == "boxplot") {
    plot <- plot +
      geom_boxplot(
        aes(x = !!y_var, y = !!x_var),
        stat = stat,
        fill = pal_fill,
        col = pal_line, 
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!y_var,
          ymin = !!xmin_var,
          lower = !!xlower_var,
          middle = !!xmiddle_var,
          upper = !!xupper_var,
          ymax = !!xmax_var 
        ),
        stat = stat,
        fill = pal_fill,
        col = pal_line, 
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }

  #y scale 
  plot <- plot +
    scale_x_discrete(expand = y_expand, labels = y_labels)

  #x scale
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  if (all(x_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
  }
  else ({
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
  })
  
  if (x_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #titles
  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(y_title, y_title_wrap),
        y = stringr::str_wrap(x_title, x_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      )
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(y_title, 20),
        y = stringr::str_wrap(x_title, 30),
        caption = stringr::str_wrap(caption, 50)
      ) +
      theme_mobile_extra()
  }
  
  return(plot)
}

#' @title Horizontal boxplot ggplot that is coloured.
#' @description Horizontal boxplot ggplot that is coloured, but not facetted.
#' @param data An ungrouped summarised tibble or dataframe generally in a structure to be transformed to boxplot statistics (or alternatively in a structure of summary boxplot statistics). Required input.
#' @param x_var Unquoted numeric variable to be on the x scale for when stat = "boxplot" is selected. 
#' @param y_var Unquoted categorical variable to be on the y scale (i.e. character, factor, or logical). Required input.
#' @param col_var Unquoted categorical or numeric variable to colour the boxplots. Required input.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param alpha_point The opacity of the outlier points. Defaults to 1. 
#' @param size_line The size of the outlines of boxplots.
#' @param size_point The size of the outlier points. Defaults to 1.5.
#' @param size_width Width of boxes. Defaults to 0.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 60. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 60. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na_rm TRUE or FALSE of whether to include y_var NA values. Defaults to FALSE.
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case for categorical colour variables and scales::number for numeric colour variables. Use ggplot2::waiver() to keep colour labels untransformed.   
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. 
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 75. 
#' @param theme A ggplot2 theme.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot".
#' @param xmin_var Unquoted numeric variable for minimum of whisker on the x scale for when stat = "identity" is selected. 
#' @param xlower_var Unquoted numeric variable for minimum of box on the x scale for when stat = "identity" is selected. 
#' @param xmiddle_var Unquoted numeric variable for middle of box on the x scale for when stat = "identity" is selected. 
#' @param xupper_var Unquoted numeric variable for maximum of box on the x scale for when stat = "identity" is selected. 
#' @param xmax_var Unquoted numeric variable for maximum of whisker on the x scale for when stat = "identity" is selected. 
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(dplyr)
#' library(palmerpenguins)
#' 
#' gg_hboxplot_col(penguins, 
#'             x_var = body_mass_g, 
#'             y_var = species, 
#'             col_var = sex, 
#'             col_na_rm = TRUE)
#'             
#' #For ggplotly, pipe in plotly::layout(boxmode = "group") layer
#' 
#' plot_data <- penguins %>%
#'   group_by(species, sex) %>%
#'   summarise_boxplot_stats(body_mass_g)
#'
#' outliers <- penguins %>% 
#'   group_by(species, sex) %>% 
#'   summarise_boxplot_outliers(body_mass_g)
#' 
#' size_width <- 0.5
#' 
#' gg_hboxplot_col(plot_data,
#'                xmin_var = min,
#'                xlower_var = lower,
#'                xmiddle_var = middle,
#'                xupper_var = upper,
#'                xmax_var = max,
#'                y_var = species,
#'                col_var = sex,
#'                size_width = size_width,
#'                stat = "identity",
#'                x_title = "Body mass g",
#'                x_breaks_n = 4, 
#'                col_na_rm = TRUE) +
#'                ggplot2::geom_point(ggplot2::aes(x = species, y = body_mass_g, col = sex), 
#'                      size = 0.75, 
#'                      position = ggplot2::position_dodge(width = size_width),
#'                      data = outliers)
#'                      
gg_hboxplot_col <- function(data,
                        x_var = NULL,
                        y_var,
                        col_var,
                        pal = NULL,
                        pal_na = "#7F7F7F",
                        pal_rev = FALSE,
                        alpha_fill = 0.2,
                        alpha_line = 1,
                        alpha_point = 1,
                        size_line = 0.5,
                        size_point = 1.5,
                        size_width = 0.5,
                        title = NULL,
                        title_wrap = 75,
                        subtitle = NULL,
                        subtitle_wrap = 75,
                        x_balance = FALSE,
                        x_breaks_n = 5,
                        x_expand = c(0, 0),
                        x_labels = scales::label_comma(),
                        x_title = NULL,
                        x_title_wrap = 50,
                        x_zero = FALSE,
                        x_zero_line = NULL,
                        y_expand = ggplot2::waiver(),
                        y_labels = snakecase::to_sentence_case,
                        y_na_rm = FALSE,
                        y_rev = FALSE,
                        y_title = NULL,
                        y_title_wrap = 50,
                        col_labels = stringr::str_to_sentence,
                        col_legend_none = FALSE,
                        col_na_rm = FALSE,
                        col_rev = FALSE,
                        col_title = NULL,
                        col_title_wrap = 25,
                        caption = NULL,
                        caption_wrap = 75,
                        theme = gg_theme(gridlines_v = TRUE),
                        stat = "boxplot",
                        xmin_var = NULL,
                        xlower_var = NULL,
                        xmiddle_var = NULL,
                        xupper_var = NULL,
                        xmax_var = NULL,
                        mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote & vectors
  y_var <- rlang::enquo(y_var) 
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  col_var <- rlang::enquo(col_var) #categorical var
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (stat == "boxplot") {
    x_var <- rlang::enquo(x_var) #numeric var  
    x_var_vctr <- dplyr::pull(data, !!x_var)
    
  }
  else if (stat == "identity") {
    xmin_var <- rlang::enquo(xmin_var) #numeric var
    xlower_var <- rlang::enquo(xlower_var) #numeric var
    xmiddle_var <- rlang::enquo(xmiddle_var) #numeric var
    xupper_var <- rlang::enquo(xupper_var) #numeric var
    xmax_var <- rlang::enquo(xmax_var) #numeric var
    
    x_var_vctr <- c(min(dplyr::pull(data, !!xmin_var), na.rm = TRUE), max(dplyr::pull(data, !!xmax_var), na.rm = TRUE))
  }

  #na's
  if (y_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }

  #warnings
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a horizontal boxplot")
  if (stat == "boxplot" & !is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal boxplot when stat = 'boxplot'")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal boxplot")
  
  #logical to factor
  if (is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) {
    if (stat == "boxplot") x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
    else if (stat == "identity") x_title <- ""
  }
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #reverse
  if (y_rev == FALSE) {
    if (is.factor(y_var_vctr) | is.character(y_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
  }
  
  if (col_rev == FALSE) {
    if (is.factor(col_var_vctr) | is.character(col_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))

      col_var_vctr <- dplyr::pull(data, !!col_var)
    }
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
    coord_flip(clip = "off") +
    theme
  
  if (stat == "boxplot") {
    plot <- plot +
      geom_boxplot(
        aes(x = !!y_var, y = !!x_var, col = !!col_var, fill = !!col_var),
        position = position_dodge2(preserve = "single"),
        stat = stat,
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!y_var,
          ymin = !!xmin_var,
          lower = !!xlower_var,
          middle = !!xmiddle_var,
          upper = !!xupper_var,
          ymax = !!xmax_var, 
          col = !!col_var, 
          fill = !!col_var
        ),
        position = position_dodge2(preserve = "single"),
        stat = stat,
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }

  #y scale 
  plot <- plot +
    scale_x_discrete(expand = y_expand, labels = y_labels)

  #x scale
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  if (all(x_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
  }
  else ({
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    
    plot <- plot +
      scale_y_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
  })
  
  if (x_zero_line == TRUE) {
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
  
  if (col_legend_none == FALSE) {
    if (mobile == FALSE) {
      plot <- plot +
        guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE))
    }
    else if (mobile == TRUE) {
      plot <- plot +
        guides(col = guide_legend(ncol = 1, reverse = TRUE), fill = guide_legend(ncol = 1, reverse = TRUE))
    }
  }
  
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")
  
  #titles
  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(y_title, y_title_wrap),
        y = stringr::str_wrap(x_title, x_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) 
  }
  else if (mobile == TRUE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(y_title, 20),
        y = stringr::str_wrap(x_title, 30),
        caption = stringr::str_wrap(caption, 50)
      ) +
      theme_mobile_extra()
  }
  
  return(plot)
}

#' @title Horizontal boxplot ggplot that is facetted.
#' @description Horizontal boxplot ggplot that is facetted, but not coloured.
#' @param data An ungrouped summarised tibble or dataframe generally in a structure to be transformed to boxplot statistics (or alternatively in a structure of summary boxplot statistics). Required input.
#' @param x_var Unquoted numeric variable to be on the x scale for when stat = "boxplot" is selected. 
#' @param y_var Unquoted categorical variable to be on the y scale (i.e. character, factor, or logical). Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param pal Character vector of hex codes. 
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param alpha_point The opacity of the outlier points. Defaults to 1. 
#' @param size_line The size of the outlines of boxplots. 
#' @param size_point The size of the outlier points. Defaults to 1.5.
#' @param size_width Width of boxes. Defaults to 0.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 60. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 60. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na_rm TRUE or FALSE of whether to include y_var NA values. Defaults to FALSE.
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use ggplot2::waiver() to keep facet labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 75. 
#' @param theme A ggplot2 theme.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot".
#' @param xmin_var Unquoted numeric variable for minimum of whisker on the x scale for when stat = "identity" is selected. 
#' @param xlower_var Unquoted numeric variable for minimum of box on the x scale for when stat = "identity" is selected. 
#' @param xmiddle_var Unquoted numeric variable for middle of box on the x scale for when stat = "identity" is selected. 
#' @param xupper_var Unquoted numeric variable for maximum of box on the x scale for when stat = "identity" is selected. 
#' @param xmax_var Unquoted numeric variable for maximum of whisker on the x scale for when stat = "identity" is selected. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_hboxplot_facet(penguins, 
#'               x_var = body_mass_g, 
#'               y_var = sex, 
#'               facet_var = species, 
#'               y_na_rm = TRUE)
#'
gg_hboxplot_facet <- function(data,
                          x_var = NULL,
                          y_var,
                          facet_var,
                          pal = pal_viridis_reorder(1),
                          alpha_fill = 0.2,
                          alpha_line = 1,
                          alpha_point = 1,
                          size_line = 0.5,
                          size_point = 1.5,
                          size_width = 0.5,
                          title = NULL,
                          title_wrap = 75,
                          subtitle = NULL,
                          subtitle_wrap = 75,
                          x_balance = FALSE,
                          x_breaks_n = 2,
                          x_expand = c(0, 0),
                          x_labels = scales::label_comma(),
                          x_title = NULL,
                          x_title_wrap = 50,
                          x_zero = FALSE,
                          x_zero_line = NULL,
                          y_expand = ggplot2::waiver(),
                          y_labels = snakecase::to_sentence_case,
                          y_na_rm = FALSE,
                          y_rev = FALSE,
                          y_title = NULL,
                          y_title_wrap = 50,
                          facet_labels = snakecase::to_sentence_case,
                          facet_na_rm = FALSE,
                          facet_ncol = NULL,
                          facet_nrow = NULL,
                          facet_rev = FALSE,
                          facet_scales = "fixed",
                          caption = NULL,
                          caption_wrap = 75,
                          theme = gg_theme(gridlines_v = TRUE),
                          stat = "boxplot", 
                          xmin_var = NULL,
                          xlower_var = NULL,
                          xmiddle_var = NULL,
                          xupper_var = NULL,
                          xmax_var = NULL) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote & vectors
  y_var <- rlang::enquo(y_var) 
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  facet_var <- rlang::enquo(facet_var) #categorical var
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (stat == "boxplot") {
    x_var <- rlang::enquo(x_var) #numeric var  
    x_var_vctr <- dplyr::pull(data, !!x_var)
    
  }
  else if (stat == "identity") {
    xmin_var <- rlang::enquo(xmin_var) #numeric var
    xlower_var <- rlang::enquo(xlower_var) #numeric var
    xmiddle_var <- rlang::enquo(xmiddle_var) #numeric var
    xupper_var <- rlang::enquo(xupper_var) #numeric var
    xmax_var <- rlang::enquo(xmax_var) #numeric var
    
    x_var_vctr <- c(min(dplyr::pull(data, !!xmin_var), na.rm = TRUE), max(dplyr::pull(data, !!xmax_var), na.rm = TRUE))
  }

  #na's
  if (y_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }

  #warnings
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a horizontal boxplot")
  if (stat == "boxplot" & !is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal boxplot when stat = 'boxplot'")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal boxplot")
  
  #logical to factor
  if (is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if (is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) {
    if (stat == "boxplot") x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
    else if (stat == "identity") x_title <- ""
  }
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  #reverse
  if (y_rev == FALSE) {
    if (is.factor(y_var_vctr) | is.character(y_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
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
  pal_point <- scales::alpha(pal, alpha = alpha_point)
  
  #fundamentals
  plot <- ggplot(data) +
    coord_flip(clip = "off") +
    theme   
  
  if (stat == "boxplot") {
    plot <- plot +
      geom_boxplot(
        aes(x = !!y_var, y = !!x_var),
        stat = stat,
        fill = pal_fill,
        col = pal_line, 
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!y_var,
          ymin = !!xmin_var,
          lower = !!xlower_var,
          middle = !!xmiddle_var,
          upper = !!xupper_var,
          ymax = !!xmax_var 
        ),
        stat = stat,
        fill = pal_fill,
        col = pal_line, 
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }

  #y scale
  plot <- plot +
    scale_x_discrete(expand = y_expand, labels = y_labels)

  #x scale
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  if (facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  if (facet_scales %in% c("fixed", "free_y")) {
    if (all(x_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
    }
    else ({
      x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_breaks), max(x_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
    })
  }
  else if (facet_scales %in% c("free", "free_x")) {
    plot <- plot +
      scale_y_continuous(expand = x_expand, labels = x_labels)
  }
  
  if (x_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  #titles & facetting
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(y_title, y_title_wrap),
      y = stringr::str_wrap(x_title, x_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
  
  return(plot)
}

#' @title Horizontal boxplot ggplot that is coloured and facetted.
#' @description Horizontal boxplot ggplot that is coloured and facetted.
#' @param data An ungrouped summarised tibble or dataframe generally in a structure to be transformed to boxplot statistics (or alternatively in a structure of summary boxplot statistics). Required input.
#' @param x_var Unquoted numeric variable to be on the x scale for when stat = "boxplot" is selected. 
#' @param y_var Unquoted categorical variable to be on the y scale (i.e. character, factor, or logical). Required input.
#' @param col_var Unquoted categorical or numeric variable to colour the boxplots. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev TRUE or FALSE of whether to reverse the pal.
#' @param alpha_fill The opacity of the fill. Defaults to 0.2. 
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param alpha_point The opacity of the outlier points. Defaults to 1. 
#' @param size_width Width of boxes. Defaults to 0.5.
#' @param size_line The size of the outlines of boxplots. 
#' @param size_point The size of the outlier points. Defaults to 1.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 60. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 60. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na_rm TRUE or FALSE of whether to include y_var NA values. Defaults to FALSE.
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case for categorical colour variables and scales::number for numeric colour variables. Use ggplot2::waiver() to keep colour labels untransformed.   
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
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 75. 
#' @param theme A ggplot2 theme.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot".
#' @param xmin_var Unquoted numeric variable for minimum of whisker on the x scale for when stat = "identity" is selected. 
#' @param xlower_var Unquoted numeric variable for minimum of box on the x scale for when stat = "identity" is selected. 
#' @param xmiddle_var Unquoted numeric variable for middle of box on the x scale for when stat = "identity" is selected. 
#' @param xupper_var Unquoted numeric variable for maximum of box on the x scale for when stat = "identity" is selected. 
#' @param xmax_var Unquoted numeric variable for maximum of whisker on the x scale for when stat = "identity" is selected. 
#' 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' penguins %>%
#'   dplyr::mutate(year = as.character(year)) %>%
#'   gg_hboxplot_col_facet(x_var = body_mass_g,
#'                         y_var = year,
#'                         col_var = sex,
#'                         facet_var = species,
#'                         col_na_rm = TRUE)
#' 
#' #For ggplotly, pipe in plotly::layout(boxmode = "group") layer
#' 
gg_hboxplot_col_facet <- function(data,
                              x_var = NULL,
                              y_var,
                              col_var,
                              facet_var,
                              pal = NULL,
                              pal_na = "#7F7F7F",
                              pal_rev = FALSE,
                              alpha_fill = 0.2,
                              alpha_line = 1,
                              alpha_point = 1,
                              size_line = 0.5,
                              size_point = 1.5,
                              size_width = 0.5,
                              title = NULL,
                              title_wrap = 75,
                              subtitle = NULL,
                              subtitle_wrap = 75,
                              x_breaks_n = 2,
                              x_balance = FALSE,
                              x_expand = c(0, 0),
                              x_labels = scales::label_comma(),
                              x_title = NULL,
                              x_title_wrap = 50,
                              x_zero = FALSE,
                              x_zero_line = NULL,
                              y_expand = ggplot2::waiver(),
                              y_labels = snakecase::to_sentence_case,
                              y_na_rm = FALSE,
                              y_rev = FALSE,
                              y_title = NULL,
                              y_title_wrap = 50,
                              col_labels = stringr::str_to_sentence,
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
                              caption_wrap = 75,
                              theme = gg_theme(gridlines_v = TRUE), 
                              stat = "boxplot", 
                              xmin_var = NULL,
                              xlower_var = NULL,
                              xmiddle_var = NULL,
                              xupper_var = NULL,
                              xmax_var = NULL) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote & vectors
  y_var <- rlang::enquo(y_var) 
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  col_var <- rlang::enquo(col_var) #categorical var
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  facet_var <- rlang::enquo(facet_var) #categorical var
  facet_var_vctr <- dplyr::pull(data, !!facet_var)

  if (stat == "boxplot") {
    x_var <- rlang::enquo(x_var) #numeric var  
    x_var_vctr <- dplyr::pull(data, !!x_var)
    
  }
  else if (stat == "identity") {
    xmin_var <- rlang::enquo(xmin_var) #numeric var
    xlower_var <- rlang::enquo(xlower_var) #numeric var
    xmiddle_var <- rlang::enquo(xmiddle_var) #numeric var
    xupper_var <- rlang::enquo(xupper_var) #numeric var
    xmax_var <- rlang::enquo(xmax_var) #numeric var
    
    x_var_vctr <- c(min(dplyr::pull(data, !!xmin_var), na.rm = TRUE), max(dplyr::pull(data, !!xmax_var), na.rm = TRUE))
  }
  
  #na's
  if (y_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #warnings
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a horizontal boxplot")
  if (stat == "boxplot" & !is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal boxplot when stat = 'boxplot'")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal boxplot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal boxplot")

  #logical to factor
  if (is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  if (is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) {
    if (stat == "boxplot") x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
    else if (stat == "identity") x_title <- ""
  }
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #reverse
  if (y_rev == FALSE) {
    if (is.factor(y_var_vctr) | is.character(y_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
  }
  
  if (col_rev == FALSE) {
    if (is.factor(col_var_vctr) | is.character(col_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
      
      col_var_vctr <- dplyr::pull(data, !!col_var)
    }
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
    coord_flip(clip = "off") +
    theme 
  
  if (stat == "boxplot") {
    plot <- plot +
      geom_boxplot(
        aes(x = !!y_var, y = !!x_var, col = !!col_var, fill = !!col_var), 
        position = position_dodge2(preserve = "single"),
        stat = stat,
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!y_var,
          ymin = !!xmin_var,
          lower = !!xlower_var,
          middle = !!xmiddle_var,
          upper = !!xupper_var,
          ymax = !!xmax_var, 
          col = !!col_var, 
          fill = !!col_var
        ),
        position = position_dodge2(preserve = "single"),
        stat = stat,
        size = size_line, 
        width = size_width,
        outlier.alpha = alpha_point,
        outlier.size = size_point
      )
  }

  #y scale
  plot <- plot +
    scale_x_discrete(expand = y_expand, labels = y_labels)

  #x scale
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  if (facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  if (facet_scales %in% c("fixed", "free_y")) {
    if (all(x_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
    }
    else ({
      x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_breaks), max(x_breaks))
      
      plot <- plot +
        scale_y_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
    })
  }
  else if (facet_scales %in% c("free", "free_x")) {
    plot <- plot +
      scale_y_continuous(expand = x_expand, labels = x_labels)
  }
  
  if (x_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (col_legend_none == FALSE) {
    plot <- plot +
      guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE))
  } 
  
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")
  
  #colour, titles & facetting
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
      x = stringr::str_wrap(y_title, y_title_wrap),
      y = stringr::str_wrap(x_title, x_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
  
  return(plot)
}