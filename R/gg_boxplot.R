#' @title Boxplot ggplot.
#' @description Boxplot ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. However if stat = "identity" is selected, a list-column with min, lower, middle, upper, and max variable names.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". 
#' @param pal Character vector of hex codes. 
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_point The size of the outliers. Defaults to 1.
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_boxplot(penguins, 
#'            x_var = species, 
#'            y_var = body_mass_g)
#' 
#' plot_data <- penguins %>%
#'   group_by(species) %>%
#'   summarise(across(body_mass_g, ~ list(
#'     rlang::set_names(
#'       boxplot.stats(.x)$stats,
#'       c('min', 'lower', 'middle', 'upper', 'max')
#'     )
#'   )))
#'   
#' plot_data
#' 
#' plot_data %>% 
#'   tidyr::unnest_wider(body_mass_g)
#' 
#' gg_boxplot(plot_data, 
#'            x_var = species, 
#'            y_var = body_mass_g, 
#'            stat = "identity",
#'            y_pretty_n = 4)
#' 
gg_boxplot <- function(data,
                       x_var,
                       y_var = NULL,
                       stat = "boxplot",
                       pal = NULL,
                       width = 0.5,
                       alpha = 1,
                       size_line = 0.5,
                       size_point = 1,
                       title = NULL,
                       title_wrap = 80,
                       subtitle = NULL,
                       subtitle_wrap = 80,
                       x_balance = FALSE,
                       x_labels = NULL,
                       x_na_rm = FALSE,
                       x_pretty_n = 6,
                       x_expand = NULL,
                       x_rev = FALSE,
                       x_title = NULL,
                       x_title_wrap = 50,
                       x_zero = FALSE,
                       x_zero_line = NULL,
                       y_balance = FALSE,
                       y_expand = NULL,
                       y_gridlines_minor = FALSE,
                       y_labels = scales::comma,
                       y_pretty_n = 5,
                       y_title = NULL,
                       y_title_wrap = 50,
                       y_trans = "identity",
                       y_zero = FALSE,
                       y_zero_line = NULL,
                       caption = NULL,
                       caption_wrap = 80,
                       font_family = "",
                       font_size_title = NULL,
                       font_size_body = NULL,
                       mobile = FALSE
) {
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  
  if (x_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }

  x_var_vctr <- dplyr::pull(data, !!x_var)
  
  if(stat == "boxplot") {
    y_var_vctr <- dplyr::pull(data, !!y_var)
  } else if(stat == "identity") {
    data <- data %>% 
      tidyr::unnest_wider(!!y_var)
    
    y_var_vctr <- c(dplyr::pull(data, .data$min), dplyr::pull(data, .data$max))
  }
  
  if (stat == "boxplot" & !is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot when stat = 'boxplot'")
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }

  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  if (x_rev == TRUE) {
    if (is.factor(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
    }
    else if (is.character(x_var_vctr) | is.logical(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(factor(.x))))
    }
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme_h_gridlines(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (stat == "boxplot") {
    plot <- plot +
      geom_boxplot(
        aes(x = !!x_var, y = !!y_var, group = !!x_var),
        stat = stat,
        col = "#323232", 
        fill = pal,
        width = width,
        size = size_line, 
        alpha = alpha,
        outlier.alpha = 1, 
        outlier.size = size_point 
      )
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!x_var,
          ymin = .data$min,
          lower = .data$lower,
          middle = .data$middle,
          upper = .data$upper,
          ymax = .data$max, 
          group = !!x_var
        ),
        stat = stat,
        col = "#323232", 
        fill = pal,
        width = width,
        size = size_line, 
        alpha = alpha,
        outlier.alpha = 1, 
        outlier.size = size_point
      )
  }
  
  if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_var_vctr), max(x_var_vctr))
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(x_labels)) x_labels <- waiver()
    
    if(mobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
    }
  }
  
  if (is.numeric(x_var_vctr)) {
    plot <- plot +
      scale_x_continuous(expand = x_expand,
                         breaks = x_breaks,
                         labels = x_labels,
                         oob = scales::squish)
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  else if (lubridate::is.Date(x_var_vctr)) {
    plot <- plot +
      scale_x_date(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels
      )
  }
  else if (lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
    plot <- plot +
      scale_x_datetime(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels
      )
  }
  else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if(is.null(x_expand)) x_expand <- waiver()
    if(is.null(x_labels)) x_labels <- stringr::str_to_sentence
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  
  if(is.null(y_expand)) y_expand <- c(0, 0)  
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = y_trans, zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(
        expand = y_expand,
        breaks = y_breaks,
        limits = y_limits,
        trans = y_trans,
        labels = y_labels,
        oob = scales::rescale_none
      )
  })
  
  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (y_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.2))
  }

  if (mobile == FALSE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) 
  }
  else if (mobile == TRUE){
    plot <- plot +
      theme_mobile_extra() +
      theme(panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2)) +
      theme(panel.grid.major.y = element_blank()) +
      theme(axis.text.x = element_text(hjust = 1)) +  
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(x_title, 20),
        y = stringr::str_wrap(y_title, 30),
        caption = stringr::str_wrap(caption, 50)
      ) 
  }
  
  return(plot)
}

#' Boxplot ggplot that is coloured
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. However if stat = "identity" is selected, a list-column with min, lower, middle, upper, and max variable names.
#' @param col_var Unquoted categorical variable to colour the fill of the boxes. Required input.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". 
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE. 
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_point The size of the outliers. Defaults to 1.
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75.  
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to stringr::str_to_sentence. Use ggplot2::waiver() to keep colour labels untransformed. 
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.
#'
#' @return A ggplot object.
#' @export

#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_boxplot_col(penguins, 
#'                x_var = species, 
#'                y_var = body_mass_g, 
#'                col_var = sex)
#' 
#' plot <- gg_boxplot_col(penguins, 
#'                        x_var = species, 
#'                        y_var = body_mass_g, 
#'                        col_var = sex)
#' 
#' plotly::ggplotly(plot) %>%
#'   plotly::layout(boxmode = "group") %>%
#'   plotly_camera()
#' 
gg_boxplot_col <- function(data,
                           x_var,
                           y_var = NULL,
                           col_var,
                           stat = "boxplot",
                           pal = NULL,
                           pal_na = "#7F7F7F",
                           pal_rev = FALSE,
                           width = 0.5,
                           alpha = 1,
                           size_line = 0.5,
                           size_point = 1,
                           title = NULL,
                           title_wrap = 80,
                           subtitle = NULL,
                           subtitle_wrap = 80,
                           x_balance = FALSE,
                           x_labels = NULL,
                           x_pretty_n = 6,
                           x_expand = NULL,
                           x_na_rm = FALSE,
                           x_title = NULL,
                           x_title_wrap = 50,
                           x_zero = FALSE,
                           x_zero_line = NULL,
                           y_balance = FALSE,
                           y_expand = NULL,
                           y_gridlines_minor = FALSE,
                           y_labels = scales::comma,
                           y_pretty_n = 5,
                           x_rev = FALSE,
                           y_title = NULL,
                           y_title_wrap = 50,
                           y_trans = "identity",
                           y_zero = FALSE,
                           y_zero_line = NULL,
                           col_labels = stringr::str_to_sentence,
                           col_na_rm = FALSE,
                           col_title = NULL,
                           col_title_wrap = 25,
                           caption = NULL,
                           caption_wrap = 80,
                           font_family = "",
                           font_size_title = NULL,
                           font_size_body = NULL,
                           mobile = FALSE
) {
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  col_var <- rlang::enquo(col_var) #categorical var
  
  if (x_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }

  x_var_vctr <- dplyr::pull(data, !!x_var)
  
  if(stat == "boxplot") {
    y_var_vctr <- dplyr::pull(data, !!y_var)
  } else if(stat == "identity") {
    data <- data %>% 
      tidyr::unnest_wider(!!y_var)
    
    y_var_vctr <- c(dplyr::pull(data, .data$min), dplyr::pull(data, .data$max))
  }
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (stat == "boxplot" & !is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot when stat = 'boxplot'")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a boxplot")
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (x_rev == TRUE) {
    if (is.factor(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
    }
    else if (is.character(x_var_vctr) | is.logical(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(factor(.x))))
    }
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }

  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  data <- data %>% 
    tidyr::unite(col = "group_var",  !!x_var, !!col_var, remove = FALSE)
  
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme_h_gridlines(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (stat == "boxplot") {
    plot <- plot +
      geom_boxplot(
        aes(x = !!x_var, y = !!y_var, fill = !!col_var, group = .data$group_var),
        stat = stat,
        position = position_dodge2(preserve = "single"),
        col = "#323232", 
        width = width,
        size = size_line, 
        alpha = alpha,
        outlier.alpha = 1, 
        outlier.size = size_point 
      )
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!x_var,
          ymin = .data$min,
          lower = .data$lower,
          middle = .data$middle,
          upper = .data$upper,
          ymax = .data$max, 
          fill = !!col_var,
          group = .data$group_var
        ),
        stat = stat,
        position = position_dodge2(preserve = "single"),
        col = "#323232", 
        width = width,
        size = size_line, 
        alpha = alpha,
        outlier.alpha = 1, 
        outlier.size = size_point
      )
  }
  
  if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_var_vctr), max(x_var_vctr))
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(x_labels)) x_labels <- waiver()
    
    if(mobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
    }
  }
  
  if (is.numeric(x_var_vctr)) {
    plot <- plot +
      scale_x_continuous(expand = x_expand,
                         breaks = x_breaks,
                         labels = x_labels,
                         oob = scales::squish)
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  else if (lubridate::is.Date(x_var_vctr)) {
    plot <- plot +
      scale_x_date(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels
      )
  }
  else if (lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
    plot <- plot +
      scale_x_datetime(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels
      )
  }
  else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if(is.null(x_expand)) x_expand <- waiver()
    if(is.null(x_labels)) x_labels <- stringr::str_to_sentence
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  }  
  
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = y_trans, zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    plot <- plot +
      scale_y_continuous(
        expand = y_expand,
        breaks = y_breaks,
        limits = y_limits,
        trans = y_trans,
        labels = y_labels,
        oob = scales::rescale_none
      )
  })
  
  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (y_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.2))
  }

  if (mobile == TRUE) col_title_wrap <- 20
  
  plot <- plot +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na,
      name = stringr::str_wrap(col_title, col_title_wrap)
    ) 
  
  if (mobile == FALSE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(fill = guide_legend(byrow = TRUE)) 
  }
  else if (mobile == TRUE){
    plot <- plot +
      theme_mobile_extra() +
      theme(panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2)) +
      theme(panel.grid.major.y = element_blank()) +
      theme(axis.text.x = element_text(hjust = 1)) +  
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(x_title, 20),
        y = stringr::str_wrap(y_title, 30),
        caption = stringr::str_wrap(caption, 50)
      ) +
      guides(fill = guide_legend(ncol = 1)) 
  }
  
  return(plot)
}

#' @title Boxplot ggplot that is facetted.
#' @description Boxplot ggplot that is facetted, but not coloured.
#' @param data An tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. However if stat = "identity" is selected, a list-column with min, lower, middle, upper, and max variable names.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot".  
#' @param pal Character vector of hex codes. 
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param size_point The size of the outliers. Defaults to 1.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4. 
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
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_boxplot_facet(penguins, 
#'                  x_var = sex, 
#'                  y_var = body_mass_g, 
#'                  facet_var = species)
#'
gg_boxplot_facet <- function(data,
                             x_var,
                             y_var = NULL,
                             facet_var,
                             stat = "boxplot",
                             pal = NULL,
                             width = 0.5,
                             alpha = 1,
                             size_line = 0.5,
                             size_point = 1,
                             title = NULL,
                             title_wrap = 80,
                             subtitle = NULL,
                             subtitle_wrap = 80,
                             x_balance = FALSE,
                             x_expand = NULL,
                             x_labels = NULL,
                             x_na_rm = FALSE,
                             x_pretty_n = 3,
                             x_rev = FALSE,
                             x_title = NULL,
                             x_title_wrap = 50,
                             x_zero = FALSE,
                             x_zero_line = NULL,
                             y_balance = FALSE,
                             y_expand = NULL,
                             y_gridlines_minor = FALSE,
                             y_labels = scales::comma,
                             y_pretty_n = 4,
                             y_title = NULL,
                             y_title_wrap = 50,
                             y_trans = "identity",
                             y_zero = FALSE,
                             y_zero_line = NULL,
                             facet_labels = stringr::str_to_sentence,
                             facet_na_rm = FALSE,
                             facet_ncol = NULL,
                             facet_nrow = NULL,
                             facet_scales = "fixed",
                             caption = NULL,
                             caption_wrap = 80,
                             font_family = "",
                             font_size_title = NULL,
                             font_size_body = NULL)
{
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) 
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    
    if (x_na_rm == TRUE) {
      data <- data %>% 
        dplyr::filter(!is.na(!!x_var))
    }
    if (facet_na_rm == TRUE) {
      data <- data %>% 
        dplyr::filter(!is.na(!!facet_var))
    }
    
    x_var_vctr <- dplyr::pull(data, !!x_var) 
    
    if(stat == "boxplot") {
      y_var_vctr <- dplyr::pull(data, !!y_var)
    } else if(stat == "identity") {
      data <- data %>% 
        tidyr::unnest_wider(!!y_var)
      
      y_var_vctr <- c(dplyr::pull(data, .data$min), dplyr::pull(data, .data$max))
    }
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (stat == "boxplot" & !is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot when stat = 'boxplot'")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a boxplot")
    
    if(is.logical(x_var_vctr)) {
      data <- data %>% 
        dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
    if(is.logical(facet_var_vctr)) {
      data <- data %>% 
        dplyr::mutate(dplyr::across(!!facet_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
      
      facet_var_vctr <- dplyr::pull(data, !!facet_var)
    }
    
    if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
    if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (x_rev == TRUE) {
      if (is.factor(x_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      }
      else if (is.character(x_var_vctr) | is.logical(x_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(factor(.x))))
      }
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
    
    if (is.null(pal)) pal <- pal_viridis_reorder(1)
    else pal <- pal[1]
    
    data <- data %>% 
      tidyr::unite(col = "group_var",  !!x_var, !!facet_var, remove = FALSE)
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_h_gridlines(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    if (stat == "boxplot") {
      plot <- plot +
        geom_boxplot(
          aes(x = !!x_var, y = !!y_var, group = .data$group_var),
          stat = stat,
          col = "#323232", 
          fill = pal,
          width = width,
          size = size_line, 
          alpha = alpha,
          outlier.alpha = 1
        )
    }
    else if (stat == "identity") {
      plot <- ggplot(data) +
        coord_cartesian(clip = "off") +
        theme_h_gridlines(
          font_family = font_family,
          font_size_body = font_size_body,
          font_size_title = font_size_title
        ) +
        geom_boxplot(
          aes(
            x = !!x_var,
            ymin = .data$min,
            lower = .data$lower,
            middle = .data$middle,
            upper = .data$upper,
            ymax = .data$max, 
            group = .data$group_var
          ),
          stat = stat,
          col = "#323232", 
          fill = pal,
          width = width,
          size = size_line, 
          alpha = alpha,
          outlier.alpha = 1, 
          outlier.size = size_point
        )
    }
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
        
        x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
        x_zero <- x_zero_list[[1]]
        x_zero_line <- x_zero_list[[2]]
        
        x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = FALSE)
        x_limits <- c(min(x_var_vctr), max(x_var_vctr))
        if(is.null(x_expand)) x_expand <- c(0, 0)
        if(is.null(x_labels)) x_labels <- waiver()
      }
      
      if (is.numeric(x_var_vctr)) {
        plot <- plot +
          scale_x_continuous(expand = x_expand,
                             breaks = x_breaks,
                             labels = x_labels,
                             oob = scales::squish)
        
        if(x_zero_line == TRUE) {
          plot <- plot +
            geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
        }
      }
      else if (lubridate::is.Date(x_var_vctr)) {
        plot <- plot +
          scale_x_date(
            expand = x_expand,
            breaks = x_breaks,
            labels = x_labels
          )
      }
      else if (lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
        plot <- plot +
          scale_x_datetime(
            expand = x_expand,
            breaks = x_breaks,
            labels = x_labels
          )
      }
      else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
        if(is.null(x_expand)) x_expand <- waiver()
        if(is.null(x_labels)) x_labels <- stringr::str_to_sentence
        
        plot <- plot +
          scale_x_discrete(expand = x_expand, labels = x_labels)
      }
    }
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    if(facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_x")) {
      if (all(y_var_vctr == 0, na.rm = TRUE)) {
        plot <- plot +
          scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
      }
      else ({
        y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = y_trans, zero = y_zero)
        y_limits <- c(min(y_breaks), max(y_breaks))
        
        plot <- plot +
          scale_y_continuous(
            expand = y_expand,
            breaks = y_breaks,
            limits = y_limits,
            trans = y_trans,
            labels = y_labels,
            oob = scales::rescale_none
          )
      })
    }
    else if (facet_scales %in% c("free", "free_y")) {
      plot <- plot +
        scale_y_continuous(expand = y_expand,
                           trans = y_trans,
                           labels = y_labels,
                           oob = scales::rescale_none)
    }
    
    if(y_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
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

#' Boxplot ggplot that is coloured
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). Required input.
#' @param y_var Generally an unquoted numeric variable to be on the y scale. However if stat = "identity" is selected, a list-column with min, lower, middle, upper, and max variable names.
#' @param col_var Unquoted categorical variable to colour the fill of the boxes. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot".  
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE. 
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_point The size of the outliers. Defaults to 1.
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 75. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to stringr::str_to_sentence. Use ggplot2::waiver() to keep colour labels untransformed. 
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
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' plot_data <- penguins %>% 
#'   mutate(year = as.character(year))
#' 
#' gg_boxplot_col_facet(plot_data, 
#'                      x_var = year, 
#'                      y_var = body_mass_g, 
#'                      col_var = sex, 
#'                      facet_var = species)
#' 
#' plot <- gg_boxplot_col_facet(plot_data, 
#'                              x_var = year, 
#'                              y_var = body_mass_g, 
#'                              col_var = sex, 
#'                              facet_var = species)
#' 
#' plotly::ggplotly(plot) %>%
#'   plotly::layout(boxmode = "group") %>%
#'   plotly_camera()
#' 
gg_boxplot_col_facet <- function(data,
                                 x_var,
                                 y_var = NULL,
                                 col_var,
                                 facet_var,
                                 stat = "boxplot",
                                 pal = NULL,
                                 pal_na = "#7F7F7F",
                                 pal_rev = FALSE,
                                 width = 0.5,
                                 alpha = 1,
                                 size_line = 0.5,
                                 size_point = 1,
                                 title = NULL,
                                 title_wrap = 80,
                                 subtitle = NULL,
                                 subtitle_wrap = 80,
                                 x_balance = FALSE,
                                 x_labels = NULL,
                                 x_na_rm = FALSE,
                                 x_pretty_n = 3,
                                 x_expand = NULL,
                                 x_rev = FALSE,
                                 x_title = NULL,
                                 x_title_wrap = 50,
                                 x_zero = FALSE,
                                 x_zero_line = NULL,
                                 y_balance = FALSE,
                                 y_expand = NULL,
                                 y_gridlines_minor = FALSE,
                                 y_labels = scales::comma,
                                 y_pretty_n = 4,
                                 y_title = NULL,
                                 y_title_wrap = 50,
                                 y_trans = "identity",
                                 y_zero = FALSE,
                                 y_zero_line = NULL,
                                 col_labels = stringr::str_to_sentence,
                                 col_na_rm = FALSE,
                                 col_title = NULL,
                                 col_title_wrap = 25,
                                 facet_labels = stringr::str_to_sentence,
                                 facet_na_rm = FALSE,
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
  y_var <- rlang::enquo(y_var) #numeric var
  col_var <- rlang::enquo(col_var) #categorical var
  facet_var <- rlang::enquo(facet_var) #categorical var
  
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
  
  x_var_vctr <- dplyr::pull(data, !!x_var) 
  
  if(stat == "boxplot") {
    y_var_vctr <- dplyr::pull(data, !!y_var)
  } else if(stat == "identity") {
    data <- data %>% 
      tidyr::unnest_wider(!!y_var)
    
    y_var_vctr <- c(dplyr::pull(data, .data$min), dplyr::pull(data, .data$max))
  }
  
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (stat == "boxplot" & !is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot when stat = 'boxplot'")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a boxplot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a boxplot")
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  if(is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (x_rev == TRUE) {
    if (is.factor(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
    }
    else if (is.character(x_var_vctr) | is.logical(x_var_vctr)){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(factor(.x))))
    }
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  data <- data %>% 
    tidyr::unite(col = "group_var",  !!x_var, !!col_var, !!facet_var, remove = FALSE)
  
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme_h_gridlines(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) 
  
  if (stat == "boxplot") {
    plot <- plot +
      geom_boxplot(
        aes(x = !!x_var, y = !!y_var, fill = !!col_var, group = .data$group_var),
        stat = stat,
        col = "#323232", 
        width = width,
        size = size_line, 
        alpha = alpha,
        outlier.alpha = 1
      )
  }
  else if (stat == "identity") {
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_h_gridlines(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_boxplot(
        aes(
          x = !!x_var,
          ymin = .data$min,
          lower = .data$lower,
          middle = .data$middle,
          upper = .data$upper,
          ymax = .data$max, 
          fill = !!col_var,
          group = .data$group_var
        ),
        stat = stat,
        col = "#323232", 
        width = width,
        size = size_line, 
        alpha = alpha,
        outlier.alpha = 1, 
        outlier.size = size_point
      )
  }
  
  if (facet_scales %in% c("fixed", "free_y")) {
    if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
      
      x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
      x_zero <- x_zero_list[[1]]
      x_zero_line <- x_zero_list[[2]]
      
      x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_var_vctr), max(x_var_vctr))
      if(is.null(x_expand)) x_expand <- c(0, 0)
      if(is.null(x_labels)) x_labels <- waiver()
    }
    
    if (is.numeric(x_var_vctr)) {
      plot <- plot +
        scale_x_continuous(expand = x_expand,
                           breaks = x_breaks,
                           labels = x_labels,
                           oob = scales::squish)
      
      if(x_zero_line == TRUE) {
        plot <- plot +
          geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
      }
    }
    else if (lubridate::is.Date(x_var_vctr)) {
      plot <- plot +
        scale_x_date(
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels
        )
    }
    else if (lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
      plot <- plot +
        scale_x_datetime(
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels
        )
    }
    else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
      if(is.null(x_expand)) x_expand <- waiver()
      if(is.null(x_labels)) x_labels <- stringr::str_to_sentence
      
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = x_labels)
    }
  }
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  if(facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
  if (facet_scales %in% c("fixed", "free_x")) {
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = y_trans, zero = y_zero)
      y_limits <- c(min(y_breaks), max(y_breaks))
      
      plot <- plot +
        scale_y_continuous(
          expand = y_expand,
          breaks = y_breaks,
          limits = y_limits,
          trans = y_trans,
          labels = y_labels,
          oob = scales::rescale_none
        )
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand,
                         trans = y_trans,
                         labels = y_labels,
                         oob = scales::rescale_none)
  }
  
  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
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
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(x_title, x_title_wrap),
      y = stringr::str_wrap(y_title, y_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = as_labeller(facet_labels), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow) +
    guides(fill = guide_legend(byrow = TRUE)) 
    
    return(plot)
  }
