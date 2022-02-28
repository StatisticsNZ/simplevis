#' @title Point ggplot.
#' @description Point ggplot that is not coloured and not facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be plotted untransformed. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or POSIXt). Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param alpha_point The opacity of the points. 
#' @param size_point Size of points. Defaults to 1.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_jitter Amount of horizontal jitter to be added in positive and negative directions. Defaults to 0. See ggplot2::position_jitter for further information.
#' @param x_labels A function or named vector to modify x scale labels. Use function(x) x to keep labels untransformed.
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_jitter Amount of vertical jitter to be added in positive and negative directions. Defaults to 0.See ggplot2::position_jitter for further information. 
#' @param y_labels A function or named vector to modify y scale labels. Use function(x) x to keep labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80.
#' @param theme A ggplot2 theme.  
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_point(penguins, 
#'          x_var = bill_length_mm, 
#'          y_var = body_mass_g)
#' 
gg_point <- function(data,
                     x_var,
                     y_var,
                     text_var = NULL,
                     pal = pal_viridis_reorder(1),
                     alpha_point = 1,
                     size_point = 1.5,
                     title = NULL,
                     title_wrap = 80,
                     subtitle = NULL,
                     subtitle_wrap = 80,
                     x_balance = FALSE,
                     x_breaks_n = 5,
                     x_expand = NULL,
                     x_jitter = 0,
                     x_labels = NULL,
                     x_rev = FALSE,
                     x_title = NULL,
                     x_title_wrap = 50,
                     x_zero = FALSE,
                     x_zero_line = NULL,
                     y_balance = FALSE,
                     y_breaks_n = 5,
                     y_expand = c(0, 0),
                     y_jitter = 0,
                     y_labels = scales::label_comma(),
                     y_title = NULL,
                     y_title_wrap = 50,
                     y_zero = FALSE,
                     y_zero_line = NULL,
                     caption = NULL,
                     caption_wrap = 80,
                     theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE),
                     mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  #na's
  data <- data %>% 
    dplyr::filter(!is.na(!!x_var), !is.na(!!y_var))
  
  #vectors
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  #warnings
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
  
  #logical to factor
  if (is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
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
  
  #colour
  pal <- pal[1]
  pal_point <- scales::alpha(pal, alpha = alpha_point)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_point(aes(!!x_var, !!y_var, text = !!text_var), 
               col = pal_point, 
               size = size_point, 
               position = position_jitter(width = x_jitter, height = y_jitter))
  
  #x scale
  if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr)) {
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    
    if (is.null(x_labels)) {
      if (is.numeric(x_var_vctr)) x_labels <- scales::label_comma()
      else if (lubridate::is.Date(x_var_vctr)) x_labels <- scales::label_date()
      else x_labels <- waiver()
    }
  }
  
  if (is.numeric(x_var_vctr)) {
    if (mobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
    }
    
    plot <- plot +
      scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  else if (lubridate::is.Date(x_var_vctr)) {
    plot <- plot +
      scale_x_date(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
  }
  else if (lubridate::is.POSIXt(x_var_vctr)) {
    plot <- plot +
      scale_x_datetime(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
  }
  else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if (is.null(x_expand)) x_expand <- waiver()
    if (is.null(x_labels)) x_labels <- snakecase::to_sentence_case
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
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
      theme_mobile_extra() #extra mobile theme components
  }
  
  return(plot)
}

#' @title Point ggplot that is coloured.
#' @description Point ggplot that is coloured, but not facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be plotted untransformed. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or POSIXt). Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_point The opacity of the points. 
#' @param size_point Size of points. Defaults to 1.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 75. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_jitter Amount of horizontal jitter to be added in positive and negative directions. Defaults to 0. See ggplot2::position_jitter for further information.
#' @param x_labels A function or named vector to modify x scale labels. Use function(x) x to keep labels untransformed.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_jitter Amount of vertical jitter to be added in positive and negative directions. Defaults to 0.See ggplot2::position_jitter for further information. 
#' @param y_labels A function or named vector to modify y scale labels. Use function(x) x to keep labels untransformed.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_breaks_n For a numeric colour variable. If "bin" col_method, the intervals on the colour scale for the pretty algorithm to aim for. If "quantile" col_method, the number of equal quantiles. Defaults to 4.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles.
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_labels A function or named vector to modify the colour scale labels. Defaults to stringr::str_to_sentence if categorical, and scales::label_comma() if numeric.    
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_method The method of colouring features, either "bin", "quantile", "continuous", or "category." If numeric, defaults to "bin".
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.  
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_point_col(penguins, 
#'              x_var = bill_length_mm, 
#'              y_var = body_mass_g, 
#'              col_var = species)
#' 
gg_point_col <- function(data,
                         x_var,
                         y_var,
                         col_var,
                         text_var = NULL,
                         pal = NULL,
                         pal_na = "#7F7F7F",
                         pal_rev = FALSE,
                         alpha_point = 1,
                         size_point = 1.5,
                         title = NULL,
                         title_wrap = 80,
                         subtitle = NULL,
                         subtitle_wrap = 80,
                         x_balance = FALSE,
                         x_breaks_n = 5,
                         x_expand = NULL,
                         x_jitter = 0,
                         x_labels = NULL,
                         x_rev = FALSE,
                         x_title = NULL,
                         x_title_wrap = 50,
                         x_zero = FALSE,
                         x_zero_line = NULL,
                         y_balance = FALSE,
                         y_breaks_n = 5,
                         y_expand = c(0, 0),
                         y_jitter = 0,
                         y_labels = scales::label_comma(),
                         y_title = NULL,
                         y_title_wrap = 50,
                         y_zero = FALSE,
                         y_zero_line = NULL,
                         col_breaks_n = 4,
                         col_cuts = NULL,
                         col_labels = NULL,
                         col_legend_none = FALSE,
                         col_method = NULL,
                         col_intervals_right = TRUE,
                         col_na_rm = FALSE,
                         col_title = NULL,
                         col_title_wrap = 25,
                         caption = NULL,
                         caption_wrap = 80,
                         theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE),
                         mobile = FALSE){
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) 
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
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
  
  if (!is.null(col_method)) {
    if (!col_method %in% c("continuous", "bin", "quantile", "category")) stop("Please use a colour method of 'continuous', 'bin', 'quantile' or 'category'")
  }
  
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
  
  ##titles sentence case
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
  
  #colour
  if (mobile == TRUE) col_title_wrap <- 20
  
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
                          ~ kimisc::cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE, format_fun = col_labels)))
        
        col_labels <- sv_interval_labels_chr
      }
      else {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ kimisc::cut_format(.x, col_cuts,
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
  
  pal_point <- scales::alpha(pal, alpha = alpha_point)
  pal_na_point <- scales::alpha(pal_na, alpha = alpha_point)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_point(aes(x = !!x_var, y = !!y_var, col = !!col_var, text = !!text_var), 
               size = size_point, 
               position = position_jitter(width = x_jitter, height = y_jitter))
  
  #x scale
  if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr)) {
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if (is.null(x_expand)) x_expand <- c(0, 0)
    
    if (is.null(x_labels)) {
      if (is.numeric(x_var_vctr)) x_labels <- scales::label_comma()
      else if (lubridate::is.Date(x_var_vctr)) x_labels <- scales::label_date()
      else x_labels <- waiver()
    }
  }
  
  if (is.numeric(x_var_vctr)) {
    if (mobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
    }
    
    plot <- plot +
      scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
    
    if (x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  else if (lubridate::is.Date(x_var_vctr)) {
    plot <- plot +
      scale_x_date(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
  }
  else if (lubridate::is.POSIXt(x_var_vctr)) {
    plot <- plot +
      scale_x_datetime(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
  }
  else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if (is.null(x_expand)) x_expand <- waiver()
    if (is.null(x_labels)) x_labels <- snakecase::to_sentence_case
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
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
  if (col_method == "continuous") {
    plot <- plot +
      scale_colour_gradientn(
        colors = pal_point,
        labels = col_labels,
        breaks = col_cuts,
        na.value = pal_na_point,
        name = stringr::str_wrap(col_title, col_title_wrap)) 
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    plot <- plot +
      scale_colour_manual(
        values = pal_point,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na_point,
        name = stringr::str_wrap(col_title, col_title_wrap)
      ) 
    
    if (mobile == TRUE) {
      plot <- plot +
        guides(col = guide_legend(ncol = 1))
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
      )  +
      theme_mobile_extra() #extra mobile theme components
  }
  
  return(plot)
}

#' @title Point ggplot that is facetted.
#' @description Point ggplot that is facetted, but not coloured.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be plotted untransformed. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or POSIXt). Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param alpha_point The opacity of the points. 
#' @param size_point Size of points. Defaults to 1.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_jitter Amount of horizontal jitter to be added in positive and negative directions. Defaults to 0. See ggplot2::position_jitter for further information.
#' @param x_labels A function or named vector to modify x scale labels. Use function(x) x to keep labels untransformed.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_jitter Amount of vertical jitter to be added in positive and negative directions. Defaults to 0.See ggplot2::position_jitter for further information. 
#' @param y_labels A function or named vector to modify y scale labels. Use function(x) x to keep labels untransformed.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use function(x) x to keep labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.  
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_point_facet(penguins, 
#'                x_var = bill_length_mm, 
#'                y_var = body_mass_g, 
#'                facet_var = species)
#' 
gg_point_facet <- function(data,
                           x_var,
                           y_var,
                           facet_var,
                           text_var = NULL,
                           pal = pal_viridis_reorder(1),
                           alpha_point = 1,
                           size_point = 1.5,
                           title = NULL,
                           title_wrap = 80,
                           subtitle = NULL,
                           subtitle_wrap = 80,
                           x_balance = FALSE,
                           x_breaks_n = 2,
                           x_expand = NULL,
                           x_jitter = 0,
                           x_labels = NULL,
                           x_rev = FALSE,
                           x_title = NULL,
                           x_title_wrap = 50,
                           x_zero = FALSE,
                           x_zero_line = NULL,
                           y_balance = FALSE,
                           y_breaks_n = 3,
                           y_expand = c(0, 0),
                           y_jitter = 0,
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
                           theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE)) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) 
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
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a point plot")
  
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
  pal_point <- scales::alpha(pal, alpha = alpha_point)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_point(aes(x = !!x_var, y = !!y_var, text = !!text_var), 
               col = pal_point, 
               size = size_point, 
               position = position_jitter(width = x_jitter, height = y_jitter))
  
  #x scale 
  if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if (is.null(x_expand)) x_expand <- waiver()
    if (is.null(x_labels)) x_labels <- snakecase::to_sentence_case
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  else if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr)) {
    if (facet_scales %in% c("fixed", "free_y")) {
      x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
      x_zero <- x_zero_list[[1]]
      x_zero_line <- x_zero_list[[2]]
      x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_breaks), max(x_breaks))
      if (is.null(x_expand)) x_expand <- c(0, 0)
      
      if (is.null(x_labels)) {
        if (is.numeric(x_var_vctr)) x_labels <- scales::label_comma()
        else if (lubridate::is.Date(x_var_vctr)) x_labels <- scales::label_date()
        else x_labels <- waiver()
      }
    }
    
    if (is.numeric(x_var_vctr)) {
      plot <- plot +
        scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
      
      if (x_zero_line == TRUE) {
        plot <- plot +
          geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
      }
    }
    else if (lubridate::is.Date(x_var_vctr)) {
      plot <- plot +
        scale_x_date(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
    }
    else if (lubridate::is.POSIXt(x_var_vctr)) {
      plot <- plot +
        scale_x_datetime(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
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

#' @title Point ggplot that is coloured and facetted.
#' @description Point ggplot that is coloured and facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be plotted untransformed. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or POSIXt). Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_point The opacity of the points. 
#' @param size_point Size of points. Defaults to 1.5.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 2. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_jitter Amount of horizontal jitter to be added in positive and negative directions. Defaults to 0. See ggplot2::position_jitter for further information.
#' @param x_labels A function or named vector to modify x scale labels. Use function(x) x to keep labels untransformed.
#' @param x_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_breaks_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_jitter Amount of vertical jitter to be added in positive and negative directions. Defaults to 0.See ggplot2::position_jitter for further information. 
#' @param y_labels A function or named vector to modify y scale labels. Use function(x) x to keep labels untransformed.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_breaks_n For a numeric colour variable, the desired number of intervals on the colour scale. 
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case for categorical colour variables and scales::label_comma() for numeric. Use function(x) x to keep labels untransformed.   
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_method The method of colouring features, either "bin", "quantile", "continuous", or "category." If numeric, defaults to "bin".
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use function(x) x to keep labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param theme A ggplot2 theme.  
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_point_col_facet(penguins, 
#'                    x_var = bill_length_mm, 
#'                    y_var = body_mass_g, 
#'                    col_var = sex, 
#'                    facet_var = species)
#' 
gg_point_col_facet <- function(data,
                               x_var,
                               y_var,
                               col_var,
                               facet_var,
                               text_var = NULL,
                               pal = NULL,
                               pal_na = "#7F7F7F",
                               pal_rev = FALSE,
                               alpha_point = 1,
                               size_point = 1.5,
                               title = NULL,
                               title_wrap = 80,
                               subtitle = NULL,
                               subtitle_wrap = 80,
                               x_balance = FALSE,
                               x_breaks_n = 2,
                               x_expand = NULL,
                               x_jitter = 0,
                               x_labels = NULL,
                               x_rev = FALSE,
                               x_title = NULL,
                               x_title_wrap = 50,
                               x_zero = FALSE,
                               x_zero_line = NULL,
                               y_balance = FALSE,
                               y_breaks_n = 3,
                               y_expand = c(0, 0),
                               y_jitter = 0,
                               y_labels = scales::label_comma(),
                               y_title = NULL,
                               y_title_wrap = 50,
                               y_zero = FALSE,
                               y_zero_line = NULL,
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
                               facet_scales = "fixed",
                               caption = NULL,
                               caption_wrap = 80,
                               theme = gg_theme(gridlines_h = TRUE, gridlines_v = TRUE)) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) 
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
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a point plot")
  
  if (!is.null(col_method)) {
    if (!col_method %in% c("continuous", "bin", "quantile", "category")) stop("Please use a colour method of 'continuous', 'bin', 'quantile' or 'category'")
  }
  
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
  
  if (facet_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!facet_var, ~forcats::fct_rev(.x)))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
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
                          ~ kimisc::cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE, format_fun = col_labels)))
        
        col_labels <- sv_interval_labels_chr
      }
      else {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ kimisc::cut_format(.x, col_cuts,
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
  
  pal_point <- scales::alpha(pal, alpha = alpha_point)
  pal_na_point <- scales::alpha(pal_na, alpha = alpha_point)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    coord_cartesian(clip = "off") +
    geom_point(aes(x = !!x_var, y = !!y_var, col = !!col_var, text = !!text_var), 
               size = size_point, 
               position = position_jitter(width = x_jitter, height = y_jitter))
  
  #x scale 
    if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
      if (is.null(x_expand)) x_expand <- waiver()
      if (is.null(x_labels)) x_labels <- snakecase::to_sentence_case
      
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = x_labels)
    }
    else if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr)) {
      if (facet_scales %in% c("fixed", "free_y")) {
          x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
          x_zero <- x_zero_list[[1]]
          x_zero_line <- x_zero_list[[2]]
          x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, breaks_n = x_breaks_n, zero = x_zero, mobile = FALSE)
          x_limits <- c(min(x_breaks), max(x_breaks))
          if (is.null(x_expand)) x_expand <- c(0, 0)
          
          if (is.null(x_labels)) {
            if (is.numeric(x_var_vctr)) x_labels <- scales::label_comma()
            else if (lubridate::is.Date(x_var_vctr)) x_labels <- scales::label_date()
            else x_labels <- waiver()
          }
        }
        
        if (is.numeric(x_var_vctr)) {
          plot <- plot +
            scale_x_continuous(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
          
          if (x_zero_line == TRUE) {
            plot <- plot +
              geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
          }
        }
        else if (lubridate::is.Date(x_var_vctr)) {
          plot <- plot +
            scale_x_date(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
        }
        else if (lubridate::is.POSIXt(x_var_vctr)) {
          plot <- plot +
            scale_x_datetime(expand = x_expand, breaks = x_breaks, limits = x_limits, labels = x_labels)
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
    
    #colour
    if (col_method == "continuous") {
      plot <- plot +
        scale_colour_gradientn(
          colors = pal_point,
          labels = col_labels,
          breaks = col_cuts,
          na.value = pal_na_point,
          name = stringr::str_wrap(col_title, col_title_wrap))  
    }
    else if (col_method %in% c("quantile", "bin", "category")) {
      plot <- plot +
        scale_colour_manual(
          values = pal_point,
          drop = FALSE,
          labels = col_labels,
          na.value = pal_na_point,
          name = stringr::str_wrap(col_title, col_title_wrap)
        ) 
    }
    
    #titles & facetting
    if (col_legend_none == TRUE) plot <- plot +
      theme(legend.position = "none")
    
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
