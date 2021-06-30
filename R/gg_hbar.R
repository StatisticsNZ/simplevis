#' @title Horizontal bar ggplot.
#' @description Horizontal bar ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted variable to be on the y scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 80. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the x scale. Defaults to FALSE.
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre. Defaults to FALSE.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param y_reorder For a categorical y variable, TRUE or FALSE of whether the y variable variable is to be reordered by the y variable. Defaults to FALSE.
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE of whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.   
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
#' plot_data <- penguins %>% 
#'   group_by(species) %>% 
#'   summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))  
#' 
#' gg_hbar(plot_data, 
#'         x_var = body_mass_g, 
#'         y_var = species)
#' 
gg_hbar <- function(data,
                    x_var,
                    y_var,
                    text_var = NULL,
                    pal = NULL,
                    width = 0.75,
                    alpha = 1,
                    size_line = 0.5,
                    title = NULL,
                    title_wrap = 80,
                    subtitle = NULL,
                    subtitle_wrap = 80,
                    x_balance = FALSE,
                    x_expand = NULL,
                    x_gridlines_minor = FALSE,
                    x_labels = waiver(),
                    x_na = TRUE,
                    x_pretty_n = 5,
                    x_title = NULL,
                    x_title_wrap = 50,
                    x_trans = "identity",
                    x_zero = TRUE,
                    x_zero_line = NULL,
                    y_balance = FALSE,
                    y_expand = NULL,
                    y_labels = NULL,
                    y_na = TRUE,
                    y_pretty_n = 6,
                    y_reorder = FALSE,
                    y_rev = FALSE,
                    y_title = NULL,
                    y_title_wrap = 50,
                    y_zero = FALSE,
                    y_zero_line = NULL,
                    caption = NULL,
                    caption_wrap = 80,
                    font_family = "",
                    font_size_title = NULL,
                    font_size_body = NULL,
                    mobile = FALSE
) {
  
  data <- dplyr::ungroup(data)
  y_var <- rlang::enquo(y_var)
  x_var <- rlang::enquo(x_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  if (x_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }

  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
  
  if(is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))

  if (is.character(y_var_vctr) | is.factor(y_var_vctr)) {
    if (y_reorder == TRUE) {
      if(y_rev == FALSE) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_reorder(.x, !!x_var, .desc = FALSE)))
      } 
      else if(y_rev == TRUE) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_reorder(.x, !!x_var, .desc = TRUE)))
      } 
      y_var_vctr <- dplyr::pull(data, !!y_var)
    } 
    else if (y_rev == FALSE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  if (lubridate::is.Date(y_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  plot <- ggplot(data) +
    theme_x_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_col(aes(x = !!y_var, y = !!x_var, text = !!text_var), 
             col = pal, 
             fill = pal, 
             alpha = alpha, 
             size = size_line, 
             width = bar_width)
  
  if (is.numeric(y_var_vctr) | lubridate::is.Date(y_var_vctr) | lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = "identity", zero = y_zero)
    y_limits <- c(min(y_var_vctr), max(y_var_vctr))
    if(is.null(y_expand)) y_expand <- c(0, 0)
    if(is.null(y_labels)) y_labels <- waiver()

    if(mobile == TRUE) {
      y_breaks <- y_limits
      if (min(y_limits) < 0 & max(y_limits > 0)) y_breaks <- c(y_limits[1], 0, y_limits[2])
    }
  }
  
  if (is.numeric(y_var_vctr)) {
    plot <- plot +      
      coord_flip() +
      scale_x_reverse(expand = y_expand,
                      breaks = y_breaks,
                      labels = y_labels,
                      oob = scales::squish)
    
    if(y_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  else if (lubridate::is.Date(y_var_vctr)) {
    plot <- plot +
      coord_flip() +
      scale_x_date(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels
      )
  }
  else if (lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
    plot <- plot +
      coord_flip() +
      scale_x_datetime(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels
      )
  }
  else if (is.character(y_var_vctr) | is.factor(y_var_vctr)){
    if(is.null(y_expand)) y_expand <- waiver()
    if(is.null(y_labels)) y_labels <- function(x) stringr::str_to_sentence(x)
    
      plot <- plot +
        coord_flip() +
        scale_x_discrete(expand = y_expand, labels = y_labels)
  }

  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  if(is.null(x_expand)) x_expand <- c(0, 0)
  
  if (all(x_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
  }
  else ({
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = x_trans, zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    
    plot <- plot +
      scale_y_continuous(
        expand = x_expand,
        breaks = x_breaks,
        limits = x_limits,
        trans = x_trans,
        labels = x_labels,
        oob = scales::squish
      )
  })
  
  if(x_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (x_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.x = element_line(colour = "#D3D3D3", size = 0.2))
  }
  
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

#' @title Horizontal bar ggplot that is coloured.
#' @description Horizontal bar ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted variable to be on the y scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "dodge" or "stack". Defaults to "dodge".
#' @param pal Character vector of hex codes. 
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 80. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the x scale. Defaults to FALSE.
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre. Defaults to FALSE.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_reorder For a categorical y variable, TRUE or FALSE of whether the y variable variable is to be reordered by the y variable. Defaults to FALSE.
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE of whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.   
#' @param col_labels A function or vector to modify colour scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
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
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' plot_data <- penguins %>% 
#'   group_by(species, sex) %>% 
#'   summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))  
#' 
#' gg_hbar_col(plot_data, 
#'             x_var = body_mass_g, 
#'             y_var = species, 
#'             col_var = sex)
#' 
#' gg_hbar_col(plot_data, 
#'             x_var = body_mass_g, 
#'             y_var = species, 
#'             col_var = sex, 
#'             position = "stack")
#'             
gg_hbar_col <- function(data,
                        x_var,
                        y_var,
                        col_var,
                        text_var = NULL,
                        position = NULL,
                        pal = NULL,
                        pal_rev = FALSE,
                        width = 0.75,
                        alpha = 1,
                        size_line = 0.5,
                        title = NULL,
                        title_wrap = 80,
                        subtitle = NULL,
                        subtitle_wrap = 80,
                        x_balance = FALSE,
                        x_expand = NULL,
                        x_gridlines_minor = FALSE,
                        x_labels = waiver(),
                        x_na = TRUE,
                        x_pretty_n = 5,
                        x_title = NULL,
                        x_title_wrap = 50,
                        x_trans = "identity",
                        x_zero = TRUE,
                        x_zero_line = NULL,
                        y_balance = FALSE,
                        y_expand = NULL,
                        y_labels = NULL,
                        y_na = TRUE,
                        y_pretty_n = 6,
                        y_reorder = FALSE,
                        y_rev = FALSE,
                        y_title = NULL,
                        y_title_wrap = 50,
                        y_zero = FALSE,
                        y_zero_line = NULL,
                        col_labels = NULL,
                        col_legend_ncol = NULL,
                        col_legend_nrow = NULL,
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
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) 
  col_var <- rlang::enquo(col_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  if (x_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal bar plot")
  
  if(is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (is.character(y_var_vctr) | is.factor(y_var_vctr)) {
    if (y_rev == FALSE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
  }
  
  if (col_rev == FALSE){
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
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (lubridate::is.Date(y_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == FALSE) pal <- rev(pal)
  
  if(!is.null(position)) {
    if (!position %in% c("dodge", "stack")) stop("Please use a position of either 'stack' or 'fill'")
  }
  
  if (is.null(position)) {
    position2 <- position_dodge2(preserve = "single")
  }
  else position2 <- position

  plot <- ggplot(data) +
    theme_x_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_col(aes(x = !!y_var, y = !!x_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
             alpha = alpha, 
             size = size_line, 
             width = bar_width, 
             position = position2)
  
  if (!is.null(position)) {
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by(dplyr::across(!!y_var), .drop = FALSE) %>%
        dplyr::summarise(dplyr::across(!!x_var, ~sum(.x, na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      x_var_vctr <- dplyr::pull(data_sum, !!x_var)
    }
    # else if (position == "fill") x_var_vctr <- c(0, 1)
  }
  
  if (is.numeric(y_var_vctr) | lubridate::is.Date(y_var_vctr) | lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = "identity", zero = y_zero)
    y_limits <- c(min(y_var_vctr), max(y_var_vctr))
    if(is.null(y_expand)) y_expand <- c(0, 0)
    if(is.null(y_labels)) y_labels <- waiver()
    
    if(mobile == TRUE) {
      y_breaks <- y_limits
      if (min(y_limits) < 0 & max(y_limits > 0)) y_breaks <- c(y_limits[1], 0, y_limits[2])
    }
  }
  
  if (is.numeric(y_var_vctr)) {
    plot <- plot +      
      coord_flip() +
      scale_x_reverse(expand = y_expand,
                      breaks = y_breaks,
                      labels = y_labels,
                      oob = scales::squish)
    
    if(y_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  else if (lubridate::is.Date(y_var_vctr)) {
    plot <- plot +
      coord_flip() +
      scale_x_date(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels
      )
  }
  else if (lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
    plot <- plot +
      coord_flip() +
      scale_x_datetime(
        expand = y_expand,
        breaks = y_breaks,
        labels = y_labels
      )
  }
  else if (is.character(y_var_vctr) | is.factor(y_var_vctr)){
    if(is.null(y_expand)) y_expand <- waiver()
    if(is.null(y_labels)) y_labels <- function(x) stringr::str_to_sentence(x)
    
    plot <- plot +
      coord_flip() +
      scale_x_discrete(expand = y_expand, labels = y_labels)
  }

  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  if(is.null(x_expand)) x_expand <- c(0, 0)
  
  if (all(x_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
  }
  else ({
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = x_trans, zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    
    plot <- plot +
      scale_y_continuous(
        expand = x_expand,
        breaks = x_breaks,
        limits = x_limits,
        trans = x_trans,
        labels = x_labels,
        oob = scales::squish
      )
  })
  
  if(x_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (x_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.x = element_line(colour = "#D3D3D3", size = 0.2))
  }
  
  if(is.null(col_labels)) col_labels <- function(x) stringr::str_to_sentence(x)

  plot <- plot +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na()
    ) +
    scale_colour_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na()
    ) 
  
  if (mobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(y_title, y_title_wrap),
        y = stringr::str_wrap(x_title, x_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(fill = guide_legend(
        ncol = col_legend_ncol,
        byrow = TRUE,
        reverse = TRUE, 
        title = stringr::str_wrap(col_title, col_title_wrap)
      ), 
      col = guide_legend(
        ncol = col_legend_ncol, nrow = col_legend_nrow, 
        byrow = TRUE,
        reverse = TRUE, 
        title = stringr::str_wrap(col_title, col_title_wrap)
      ))
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
      guides(
        fill = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 20)),
        col = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 20))
      ) +
      theme_mobile_extra()
  }
  
  return(plot)
}

#' @title Horizontal bar ggplot that is facetted.
#' @description Horizontal bar ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted variable to be on the y scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1.
#' @param size_line The size of the outlines of bars. 
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 80. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the x scale. Defaults to FALSE.
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre. Defaults to FALSE.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE of whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.   
#' @param facet_labels As per the ggplot2 labeller argument within the ggplot facet_wrap function. If NULL, defaults to ggplot2::as_labeller(stringr::str_to_sentence). Use facet_labels = ggplot2::label_value to turn off default sentence case transformation.
#' @param facet_na TRUE or FALSE of whether to include facet_var NA values. Defaults to TRUE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' plot_data <- penguins %>% 
#'   group_by(species, sex) %>% 
#'   summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))  
#' 
#' gg_hbar_facet(plot_data, 
#'               x_var = body_mass_g, 
#'               y_var = sex, 
#'               facet_var = species)
#'
gg_hbar_facet <- function(data,
                          x_var,
                          y_var,
                          facet_var,
                          text_var = NULL,
                          pal = NULL,
                          width = 0.75,
                          alpha = 1,
                          size_line = 0.5,
                          title = NULL,
                          title_wrap = 80,
                          subtitle = NULL,
                          subtitle_wrap = 80,
                          x_balance = FALSE,
                          x_expand = NULL,
                          x_gridlines_minor = FALSE,
                          x_labels = waiver(),
                          x_na = TRUE,
                          x_pretty_n = 3,
                          x_title = NULL,
                          x_title_wrap = 50,
                          x_trans = "identity",
                          x_zero = TRUE,
                          x_zero_line = NULL,
                          y_balance = FALSE,
                          y_expand = NULL,
                          y_labels = NULL,
                          y_na = TRUE,
                          y_pretty_n = 4,
                          y_rev = FALSE,
                          y_title = NULL,
                          y_title_wrap = 50,
                          y_zero = FALSE,
                          y_zero_line = NULL,
                          facet_labels = NULL,
                          facet_na = TRUE,
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
  y_var <- rlang::enquo(y_var) #categorical var
  x_var <- rlang::enquo(x_var) #numeric var
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  if (x_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (facet_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal bar plot")
  
  if(is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if(is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  if (is.character(y_var_vctr) | is.factor(y_var_vctr)) {
    if (y_rev == FALSE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  if (lubridate::is.Date(y_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  plot <- ggplot(data) +
    theme_x_gridlines(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    geom_col(aes(x = !!y_var, y = !!x_var, text = !!text_var), col = pal, fill = pal, alpha = alpha, size = size_line, width = bar_width)
  
  if (facet_scales %in% c("fixed", "free_x")) {
    if (is.numeric(y_var_vctr) | lubridate::is.Date(y_var_vctr) | lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
      
      y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
      y_zero <- y_zero_list[[1]]
      y_zero_line <- y_zero_list[[2]]
      
      y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = "identity", zero = y_zero)
      y_limits <- c(min(y_var_vctr), max(y_var_vctr))
      if(is.null(y_expand)) y_expand <- c(0, 0)
      if(is.null(y_labels)) y_labels <- waiver()
    }
    
    if (is.numeric(y_var_vctr)) {
      plot <- plot +      
        coord_flip() +
        scale_x_reverse(expand = y_expand,
                        breaks = y_breaks,
                        labels = y_labels,
                        oob = scales::squish)
      
      if(y_zero_line == TRUE) {
        plot <- plot +
          geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
      }
    }
    else if (lubridate::is.Date(y_var_vctr)) {
      plot <- plot +
        coord_flip() +
        scale_x_date(
          expand = y_expand,
          breaks = y_breaks,
          labels = y_labels
        )
    }
    else if (lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
      plot <- plot +
        coord_flip() +
        scale_x_datetime(
          expand = y_expand,
          breaks = y_breaks,
          labels = y_labels
        )
    }
    else if (is.character(y_var_vctr) | is.factor(y_var_vctr)){
      if(is.null(y_expand)) y_expand <- waiver()
      if(is.null(y_labels)) y_labels <- function(x) stringr::str_to_sentence(x)
      
      plot <- plot +
        coord_flip() +
        scale_x_discrete(expand = y_expand, labels = y_labels)
    }
  }
  
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  if(facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  if(is.null(x_expand)) x_expand <- c(0, 0)
  
  if (facet_scales %in% c("fixed", "free_y")) {
    if (all(x_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
    }
    else ({
      x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = x_trans, zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_breaks), max(x_breaks))
      
      plot <- plot +
        scale_y_continuous(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          trans = x_trans,
          labels = x_labels,
          oob = scales::squish
        )
    })
  }
  else if (facet_scales %in% c("free")) {
    plot <- plot +
      scale_y_continuous(expand = x_expand,
                         trans = x_trans,
                         labels = x_labels,
                         oob = scales::squish)
  }
  
  if(facet_scales %in% c("free_y", "free")) {
    plot <- plot +
      coord_flip()
  }
  
  if(x_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (x_gridlines_minor == TRUE) {
    plot <- plot +
      theme(panel.grid.minor.x = element_line(colour = "#D3D3D3", size = 0.2))
  }
  
  if(is.null(facet_labels)) facet_labels <- as_labeller(stringr::str_to_sentence)

  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(y_title, y_title_wrap),
      y = stringr::str_wrap(x_title, x_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = facet_labels, scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
  
  return(plot)
}

#' @title Horizontal bar ggplot that is facetted.
#' @description Horizontal bar ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param y_var Unquoted variable to be on the y scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "dodge" or "stack". Defaults to "dodge".
#' @param pal Character vector of hex codes. 
#' @param pal_rev TRUE or FALSE of whether to reverse the pal.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1.
#' @param size_line The size of the outlines of bars. 
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 80. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_gridlines_minor TRUE or FALSE of whether to add minor gridlines to the x scale. Defaults to FALSE.
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre. Defaults to FALSE.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param y_rev For a categorical variable, TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE of whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.   
#' @param col_labels A function or vector to modify colour scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_na TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_labels As per the ggplot2 labeller argument within the ggplot facet_wrap function. If NULL, defaults to ggplot2::as_labeller(stringr::str_to_sentence). Use facet_labels = ggplot2::label_value to turn off default sentence case transformation.
#' @param facet_na TRUE or FALSE of whether to include facet_var NA values. Defaults to TRUE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' plot_data <- penguins %>% 
#'   group_by(species, sex, island) %>% 
#'   summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))  
#' 
#' gg_hbar_col_facet(plot_data, 
#'                   x_var = body_mass_g, 
#'                   y_var = species, 
#'                   col_var = island, 
#'                   facet_var = sex, 
#'                   facet_na = FALSE)
#' 
gg_hbar_col_facet <- function(data,
                              x_var,
                              y_var,
                              col_var,
                              facet_var,
                              text_var = NULL,
                              position = NULL,
                              pal = NULL,
                              pal_rev = FALSE,
                              width = 0.75,
                              alpha = 1,
                              size_line = 0.5,
                              title = NULL,
                              title_wrap = 80,
                              subtitle = NULL,
                              subtitle_wrap = 80,
                              x_balance = FALSE,
                              x_expand = NULL,
                              x_gridlines_minor = FALSE,
                              x_labels = waiver(),
                              x_na = TRUE,
                              x_pretty_n = 3,
                              x_title = NULL,
                              x_title_wrap = 50,
                              x_trans = "identity",
                              x_zero = TRUE,
                              x_zero_line = NULL,
                              y_balance = FALSE,
                              y_expand = NULL,
                              y_labels = NULL,
                              y_na = TRUE,
                              y_pretty_n = 4,
                              y_rev = FALSE,
                              y_title = NULL,
                              y_title_wrap = 50,
                              y_zero = FALSE,
                              y_zero_line = NULL,
                              col_labels = NULL,
                              col_legend_ncol = NULL,
                              col_legend_nrow = NULL,
                              col_na = TRUE,
                              col_rev = FALSE,
                              col_title = NULL,
                              col_title_wrap = 25,
                              facet_labels = NULL,
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
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) 
  col_var <- rlang::enquo(col_var) #categorical var
  facet_var <- rlang::enquo(facet_var) #categorical var
  text_var <- rlang::enquo(text_var)
  
  if (x_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  if (facet_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal bar plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal bar plot")
  
  if(is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
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
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (is.character(y_var_vctr) | is.factor(y_var_vctr)) {
    if (y_rev == FALSE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
  }
  
  if (col_rev == FALSE){
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
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  if (lubridate::is.Date(y_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == FALSE) pal <- rev(pal)
  
  if(!is.null(position)) {
    if (!position %in% c("dodge", "stack")) stop("Please use a position of either 'stack' or 'fill'")
  }
  
  if (is.null(position)) {
    position2 <- position_dodge2(preserve = "single")
  }
  else position2 <- position
  
  plot <- ggplot(data) +
    theme_x_gridlines(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    geom_col(aes(x = !!y_var, y = !!x_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
             alpha = alpha, 
             size = size_line, 
             width = bar_width, 
             position = position2)
  
    if (!is.null(position)) {
      if (position == "stack") {
        data_sum <- data %>%
          dplyr::group_by(dplyr::across(c(!!y_var, !!facet_var)), .drop = FALSE) %>%
          dplyr::summarise(dplyr::across(!!x_var, ~sum(.x, na.rm = TRUE))) %>%
          dplyr::ungroup()
        
        x_var_vctr <- dplyr::pull(data_sum, !!x_var)
      }
      # else if (position == "fill") x_var_vctr <- seq(0, 1, 0.1)
    }
  
    if (facet_scales %in% c("fixed", "free_x")) {
      if (is.numeric(y_var_vctr) | lubridate::is.Date(y_var_vctr) | lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
        
        y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
        y_zero <- y_zero_list[[1]]
        y_zero_line <- y_zero_list[[2]]
        
        y_breaks <- sv_numeric_breaks_v(y_var_vctr, balance = y_balance, pretty_n = y_pretty_n, trans = "identity", zero = y_zero)
        y_limits <- c(min(y_var_vctr), max(y_var_vctr))
        if(is.null(y_expand)) y_expand <- c(0, 0)
        if(is.null(y_labels)) y_labels <- waiver()
      }
      
      if (is.numeric(y_var_vctr)) {
        plot <- plot +      
          coord_flip() +
          scale_x_reverse(expand = y_expand,
                          breaks = y_breaks,
                          labels = y_labels,
                          oob = scales::squish)
        
        if(y_zero_line == TRUE) {
          plot <- plot +
            geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
        }
      }
      else if (lubridate::is.Date(y_var_vctr)) {
        plot <- plot +
          coord_flip() +
          scale_x_date(
            expand = y_expand,
            breaks = y_breaks,
            labels = y_labels
          )
      }
      else if (lubridate::is.POSIXt(y_var_vctr) | lubridate::is.POSIXct(y_var_vctr) | lubridate::is.POSIXlt(y_var_vctr)) {
        plot <- plot +
          coord_flip() +
          scale_x_datetime(
            expand = y_expand,
            breaks = y_breaks,
            labels = y_labels
          )
      }
      else if (is.character(y_var_vctr) | is.factor(y_var_vctr)){
        if(is.null(y_expand)) y_expand <- waiver()
        if(is.null(y_labels)) y_labels <- function(x) stringr::str_to_sentence(x)
        
        plot <- plot +
          coord_flip() +
          scale_x_discrete(expand = y_expand, labels = y_labels)
      }
    }
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    if(facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if (all(x_var_vctr == 0, na.rm = TRUE)) {
        plot <- plot +
          scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
      }
      else ({
        x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = x_trans, zero = x_zero, mobile = FALSE)
        x_limits <- c(min(x_breaks), max(x_breaks))
        
        plot <- plot +
          scale_y_continuous(
            expand = x_expand,
            breaks = x_breaks,
            limits = x_limits,
            trans = x_trans,
            labels = x_labels,
            oob = scales::squish
          )
      })
    }
    else if (facet_scales %in% c("free")) {
      plot <- plot +
        scale_y_continuous(expand = x_expand,
                           trans = x_trans,
                           labels = x_labels,
                           oob = scales::squish)
    }
    
    if(facet_scales %in% c("free_y", "free")) {
      plot <- plot +
        coord_flip()
    }
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }
    
    if (x_gridlines_minor == TRUE) {
      plot <- plot +
        theme(panel.grid.minor.x = element_line(colour = "#D3D3D3", size = 0.2))
    }
    
    if(is.null(col_labels)) col_labels <- function(x) stringr::str_to_sentence(x)
    if(is.null(facet_labels)) facet_labels <- as_labeller(stringr::str_to_sentence)
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na()
      ) +
      scale_colour_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na()
      ) +
      guides(fill = guide_legend(
        ncol = col_legend_ncol,
        byrow = TRUE,
        reverse = TRUE, 
        title = stringr::str_wrap(col_title, col_title_wrap)
      ), 
      col = guide_legend(
        ncol = col_legend_ncol, nrow = col_legend_nrow, 
        byrow = TRUE,
        reverse = TRUE, 
        title = stringr::str_wrap(col_title, col_title_wrap)
      )) +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(y_title, y_title_wrap),
        y = stringr::str_wrap(x_title, x_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      facet_wrap(vars(!!facet_var), labeller = facet_labels, scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)

    return(plot)
}

