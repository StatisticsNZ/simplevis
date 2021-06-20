#' @title Vertical bar ggplot.
#' @description Vertical bar ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_reorder For a categorical x variable, TRUE or FALSE of whether the x variable variable is to be reordered by the x variable. Defaults to FALSE.
#' @param x_rev For a categorical variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 5. 
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
#' plot_data <- penguins %>% 
#'   group_by(species) %>% 
#'   summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))  
#' 
#' gg_bar(plot_data, species, body_mass_g)
#' 
gg_bar <- function(data,
                    x_var,
                    y_var,
                    text_var = NULL,
                    pal = NULL,
                    width = 0.75,
                    alpha = 1,
                    size_line = 0.5,
                    title = NULL,
                    title_wrap = 100,
                    subtitle = NULL,
                    subtitle_wrap = 100,
                    x_balance = FALSE,
                    x_expand = NULL,
                    x_labels = NULL,
                    x_na = TRUE,
                    x_pretty_n = 6,
                    x_reorder = FALSE,
                    x_rev = FALSE,
                    x_title = NULL,
                    x_title_wrap = 50,
                    x_zero = FALSE,
                    x_zero_line = NULL,
                    y_balance = FALSE,
                    y_expand = NULL,
                    y_labels = waiver(),
                    y_na = TRUE,
                    y_pretty_n = 5,
                    y_title = NULL,
                    y_title_wrap = 50,
                    y_trans = "identity",
                    y_zero = TRUE,
                    y_zero_line = NULL,
                    caption = NULL,
                    caption_wrap = 80,
                    font_family = "",
                    font_size_title = NULL,
                    font_size_body = NULL,
                    mobile = FALSE
) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  if (x_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }

  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  if (is.character(y_var_vctr) | is.factor(y_var_vctr) | is.logical(y_var_vctr)) {
    if (x_reorder == TRUE) {
      if(x_rev == FALSE) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_reorder(.x, !!y_var, .desc = TRUE)))
      } 
      else if(x_rev == TRUE) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_reorder(.x, !!y_var, .desc = FALSE)))
      } 
      x_var_vctr <- dplyr::pull(data, !!x_var)
    } 
    else if (x_rev == TRUE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  plot <- ggplot(data) +
    theme_bar(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_col(aes(x = !!x_var, y = !!y_var, text = !!text_var), 
             col = pal, 
             fill = pal, 
             alpha = alpha, 
             size = size_line, 
             width = bar_width)
  
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
    if(is.null(x_labels)) x_labels <- function(x) stringr::str_to_sentence(x)
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
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
        oob = scales::squish
      )
  })
  
  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
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
      theme_mobile_graph()
  }
  
  return(plot)
}

#' @title Vertical bar ggplot that is coloured.
#' @description Vertical bar ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "dodge" or "stack". Defaults to "dodge".
#' @param pal Character vector of hex codes. 
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_rev For a categorical variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
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
#' gg_bar_col(plot_data, species, body_mass_g, sex)
#' 
#' gg_bar_col(plot_data, species, body_mass_g, sex, position = "stack")
#' 
gg_bar_col <- function(data,
                        x_var,
                        y_var,
                        col_var,
                        text_var = NULL,
                        position = "dodge",
                        pal = NULL,
                        pal_rev = FALSE,
                        width = 0.75,
                        alpha = 1,
                        size_line = 0.5,
                        title = NULL,
                        title_wrap = 100,
                        subtitle = NULL,
                        subtitle_wrap = 100,
                        x_balance = FALSE,
                        x_expand = NULL,
                        x_labels = NULL,
                        x_na = TRUE,
                        x_pretty_n = 6,
                        x_rev = FALSE,
                        x_title = NULL,
                        x_title_wrap = 50,
                        x_zero = FALSE,
                        x_zero_line = NULL,
                        y_balance = FALSE,
                        y_expand = NULL,
                        y_labels = waiver(),
                        y_na = TRUE,
                        y_pretty_n = 5,
                        y_title = NULL,
                        y_title_wrap = 50,
                        y_trans = "identity",
                        y_zero = TRUE,
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
                        mobile = FALSE)
{
  
  data <- dplyr::ungroup(data)
  y_var <- rlang::enquo(y_var) #numeric var
  x_var <- rlang::enquo(x_var) 
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
  
  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a vertical bar plot")
  
  if (y_trans != "identity") {
    if (position == "stack") stop("Please use position = 'dodge', if you would like to not have zero as the minimum of y scale")
  } 
  if (y_zero == FALSE) {
    if (position == "stack") stop("Please use position = 'dodge', if you would like to not have zero as the minimum of y scale")
  }
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (is.character(x_var_vctr) | is.factor(x_var_vctr)) {
    if (x_rev == TRUE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
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
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (position == "stack") position2 <- "stack"
  else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
  
  if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  plot <- ggplot(data) +
    theme_bar(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_col(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
             alpha = alpha, 
             size = size_line, 
             width = bar_width, 
             position = position2)
  
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
    if(is.null(x_labels)) x_labels <- function(x) stringr::str_to_sentence(x)
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  } 
  
  if (position == "stack") {
    data_sum <- data %>%
      dplyr::group_by(dplyr::across(!!x_var), .drop = FALSE) %>%
      dplyr::summarise(dplyr::across(!!y_var, ~sum(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()
    
    y_var_vctr <- dplyr::pull(data_sum, !!y_var)
  }
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
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
        oob = scales::squish
      )
  })
  
  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
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
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(fill = guide_legend(
        ncol = col_legend_ncol,
        byrow = TRUE,
        title = stringr::str_wrap(col_title, col_title_wrap)
      ), 
      col = guide_legend(
        ncol = col_legend_ncol, nrow = col_legend_nrow, 
        byrow = TRUE,
        title = stringr::str_wrap(col_title, col_title_wrap)
      ))
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
      guides(
        fill = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 20)),
        col = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 20))
      ) +
      theme_mobile_graph()
  }
  
  return(plot)
}

#' @title Vertical bar ggplot that is facetted.
#' @description Vertical bar ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1.
#' @param size_line The size of the outlines of bars. 
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_rev For a categorical variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
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
#' gg_bar_facet(plot_data, sex, body_mass_g, species)
#'
gg_bar_facet <- function(data,
                          x_var,
                          y_var,
                          facet_var,
                          text_var = NULL,
                          pal = NULL,
                          width = 0.75,
                          alpha = 1,
                          size_line = 0.5,
                          title = NULL,
                          title_wrap = 100,
                          subtitle = NULL,
                          subtitle_wrap = 100,
                          x_balance = FALSE,
                          x_expand = NULL,
                          x_labels = NULL,
                          x_na = TRUE,
                          x_pretty_n = 3,
                          x_rev = FALSE,
                          x_title = NULL,
                          x_title_wrap = 50,
                          x_zero = FALSE,
                          x_zero_line = NULL,
                          y_balance = FALSE,
                          y_expand = NULL,
                          y_labels = waiver(),
                          y_na = TRUE,
                          y_pretty_n = 4,
                          y_title = NULL,
                          y_title_wrap = 50,
                          y_trans = "identity",
                          y_zero = TRUE,
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
                          font_size_body = NULL
) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
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
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a vertical bar plot")
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if(is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  if (is.character(x_var_vctr) | is.factor(x_var_vctr)) {
    if (x_rev == TRUE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  plot <- ggplot(data) +
    theme_bar(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    geom_col(aes(x = !!x_var, y = !!y_var, text = !!text_var), col = pal, fill = pal, alpha = alpha, size = size_line, width = bar_width)
  
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
      if(is.null(x_labels)) x_labels <- function(x) stringr::str_to_sentence(x)
      
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
          oob = scales::squish
        )
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand,
                         trans = y_trans,
                         labels = y_labels,
                         oob = scales::squish)
  }
  
  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if(is.null(facet_labels)) facet_labels <- as_labeller(stringr::str_to_sentence)
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(x_title, x_title_wrap),
      y = stringr::str_wrap(y_title, y_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = facet_labels, scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
  
  return(plot)
}

#' @title Vertical bar ggplot that is coloured and facetted.
#' @description Vertical bar ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "dodge" or "stack". Defaults to "dodge".
#' @param pal Character vector of hex codes. 
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 3. 
#' @param x_rev For a categorical variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 4. 
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
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
#' plot_data <- penguins %>% 
#'   group_by(species, sex, island) %>% 
#'   summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))  
#' 
#' gg_bar_col_facet(plot_data, species, body_mass_g, island, sex)
#' 
gg_bar_col_facet <- function(data,
                              x_var,
                              y_var,
                              col_var,
                              facet_var,
                              text_var = NULL,
                              position = "dodge",
                              pal = NULL,
                              pal_rev = FALSE,
                              width = 0.75,
                              alpha = 1,
                              size_line = 0.5,
                              title = NULL,
                              title_wrap = 100,
                              subtitle = NULL,
                              subtitle_wrap = 100,
                              x_balance = FALSE,
                              x_expand = NULL,
                              x_labels = NULL,
                              x_na = TRUE,
                              x_pretty_n = 3,
                              x_rev = FALSE,
                              x_title = NULL,
                              x_title_wrap = 50,
                              x_zero = FALSE,
                              x_zero_line = NULL,
                              y_balance = FALSE,
                              y_expand = NULL,
                              y_labels = waiver(),
                              y_na = TRUE,
                              y_pretty_n = 4,
                              y_title = NULL,
                              y_title_wrap = 50,
                              y_trans = "identity",
                              y_zero = TRUE,
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
                              font_size_body = NULL)
{
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
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
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
  if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a vertical bar plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a vertical bar plot")
  
  if (y_trans != "identity") {
    if (position == "stack") stop("Please use position = 'dodge', if you would like to not have zero as the minimum of y scale")
  } 
  if (y_zero == FALSE) {
    if (position == "stack") stop("Please use position = 'dodge', if you would like to not have zero as the minimum of y scale")
  } 
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
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
  
  if (is.character(x_var_vctr) | is.factor(x_var_vctr)) {
    if (x_rev == TRUE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
      
      x_var_vctr <- dplyr::pull(data, !!x_var)
    }
  }
  
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
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
  
  if (position == "stack") position2 <- "stack"
  else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
  
  if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    col_n <- length(levels(col_var_vctr))
  }
  else col_n <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- pal_d3_reorder(col_n)
  else pal <- pal[1:col_n]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  plot <- ggplot(data) +
    coord_cartesian() +
    theme_bar(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_col(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
             alpha = alpha, 
             size = size_line, 
             width = bar_width, 
             position = position2)
  
  if (position == "stack") {
    data_sum <- data %>%
      dplyr::group_by(dplyr::across(c(!!x_var, !!facet_var)), .drop = FALSE) %>%
      dplyr::summarise(dplyr::across(!!y_var, ~sum(.x, na.rm = TRUE))) %>%
      dplyr::ungroup()
    
    y_var_vctr <- dplyr::pull(data_sum, !!y_var)
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
      if(is.null(x_labels)) x_labels <- function(x) stringr::str_to_sentence(x)
      
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
          oob = scales::squish
        )
    })
  }
  else if (facet_scales %in% c("free", "free_y")) {
    plot <- plot +
      scale_y_continuous(expand = y_expand,
                         trans = y_trans,
                         labels = y_labels,
                         oob = scales::squish)
  }
  
  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
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
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(x_title, x_title_wrap),
      y = stringr::str_wrap(y_title, y_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), labeller = facet_labels, scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow) +
    guides(fill = guide_legend(
      ncol = col_legend_ncol, nrow = col_legend_nrow, 
      byrow = TRUE,
      title = stringr::str_wrap(col_title, col_title_wrap)
    ), 
    col = guide_legend(
      ncol = 1,
      byrow = TRUE,
      title = stringr::str_wrap(col_title, 15)
      ))  
    
    return(plot)
  }
