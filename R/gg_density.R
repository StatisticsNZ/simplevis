#' @title Smoothed density ggplot.
#' @description Smoothed density ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param pal Character vector of hex codes. 
#' @param alpha The alpha of the fill. Defaults to 0.5. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_pretty_n For a numeric or date y variable, the desired number of intervals on the y scale, as calculated by the pretty algorithm. Defaults to 5. 
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
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' gg_density(penguins, body_mass_g)
#' 
gg_density <- function(data,
                    x_var,
                    pal = NULL,
                    alpha = 0.5,
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

  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }

  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- "Density"
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- pal_viridis_reorder(1)
  else pal <- pal[1]
  
  plot <- ggplot(data) +
    theme_y_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_density(aes(x = !!x_var, y = .data$..density..), 
             col = pal, 
             fill = pal, 
             alpha = alpha, 
             size = size_line)
  
  if (is.numeric(x_var_vctr) | lubridate::is.Date(x_var_vctr) | lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    x_breaks <- sv_numeric_breaks_h(x_var_vctr, balance = x_balance, pretty_n = x_pretty_n, trans = "identity", zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
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
                         limits = x_limits,
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
        limits = x_limits,
        labels = x_labels
      )
  }
  else if (lubridate::is.POSIXt(x_var_vctr) | lubridate::is.POSIXct(x_var_vctr) | lubridate::is.POSIXlt(x_var_vctr)) {
    plot <- plot +
      scale_x_datetime(
        expand = x_expand,
        breaks = x_breaks,
        limits = x_limits,
        labels = x_labels
      )
  }
  else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if(is.null(x_expand)) x_expand <- waiver()
    if(is.null(x_labels)) x_labels <- function(x) stringr::str_to_sentence(x)
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  
  y_var_vctr <- c(0, max(stats::density(x_var_vctr, na.rm = TRUE)[[2]]))
  
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
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

