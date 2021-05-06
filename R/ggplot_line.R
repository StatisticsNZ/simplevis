#' @title Line ggplot.
#' @description Line ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param size_point Size of points. Defaults to 1. 
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where mobile equals TRUE.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE. 
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' 
#' plot_data <- storms %>%
#'   group_by(year) %>%
#'   summarise(wind = round(mean(wind), 2)) 
#'
#'   ggplot_line(plot_data, year, wind,
#'       title = "Average wind speed of Atlantic storms, 1975-2015",
#'       x_title = "Year",
#'       y_title = "Average maximum sustained wind speed (knots)")
#'
ggplot_line <- function(data,
                        x_var,
                        y_var,
                        text_var = NULL,
                        pal = NULL,
                        size_point = 1,
                        size_line = 0.5,
                        title = "[Title]",
                        title_wrap = 70,
                        subtitle = NULL,
                        subtitle_wrap = 80,
                        x_balance = FALSE,
                        x_labels = waiver(),
                        x_pretty_n = 6,
                        x_expand = NULL,
                        x_title = "[X title]",
                        x_trans = "identity", 
                        x_title_wrap = 50,
                        x_zero = FALSE,
                        x_zero_line = NULL,
                        y_balance = FALSE,
                        y_expand = NULL,
                        y_labels = waiver(),
                        y_pretty_n = 5,
                        y_title = "[Y title]",
                        y_title_wrap = 50,
                        y_trans = "identity",
                        y_zero = FALSE,
                        y_zero_line = NULL,
                        caption = NULL,
                        caption_wrap = 80,
                        font_family = "Helvetica",
                        font_size_title = NULL,
                        font_size_body = NULL,
                        mobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  if (!(lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr))) stop("Please use a numeric or date x variable for a line plot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a line plot")
  if(lubridate::is.Date(x_var_vctr) & (x_zero == TRUE | x_balance == TRUE | x_trans != "identity")) {
    stop("x_zero == FALSE, x_balance == FALSE or x_trans other than identity are only allowed when x_var is numeric")
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- sv_pal(1)
  else pal <- pal[1]
  
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme_line(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  plot <- plot +
    geom_line(aes(!!x_var, !!y_var, group = 1), size = size_line, col = pal[1]) +
    geom_point(aes(!!x_var, !!y_var, text = !!text_var), col = pal[1], size = size_point, alpha = 1)
  
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if(is.null(x_expand)) x_expand <- c(0, 0)
  if(is.null(y_expand)) y_expand <- c(0, 0)

  x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = mobile)
  x_limits <- c(min(x_breaks), max(x_breaks))

  if (lubridate::is.Date(x_var_vctr)) {
    plot <- plot +
      scale_x_date(
        expand = x_expand,
        breaks = x_breaks,
        limits = x_limits,
        labels = x_labels
      )
  }
  else if (is.numeric(x_var_vctr)) {
    plot <- plot +
      scale_x_continuous(expand = x_expand,
                         breaks = x_breaks,
                         limits = x_limits,
                         trans = x_trans,
                         labels = x_labels,
                         oob = scales::rescale_none)
  }
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
  
  if(x_zero_line == TRUE) {
    plot <- plot +
      geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
  }

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
      theme_mobile_graph() +
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

#' @title Line ggplot that is coloured.
#' @description Line ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable for lines and points to be coloured by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param size_point Size of points. Defaults to 1. 
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where mobile equals TRUE.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE. 
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend. 
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' 
#' plot_data <- storms %>%
#'   mutate(status = stringr::str_to_sentence(status)) %>%
#'   group_by(year, status) %>%
#'   summarise(wind = round(mean(wind), 2))
#'
#' ggplot_line_col(plot_data, year, wind, status)
#'
ggplot_line_col <-
  function(data,
           x_var,
           y_var,
           col_var,
           text_var = NULL,
           pal = NULL,
           pal_rev = FALSE,
           size_point = 1,
           size_line = 0.5,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_balance = FALSE,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 6,
           x_title = "[X title]",
           x_title_wrap = 50,
           x_trans = "identity",
           x_zero = FALSE,
           x_zero_line = NULL,
           y_balance = FALSE,
           y_expand = NULL,
           y_labels = waiver(),
           y_pretty_n = 5,
           y_title = "[Y title]",
           y_title_wrap = 50,
           y_trans = "identity",
           y_zero = FALSE,
           y_zero_line = NULL,
           col_labels = NULL,
           col_legend_ncol = NULL,
           col_legend_nrow = NULL,
           col_na = TRUE,
           col_title = "",
           col_title_wrap = 25,
           caption = NULL,
           caption_wrap = 80,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           mobile = FALSE) {
    
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    data <- data %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(!!x_var) #fix ggplotly legend bug
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if (!(lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr))) stop("Please use a numeric or date x variable for a line plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a line plot")
    if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a line plot")
    if(lubridate::is.Date(x_var_vctr) & (x_zero == TRUE | x_balance == TRUE | x_trans != "identity")) {
      stop("x_zero == FALSE, x_balance == FALSE or x_trans other than identity are only allowed when x_var is numeric")
    }
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_line(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    plot <- plot +
      geom_line(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var), size = size_line) +
      geom_point(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var, text = !!text_var),
                 size = size_point, alpha = 1)
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (!is.null(col_labels)) labels <- col_labels
    if (is.null(col_labels)) labels <- waiver()
    
    x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    
    if (lubridate::is.Date(x_var_vctr)) {
      plot <- plot +
        scale_x_date(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          labels = x_labels
        )
    }
    else if (is.numeric(x_var_vctr)) {
      plot <- plot +
        scale_x_continuous(expand = x_expand,
                           breaks = x_breaks,
                           limits = x_limits,
                           trans = x_trans,
                           labels = x_labels,
                           oob = scales::rescale_none)
    }
    
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
    
    plot <- plot +
      scale_color_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      ) 
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
    
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
        ) +
        guides(col = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap)))
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
        guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15))) +
        theme_mobile_graph()
    }
    
    return(plot)
  }

#' @title Line ggplot that is facetted.
#' @description Line ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param size_point Size of points. Defaults to 1. 
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale. Only applicable where facet_scales equals "fixed" or "free_x".
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE. 
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' 
#' plot_data <- storms %>%
#'   mutate(status = stringr::str_to_sentence(status)) %>%
#'   group_by(year, status) %>%
#'   summarise(wind = round(mean(wind), 2)) 
#'
#'  ggplot_line_facet(plot_data, year, wind, status)
#'
ggplot_line_facet <-
  function(data,
           x_var,
           y_var,
           facet_var,
           text_var = NULL,
           pal = NULL,
           size_point = 1,
           size_line = 0.5,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_balance = FALSE,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 5,
           x_title = "[X title]",
           x_title_wrap = 50,
           x_trans = "identity",
           x_zero = FALSE,
           x_zero_line = NULL,
           y_balance = FALSE,
           y_expand = NULL,
           y_labels = waiver(),
           y_pretty_n = 5,
           y_title = "[Y title]",
           y_title_wrap = 50,
           y_trans = "identity",
           y_zero = FALSE,
           y_zero_line = NULL,
           facet_ncol = NULL,
           facet_nrow = NULL,
           facet_scales = "fixed",
           caption = NULL,
           caption_wrap = 80,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL) {
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!(lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr))) stop("Please use a numeric or date x variable for a line plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a line plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a line plot")
    if(lubridate::is.Date(x_var_vctr) & (x_zero == TRUE | x_balance == TRUE | x_trans != "identity")) {
      stop("x_zero == FALSE, x_balance == FALSE or x_trans other than identity are only allowed when x_var is numeric")
    }
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (is.null(pal)) pal <- sv_pal(1)
    else pal <- pal[1]
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_line(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      )
    
    plot <- plot +
      geom_line(aes(!!x_var, !!y_var, group = 1), size = size_line, col = pal[1]) + 
      geom_point(aes(!!x_var, !!y_var, text = !!text_var), col = pal[1], size = size_point, alpha = 1)
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    if(facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    if(facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_y")) {
        x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = FALSE)
        x_limits <- c(min(x_breaks), max(x_breaks))
        
        if (lubridate::is.Date(x_var_vctr)) {
          plot <- plot +
            scale_x_date(
              expand = x_expand,
              breaks = x_breaks,
              limits = x_limits,
              labels = x_labels
            )
        }
        else if (is.numeric(x_var_vctr)) {
          plot <- plot +
            scale_x_continuous(expand = x_expand,
                               breaks = x_breaks,
                               limits = x_limits,
                               trans = x_trans,
                               labels = x_labels,
                               oob = scales::rescale_none)
        }
    }
    
    if (facet_scales %in% c("fixed", "free_x")) {
      if (all(y_var_vctr == 0, na.rm = TRUE)) {
        plot <- plot +
          scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
      }
      else ({
        y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }

    if(y_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }
    
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow) 

    return(plot)
  }

#' @title Line ggplot that is coloured and facetted.
#' @description Line ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable for lines and points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param size_point Size of points. Defaults to 1. 
#' @param size_line Size of lines. Defaults to 0.5. 
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.   
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale. Only applicable where facet_scales equals "fixed" or "free_x".
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE. 
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_ncol The number of columns of facetted plots.  
#' @param facet_nrow The number of rows of facetted plots.  
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' 
#' plot_data <- storms %>%
#'   mutate(status = stringr::str_to_sentence(status)) %>%
#'   group_by(year, status) %>%
#'   summarise(wind = round(mean(wind), 2)) 
#'
#'  ggplot_line_col_facet(plot_data, year, wind, status, status)
#'
ggplot_line_col_facet <-
  function(data,
           x_var,
           y_var,
           col_var,
           facet_var,
           text_var = NULL,
           pal = NULL,
           pal_rev = FALSE,
           size_point = 1,
           size_line = 0.5,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_balance = FALSE,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 5,
           x_title = "[X title]",
           x_title_wrap = 50,
           x_trans = "identity",
           x_zero = FALSE,
           x_zero_line = NULL,
           y_balance = FALSE,
           y_expand = NULL,
           y_labels = waiver(),
           y_pretty_n = 5,
           y_trans = "identity",
           y_title = "[Y title]",
           y_title_wrap = 50,
           y_zero = FALSE,
           y_zero_line = NULL,
           col_na = TRUE,
           col_labels = NULL,
           col_legend_ncol = NULL,
           col_legend_nrow = NULL,
           col_title = "",
           col_title_wrap = 25,
           facet_ncol = NULL,
           facet_nrow = NULL,
           facet_scales = "fixed",
           caption = NULL,
           caption_wrap = 80,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL) {
    
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    data <- data %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(!!x_var) #fix ggplotly legend bug
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!(lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr))) stop("Please use a numeric or date x variable for a line plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a line plot")
    if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a line plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a line plot")
    if(lubridate::is.Date(x_var_vctr) & (x_zero == TRUE | x_balance == TRUE | x_trans != "identity")) {
      stop("x_zero == FALSE, x_balance == FALSE or x_trans other than identity are only allowed when x_var is numeric")
    }
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_line(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    plot <- plot +
      geom_line(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var), size = size_line) +
      geom_point(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var, text = !!text_var),
                 size = size_point, alpha = 1)
    
    if (!is.null(col_labels)) labels <- col_labels
    if (is.null(col_labels)) labels <- waiver()
    
    plot <- plot +
      scale_color_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      )
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    if(facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]

    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    if(facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_y")) {
      
      x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = FALSE)
      x_limits <- c(min(x_breaks), max(x_breaks))
      
      if (lubridate::is.Date(x_var_vctr)) {
        plot <- plot +
          scale_x_date(
            expand = x_expand,
            breaks = x_breaks,
            limits = x_limits,
            labels = x_labels
          )
      }
      else if (is.numeric(x_var_vctr)) {
        plot <- plot +
          scale_x_continuous(expand = x_expand,
                             breaks = x_breaks,
                             limits = x_limits,
                             trans = x_trans,
                             labels = x_labels,
                             oob = scales::rescale_none)
      }
    }
    
    if (facet_scales %in% c("fixed", "free_x")) {
      if (all(y_var_vctr == 0, na.rm = TRUE)) {
        plot <- plot +
          scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
      }
      else ({
        y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
    
    if(y_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }
    
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow) +
      guides(col = guide_legend(ncol = col_legend_ncol, nrow = col_legend_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) 

    return(plot)
  }
