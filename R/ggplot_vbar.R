#' @title Vertical bar ggplot.
#' @description Vertical bar ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where mobile equals TRUE.
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
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
#'   summarise(average_wind = round(mean(wind), 2)) 
#'
#' ggplot_vbar(plot_data, year, average_wind,
#'       title = "Average wind speed of Atlantic storms, 1975-2015",
#'       x_title = "Year",
#'       y_title = "Average maximum sustained wind speed (knots)")
#'
ggplot_vbar <- function(data,
                        x_var,
                        y_var,
                        text_var = NULL,
                        pal = NULL,
                        width = 0.75, 
                        alpha = 1,
                        size_line = 0.5,
                        title = "[Title]",
                        title_wrap = 70,
                        subtitle = NULL,
                        subtitle_wrap = 80,
                        x_expand = NULL,
                        x_labels = waiver(),
                        x_pretty_n = 6,
                        x_title = "[X title]",
                        x_title_wrap = 50,
                        y_balance = FALSE,
                        y_expand = NULL,
                        y_labels = waiver(),
                        y_pretty_n = 5,
                        y_title = "[Y title]",
                        y_title_wrap = 50,
                        y_trans = "identity",
                        y_zero = TRUE,
                        y_zero_line = NULL,
                        caption = NULL,
                        caption_wrap = 80,
                        font_family = "Helvetica",
                        font_size_title = NULL,
                        font_size_body = NULL,
                        mobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  if (lubridate::is.Date(x_var_vctr)) stop("Please do not use a logical x variable for a vertical bar plot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- sv_pal(1)
  else pal <- pal[1]
  
  if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  plot <- ggplot(data) +
    theme_vbar(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_col(aes(x = !!x_var, y = !!y_var, text = !!text_var), 
             col = pal, 
             fill = pal, 
             alpha = alpha, 
             size = size_line, 
             width = bar_width)
  
  if (lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr)) {
    
    x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
    x_limits <- c(min(x_var_vctr), max(x_var_vctr))
    if(is.null(x_expand)) x_expand <- c(0.5 / (length(x_var_vctr) - 1) * width, 0)
    
    if(mobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
    }
  }
  
  if (lubridate::is.Date(x_var_vctr)) {
    plot <- plot +
      coord_cartesian(xlim = c(x_limits[1], x_limits[2])) +
      scale_x_date(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels
      )
  }
  else if (is.numeric(x_var_vctr)) {
    plot <- plot +
      coord_cartesian(xlim = c(x_limits[1], x_limits[2])) +
      scale_x_continuous(expand = x_expand,
                         breaks = x_breaks,
                         labels = x_labels,
                         oob = scales::squish)
  }
  else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if(is.null(x_expand)) x_expand <- c(0, 0)
    
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
    y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where mobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_labels_ncol The number of columns in the legend. 
#' @param col_labels_nrow The number of rows in the legend.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. Defaults to FALSE.
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
#'   summarise(average_wind = round(mean(wind), 2)) 
#'
#' ggplot_vbar_col(plot_data, year, average_wind, status)
#'
ggplot_vbar_col <-
  function(data,
           x_var,
           y_var,
           col_var,
           text_var = NULL,
           position = "stack",
           pal = NULL,
           pal_rev = FALSE,
           width = 0.75, 
           alpha = 1,
           size_line = 0.5,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 6,
           x_title = "[X title]",
           x_title_wrap = 50,
           y_balance = FALSE,
           y_expand = NULL,
           y_labels = waiver(),
           y_pretty_n = 5,
           y_title = "[Y title]",
           y_title_wrap = 50,
           y_trans = "identity",
           y_zero = TRUE,
           y_zero_line = NULL,
           col_labels = NULL,
           col_labels_ncol = NULL,
           col_labels_nrow = NULL,
           col_na = TRUE,
           col_rev = FALSE,
           col_title = "",
           col_title_wrap = 25,
           caption = NULL,
           caption_wrap = 80,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           mobile = FALSE) {
    
    data <- dplyr::ungroup(data)
    y_var <- rlang::enquo(y_var) #numeric var
    x_var <- rlang::enquo(x_var) #categorical var
    col_var <- rlang::enquo(col_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
    x_var_vctr <- dplyr::pull(data, !!x_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if (lubridate::is.Date(x_var_vctr)) stop("Please do not use a logical x variable for a vertical bar plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(col_var_vctr) | is.logical(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal bar plot")
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
    
    if (col_rev == TRUE){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
    }
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if (position == "stack" & y_trans != "identity") message("simplevis may not perform correctly using a y scale other than identity where position equals stack")
    if (position == "stack" & y_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and y_zero equal to FALSE")
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
    else bar_unit <- 1
    
    bar_width <- bar_unit * width
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    if (!is.null(col_labels)) labels <- rev(col_labels)
    if (is.null(col_labels)) labels <- waiver()
    
    plot <- ggplot(data) +
      theme_vbar(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
      geom_col(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
               alpha = alpha, 
               size = size_line, 
               width = bar_width, 
               position = position2)
    
    if (lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr)) {
      
      x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
      if(is.null(x_expand)) x_expand <- waiver()
      
      if(mobile == TRUE) {
        x_limits <- c(min(x_var_vctr), max(x_var_vctr))
        x_breaks <- x_limits
        if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
      }
    }
    
    if (lubridate::is.Date(x_var_vctr)) {
      plot <- plot +
        scale_x_date(
          expand = x_expand,
          breaks = x_breaks,
          labels = x_labels
        )
    }
    else if (is.numeric(x_var_vctr)) {
      plot <- plot +
        scale_x_continuous(expand = x_expand,
                           breaks = x_breaks,
                           labels = x_labels,
                           oob = scales::squish)
    }
    else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
      if(is.null(x_expand)) x_expand <- c(0, 0)
      
      plot <- plot +
        coord_cartesian() +
        scale_x_discrete(expand = x_expand, labels = x_labels)
    }
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by(dplyr::across(!!x_var)) %>%
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
      y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
    
    plot <- plot +
      geom_col(aes(
        x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
        alpha = alpha, size = size_line, width = width, 
        position = position2)

    if(y_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      ) +
      scale_colour_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
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
          ncol = col_labels_ncol,
          byrow = TRUE,
          title = stringr::str_wrap(col_title, col_title_wrap)
        ), 
        col = guide_legend(
          ncol = col_labels_ncol, nrow = col_labels_nrow, 
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
          fill = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 15)),
          col = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 15))
        ) +
        theme_mobile_graph()
    }
    
    return(plot)
  }

#' @title Vertical bar ggplot that is facetted.
#' @description Vertical bar ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1.
#' @param size_line The size of the outlines of bars. 
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale. Only applicable where facet_scales equals "fixed" or "free_x".
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults NULL.
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
#'   summarise(average_wind = round(mean(wind), 2)) 
#'
#' ggplot_vbar_facet(plot_data, year, average_wind, status)
#'
ggplot_vbar_facet <-
  function(data,
           x_var,
           y_var,
           facet_var,
           text_var = NULL,
           pal = NULL,
           width = 0.75, 
           alpha = 1,
           size_line = 0.5,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 5,
           x_title = "[X title]",
           x_title_wrap = 50,
           y_balance = FALSE,
           y_expand = NULL,
           y_labels = waiver(),
           y_pretty_n = 5,
           y_title = "[Y title]",
           y_title_wrap = 50,
           y_trans = "identity",
           y_zero = TRUE,
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
    x_var <- rlang::enquo(x_var) #categorical var
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (lubridate::is.Date(x_var_vctr)) stop("Please do not use a logical x variable for a vertical bar plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a vertical bar plot")
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (is.null(pal)) pal <- sv_pal(1)
    else pal <- pal[1]

    if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
    else bar_unit <- 1
    
    bar_width <- bar_unit * width
    
    plot <- ggplot(data) +
      theme_vbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(x = !!x_var, y = !!y_var, text = !!text_var), col = pal, fill = pal, alpha = alpha, size = size_line, width = bar_width)
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if (lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr)) {
        
        x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
        x_limits <- c(min(x_var_vctr), max(x_var_vctr))
        if(is.null(x_expand)) x_expand <- c(0.5 / (length(x_var_vctr) - 1) * width, 0)
      }
      
      if (lubridate::is.Date(x_var_vctr)) {
        plot <- plot +
          coord_cartesian(xlim = c(x_limits[1], x_limits[2])) +
          scale_x_date(
            expand = x_expand,
            breaks = x_breaks,
            labels = x_labels
          )
      }
      else if (is.numeric(x_var_vctr)) {
        plot <- plot +
          coord_cartesian(xlim = c(x_limits[1], x_limits[2])) +
          scale_x_continuous(expand = x_expand,
                             breaks = x_breaks,
                             labels = x_labels,
                             oob = scales::squish)
      }
      else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
        if(is.null(x_expand)) x_expand <- c(0, 0)
        
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
        y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
    
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow) +
      theme(axis.text.x = element_text(hjust = 0.75))

    return(plot)
  }

#' @title Vertical bar ggplot that is coloured and facetted.
#' @description Vertical bar ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale. Only applicable where facet_scales equals "fixed" or "free_x".
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_labels_ncol The number of columns in the legend. 
#' @param col_labels_nrow The number of rows in the legend.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. Defaults to FALSE.
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
#'   group_by(year, status, name) %>%
#'   summarise(average_wind = round(mean(wind), 2)) %>%
#'   filter(year %in% 1975:1980) %>%
#'   filter(!(status == "Tropical storm" & year == 1980)) %>%
#'   filter(name %in% c("Karl", "Juliet", "Jeanne", "Ivan", "Hermine",
#'   "Henri", "Gloria", "Georges", "Frederic"))
#'
#'   ggplot_vbar_col_facet(plot_data, year, average_wind, name, status)
#'
ggplot_vbar_col_facet <-
  function(data,
           x_var,
           y_var,
           col_var,
           facet_var,
           text_var = NULL,
           position = "stack",
           pal = NULL,
           pal_rev = FALSE,
           width = 0.75, 
           alpha = 1,
           size_line = 0.5,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_labels = waiver(),
           x_pretty_n = 5,
           x_expand = NULL,
           x_title = "[X title]",
           x_title_wrap = 50,
           y_balance = FALSE,
           y_expand = NULL,
           y_labels = waiver(),
           y_pretty_n = 5,
           y_title = "[Y title]",
           y_title_wrap = 50,
           y_trans = "identity",
           y_zero = TRUE,
           y_zero_line = NULL,
           col_labels = NULL,
           col_labels_ncol = NULL,
           col_labels_nrow = NULL,
           col_na = TRUE,
           col_rev = FALSE,
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
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #categorical var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (lubridate::is.Date(x_var_vctr)) stop("Please do not use a logical x variable for a vertical bar plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(col_var_vctr) | is.logical(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a vertical bar plot")
    
    if (col_rev == TRUE){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
    }
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (position == "stack" & y_trans != "identity") message("simplevis may not perform correctly using a y scale other than identity where position equals stack")
    if (position == "stack" & y_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and y_zero equal to FALSE")
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
    else bar_unit <- 1
    
    bar_width <- bar_unit * width
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (!is.null(col_labels)) labels <- rev(col_labels)
    if (is.null(col_labels)) labels <- waiver()
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      coord_cartesian() +
      theme_vbar(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
      geom_col(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
               alpha = alpha, 
               size = size_line, 
               width = bar_width, 
               position = position2)
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by(dplyr::across(c(!!x_var, !!facet_var))) %>%
        dplyr::summarise(dplyr::across(!!y_var, ~sum(.x, na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      y_var_vctr <- dplyr::pull(data_sum, !!y_var)
    }
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if (lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr)) {
        x_breaks <- pretty(x_var_vctr, n = x_pretty_n)

        if(is.null(x_expand)) x_expand <- waiver()
      }
      if (lubridate::is.Date(x_var_vctr)) {
        plot <- plot +
          scale_x_date(
            expand = x_expand,
            breaks = x_breaks,
            labels = x_labels
          )
      }
      else if (is.numeric(x_var_vctr)) {
        plot <- plot +
          scale_x_continuous(expand = x_expand,
                             breaks = x_breaks,
                             labels = x_labels,
                             oob = scales::squish)
      }
      else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
        if(is.null(x_expand)) x_expand <- c(0, 0)
          
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
        y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
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
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      ) +
      scale_colour_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      )
    
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
      guides(fill = guide_legend(
        ncol = col_labels_ncol, nrow = col_labels_nrow, 
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
