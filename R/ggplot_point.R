#' @title Point ggplot.
#' @description Point ggplot that is not coloured and not facetted.
#' @param data An ungrouped summarised tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param size_point Size of points. Defaults to 1.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param title  Title string. Defaults to "[Title]".
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
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.     
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_trans A string specifying a transformation for the y scale. Defaults to "identity".
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
#' plot_data <- slice_sample(ggplot2::diamonds, prop = 0.05)
#'
#' ggplot_point(plot_data, carat, price, 
#'    title = "Diamond price by carat",
#'    x_title = "Carat",
#'    y_title = "Price ($US thousands)")
#'
ggplot_point <- function(data,
                         x_var,
                         y_var,
                         text_var = NULL,
                         size_point = 1,
                         pal = NULL, 
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
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a point plot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- sv_pal(1)
  else pal <- pal[1]

  plot <- ggplot(data) +
    theme_point(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    coord_cartesian(clip = "off") +
    geom_point(aes(!!x_var, !!y_var, text = !!text_var), col = pal[1], size = size_point)
  
  x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
  x_zero <- x_zero_list[[1]]
  x_zero_line <- x_zero_list[[2]]

  x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = mobile)
  x_limits <- c(min(x_breaks), max(x_breaks))
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
  y_limits <- c(min(y_breaks), max(y_breaks))

  if(is.null(x_expand)) x_expand <- c(0, 0)
  if(is.null(y_expand)) y_expand <- c(0, 0)

  plot <- plot +
    scale_x_continuous(
      expand = x_expand,
      breaks = x_breaks,
      limits = x_limits,
      trans = x_trans,
      labels = x_labels,
      oob = scales::rescale_none
    ) +
    scale_y_continuous(
      expand = y_expand,
      breaks = y_breaks,
      limits = y_limits,
      trans = y_trans,
      labels = y_labels,
      oob = scales::rescale_none
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

#' @title Point ggplot that is coloured.
#' @description Point ggplot that is coloured, but not facetted.
#' @param data An ungrouped summarised tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param size_point Size of points. Defaults to 1.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param title  Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "quantile".
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where mobile equals TRUE.
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.    
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_trans A string specifying a transformation for the y scale. Defaults to "identity".
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.
#' @param col_labels_dp Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param col_labels Adjust the colour scale labels through a vector.
#' @param col_labels_ncol The number of columns in the legend. 
#' @param col_labels_nrow The number of rows in the legend.
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
#' plot_data <- slice_sample(ggplot2::diamonds, prop = 0.05)
#'
#' ggplot_point_col(plot_data, carat, price, color)
#'
ggplot_point_col <-
  function(data,
           x_var,
           y_var,
           col_var,
           text_var = NULL,
           size_point = 1,
           pal = NULL,
           pal_rev = FALSE,
           col_method = NULL,
           col_cuts = NULL,
           col_na = TRUE,
           x_balance = FALSE,
           x_zero = FALSE,
           x_zero_line = NULL,
           x_trans = "identity",
           x_labels = waiver(),
           x_pretty_n = 6,
           x_expand = NULL,
           y_balance = FALSE, 
           y_expand = NULL,
           y_labels = waiver(),
           y_pretty_n = 5,
           y_trans = "identity",
           y_zero = FALSE,
           y_zero_line = NULL,
           title = "[Title]",
           subtitle = NULL,
           x_title = "[X title]",
           y_title = "[Y title]",
           col_title = "",
           caption = NULL,
           col_labels = NULL,
           col_labels_ncol = NULL,
           col_labels_nrow = NULL,
           col_labels_dp = 1,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           title_wrap = 70,
           subtitle_wrap = 80,
           x_title_wrap = 50,
           y_title_wrap = 50,
           col_title_wrap = 25,
           caption_wrap = 80,
           mobile = FALSE) {
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var)
    text_var <- rlang::enquo(text_var)
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a point plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
    
    if (is.null(col_method)) {
      if (!is.numeric(col_var_vctr)) col_method <- "category"
      else if (is.numeric(col_var_vctr)) col_method <- "quantile"
    }
    
    if(col_method %in% c("quantile", "bin")) {
      if (col_method == "quantile") {
        if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
        else {
          if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
          if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
        }  
        col_cuts <- quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
        if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
        
        data <- data %>% 
          dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
        
        if (is.null(col_labels)) labels <- sv_labels_from_cuts(col_cuts, col_labels_dp)
        else labels <- col_labels
      }
      else if (col_method == "bin") {
        if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
        else({
          if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
          if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
        })
        
        data <- data %>% 
          dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
        
        if (is.null(col_labels)) labels <- sv_labels_from_cuts(col_cuts, col_labels_dp)
        else labels <- col_labels
      }
      n_col <- length(col_cuts) - 1
      if (is.null(pal)) pal <- sv_pal(n_col)
      else pal <- pal[1:n_col]
    }
    else if (col_method == "category") {
      if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
        n_col <- length(levels(col_var_vctr))
      }
      else n_col <- length(unique(col_var_vctr))
      
      if (is.null(pal)) pal <- sv_pal(n_col)
      else pal <- pal[1:n_col]
      
      if (is.null(col_labels)) labels <- waiver()
      else labels <- col_labels
    }
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      theme_point(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      coord_cartesian(clip = "off")
    
    plot <- plot +
      geom_point(aes(x = !!x_var, y = !!y_var, col = !!col_var, text = !!text_var), size = size_point)
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = mobile)
    x_limits <- c(min(x_breaks), max(x_breaks))
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    plot <- plot +
      scale_color_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      ) +
      scale_x_continuous(
        expand = x_expand,
        breaks = x_breaks,
        limits = x_limits,
        trans = x_trans,
        labels = x_labels,
        oob = scales::rescale_none
      ) +
      scale_y_continuous(
        expand = y_expand,
        breaks = y_breaks,
        limits = y_limits,
        trans = y_trans,
        labels = y_labels,
        oob = scales::rescale_none
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
        guides(col = guide_legend(ncol = col_labels_ncol, nrow = col_labels_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap)))
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

#' @title Point ggplot that is facetted.
#' @description Point ggplot that is facetted, but not coloured.
#' @param data An ungrouped summarised tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param size_point Size of points. Defaults to 1.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param title  Title string. Defaults to "[Title]".
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
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.    
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans A string specifying a transformation for the y scale. Defaults to "identity".
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
#' plot_data <- slice_sample(ggplot2::diamonds, prop = 0.05)
#'
#' ggplot_point_facet(plot_data, carat, price, color)
#'
ggplot_point_facet <-
  function(data,
           x_var,
           y_var,
           facet_var,
           text_var = NULL,
           size_point = 1,
           pal = NULL,
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
    
    if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a point plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a point plot")
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (is.null(pal)) pal <- sv_pal(1)
    else pal <- pal[1]

    plot <- ggplot(data) +
      theme_point(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      coord_cartesian(clip = "off") +
      geom_point(aes(x = !!x_var, y = !!y_var, text = !!text_var), col = pal[1], size = size_point)
    
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

      plot <- plot +
        scale_x_continuous(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          trans = x_trans,
          labels = x_labels,
          oob = scales::rescale_none
        )
    }
    if (facet_scales %in% c("fixed", "free_x")) {
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

#' @title Point ggplot that is coloured and facetted.
#' @description Point ggplot that is coloured and facetted.
#' @param data An ungrouped summarised tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted variable for points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param size_point Size of points. Defaults to 1.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param title  Title string. Defaults to "[Title]".
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
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.    
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans A string specifying a transformation for the y scale. Defaults to "identity".
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels_dp Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param col_labels_ncol The number of columns in the legend. 
#' @param col_labels_nrow The number of rows in the legend.
#' @param col_labels Adjust the colour scale labels through a vector.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "quantile".
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_quantile_by_facet TRUE of FALSE whether quantiles should be calculated for each group of the facet variable. Defaults to TRUE.
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
#' plot_data <- ggplot2::diamonds %>%
#'   sample_frac(0.05) %>%
#'   mutate(cut = stringr::str_to_sentence(cut))
#'
#' ggplot_point_col_facet(plot_data, carat, price, color, cut)
#'
ggplot_point_col_facet <-
  function(data,
           x_var,
           y_var,
           col_var,
           facet_var,
           text_var = NULL,
           size_point = 1,
           pal = NULL,
           pal_rev = FALSE,
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
           col_cuts = NULL,
           col_labels_dp = 1,
           col_labels = NULL,
           col_labels_ncol = NULL,
           col_labels_nrow = NULL,
           col_method = NULL,
           col_na = TRUE,
           col_quantile_by_facet = TRUE,
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
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var)
    facet_var <- rlang::enquo(facet_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a point plot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a point plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a point plot")
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (is.null(col_method)) {
      if (!is.numeric(col_var_vctr)) col_method <- "category"
      if (is.numeric(col_var_vctr)) col_method <- "quantile"      
    }
    
    if(col_method %in% c("quantile", "bin")) {
      if (col_method == "quantile") {
        if (is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
        else {
          if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
          if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
        }  
        
        if (col_quantile_by_facet == TRUE) {
          data <- data %>%
            dplyr::group_by(!!facet_var) %>%
            dplyr::mutate(dplyr::across(!!col_var, ~percent_rank(.x))) %>%
            dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts)))
          
          if (is.null(col_labels)) labels <- paste0(sv_labels_from_cuts(col_cuts * 100, 0), "%")
          else labels <- col_labels
        }
        else if (col_quantile_by_facet == FALSE) { 
          col_cuts <- quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
          if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
          
          data <- data %>% 
            dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
          
          if (is.null(col_labels)) labels <- sv_labels_from_cuts(col_cuts, col_labels_dp)
          else labels <- col_labels
        }
      }
      else if (col_method == "bin") {
        if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
        else({
          if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
          if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
        })
        
        data <- dplyr::mutate(data, dplyr::across(!!col_var, ~cut(.x, col_cuts)))
        
        if (is.null(col_labels)) labels <- sv_labels_from_cuts(col_cuts, col_labels_dp)
        else labels <- col_labels
      }
      n_col <- length(col_cuts) - 1
      if (is.null(pal)) pal <- sv_pal(n_col)
      else pal <- pal[1:n_col]
    } 
    else if (col_method == "category") {
      if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
        n_col <- length(levels(col_var_vctr))
      }
      else n_col <- length(unique(col_var_vctr))
      
      if (is.null(pal)) pal <- sv_pal(n_col)
      else pal <- pal[1:n_col]
      
      if (!is.null(col_labels)) labels <- col_labels
      else labels <- waiver()
    }
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      theme_point(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      coord_cartesian(clip = "off") +
      geom_point(aes(x = !!x_var, y = !!y_var, col = !!col_var, text = !!text_var), size = size_point)
    
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
      
      plot <- plot +
        scale_x_continuous(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          trans = x_trans,
          labels = x_labels,
          oob = scales::rescale_none
        )
    }
    if (facet_scales %in% c("fixed", "free_x")) {
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
      guides(col = guide_legend(ncol = col_labels_ncol, nrow = col_labels_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) +
      facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)

    
    return(plot)
  }
.0