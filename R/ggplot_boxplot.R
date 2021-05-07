#' @title Boxplot ggplot.
#' @description Boxplot ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Defaults to NULL. Required if stat equals "boxplot".
#' @param group_var Unquoted variable to be the grouping variable Defaults to NULL. Only applicable if stat equals "boxplot".
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". If identity is selected, data provided must be grouped by the x_var with ymin, lower, middle, upper, ymax variables. Note "identity" does not provide outliers.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_point The size of the outliers. Defaults to 1.
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
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
#' plot_data <- iris %>%
#' tibble::as_tibble() %>%
#'   mutate(Species = stringr::str_to_sentence(Species))
#'
#' ggplot_boxplot(plot_data, Species, Petal.Length,
#'   title = "Iris petal length by species",
#'   x_title = "Species",
#'   y_title = "Petal length (cm)")
#'   
#' plot_data <- iris %>%
#'   group_by(Species) %>%
#'   summarise(boxplot_stats = list(rlang::set_names(boxplot.stats(Petal.Length)$stats,
#'                                                  c('ymin','lower','middle','upper','ymax')))) %>%
#'  tidyr::unnest_wider(boxplot_stats)
#'
#'ggplot_boxplot(plot_data, Species, Petal.Length, stat = "identity")
ggplot_boxplot <- function(data,
                           x_var,
                           y_var = NULL,
                           group_var = NULL,
                           stat = "boxplot",
                           pal = NULL,
                           width = 0.5,
                           alpha = 1,
                           size_line = 0.5,
                           size_point = 1, 
                           title = "[Title]",
                           title_wrap = 70,
                           subtitle = NULL,
                           subtitle_wrap = 80,
                           x_expand = NULL,
                           x_labels = waiver(),
                           x_title = "[X title]",
                           x_title_wrap = 50,
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
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- sv_pal(1)
  else pal <- pal[1]
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  group_var <- rlang::enquo(group_var)
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  if (stat == "boxplot") y_var_vctr <- dplyr::pull(data, !!y_var)
  else if (stat == "identity") y_var_vctr <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))
  
  if (!(is.character(x_var_vctr) |is.factor(x_var_vctr))) stop("Please use a categorical x variable for a boxplot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot")
  
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme_boxplot(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (stat == "boxplot") {
    if(rlang::quo_is_null(group_var)) {
      plot <- plot +
        geom_boxplot(
          aes(x = !!x_var, y = !!y_var),
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
    else if(!rlang::quo_is_null(group_var)) {
      plot <- plot +
        geom_boxplot(
          aes(x = !!x_var, y = !!y_var, group = !!group_var),
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
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!x_var,
          ymin = .data$ymin,
          lower = .data$lower,
          middle = .data$middle,
          upper = .data$upper,
          ymax = .data$ymax
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
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if(is.null(x_expand)) x_expand <- waiver()
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
  if (mobile == FALSE){
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  else if (mobile == TRUE){
    if(is.character(x_labels)) {
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = function(x) stringr::str_wrap(x_labels, 20))
    }
    else {
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = function(x) stringr::str_wrap(x, 20))
    }
  }

  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    if(mobile == TRUE) {
      y_breaks <- y_limits
      if (min(y_limits) < 0 & max(y_limits > 0)) y_breaks <- c(y_limits[1], 0, y_limits[2])
    }
    
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
      theme_mobile_graph() +
      coord_flip() +
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

#' ggplot boxplot that is coloured
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Defaults to NULL. Required if stat equals "boxplot".
#' @param col_var Unquoted categorical variable to colour the fill of the boxes. Required input.
#' @param group_var Unquoted variable to be the grouping variable Defaults to NULL. Only applicable if stat equals "boxplot".
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". If identity is selected, data provided must be grouped by the x_var with ymin, lower, middle, upper, ymax variables. Note "identity" does not provide outliers.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE. 
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_point The size of the outliers. Defaults to 1.
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_legend_ncol The number of columns in the legend. Defaults to 1.
#' @param col_legend_nrow The number of rows in the legend.
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
#'
#' @return A ggplot object.
#' @export

#' @examples
#' library(dplyr)
#' 
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) 
#' 
#' plot <- ggplot_boxplot_col(plot_data, cut, price, clarity, 
#'                            title = "Average diamond price by cut and clarity", 
#'                            x_title = "Average price ($US thousands)", 
#'                            y_title = "Cut") 
#' 
#' plot
#' 
#' plotly::ggplotly(plot) %>% 
#'   plotly::layout(boxmode = "group") %>% 
#'   plotly_camera()
ggplot_boxplot_col <- function(data,
                               x_var,
                               y_var = NULL,
                               col_var,
                               group_var = NULL,
                               stat = "boxplot",
                               pal = NULL,
                               pal_rev = FALSE,
                               width = 0.5,
                               alpha = 1,
                               size_line = 0.5,
                               size_point = 1, 
                               title = "[Title]",
                               title_wrap = 70,
                               subtitle = NULL,
                               subtitle_wrap = 80,
                               x_expand = NULL,
                               x_labels = waiver(),
                               x_title = "[X title]",
                               x_title_wrap = 50,
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
                               col_rev = FALSE,
                               col_title = "",
                               col_title_wrap = 25,
                               caption = NULL,
                               caption_wrap = 80,
                               font_family = "Helvetica",
                               font_size_title = NULL,
                               font_size_body = NULL,
                               mobile = FALSE) {
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  col_var <- rlang::enquo(col_var) #categorical var
  group_var <- rlang::enquo(group_var)
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  if (stat == "boxplot") y_var_vctr <- dplyr::pull(data, !!y_var)
  else if (stat == "identity") y_var_vctr <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (!(is.character(x_var_vctr) |is.factor(x_var_vctr))) stop("Please use a categorical x variable for a boxplot")
  if (!(is.character(col_var_vctr) |is.factor(col_var_vctr))) stop("Please use a categorical colour variable for a boxplot")
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot")
  
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme_boxplot(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
    n_col <- length(levels(col_var_vctr))
  }
  else n_col <- length(unique(col_var_vctr))
  
  if (is.null(pal)) pal <- sv_pal(n_col)
  else pal <- pal[1:n_col]
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  if (!is.null(col_labels)) labels <- rev(col_labels)
  if (is.null(col_labels)) labels <- waiver()
  
  if (stat == "boxplot") {
    if(rlang::quo_is_null(group_var)) {
      plot <- plot +
        geom_boxplot(
          aes(x = !!x_var, y = !!y_var, fill = !!col_var),
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
    else if(!rlang::quo_is_null(group_var)) {
      plot <- plot +
        geom_boxplot(
          aes(x = !!x_var, y = !!y_var, fill = !!col_var, group = !!group_var),
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
  }
  else if (stat == "identity") {
    plot <- plot +
      geom_boxplot(
        aes(
          x = !!x_var,
          ymin = .data$ymin,
          lower = .data$lower,
          middle = .data$middle,
          upper = .data$upper,
          ymax = .data$ymax,
          fill = !!col_var
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
  
  y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
  y_zero <- y_zero_list[[1]]
  y_zero_line <- y_zero_list[[2]]
  
  if(is.null(x_expand)) x_expand <- waiver()
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
  if (mobile == FALSE){
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  }
  else if (mobile == TRUE){
    if(is.character(x_labels)) {
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = function(x) stringr::str_wrap(x_labels, 20))
    }
    else {
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = function(x) stringr::str_wrap(x, 20))
    }
  }
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = y_trans, y_zero = y_zero)
    y_limits <- c(min(y_breaks), max(y_breaks))
    
    if(mobile == TRUE) {
      y_breaks <- y_limits
      if (min(y_limits) < 0 & max(y_limits > 0)) y_breaks <- c(y_limits[1], 0, y_limits[2])
    }
    
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
  
  plot <- plot +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = labels,
      na.translate = col_na,
      na.value = "#A8A8A8"
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
      guides(fill = guide_legend(
        ncol = col_legend_ncol,
        nrow = col_legend_nrow,
        byrow = TRUE,
        title = stringr::str_wrap(col_title, col_title_wrap)
      )) 
    
  }
  else if (mobile == TRUE){
    plot <- plot +
      theme_mobile_graph() +
      coord_flip() +
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
      guides(fill = guide_legend(
        ncol = col_legend_ncol, 
        nrow = col_legend_nrow, 
        byrow = TRUE,
        reverse = TRUE,
        title = stringr::str_wrap(col_title, col_title_wrap)
      )) 
    
  }
  
  return(plot)
}

#' @title Boxplot ggplot that is facetted.
#' @description Boxplot ggplot that is facetted, but not coloured.
#' @param data An tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Defaults to NULL. Required if stat equals "boxplot".
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param group_var Unquoted variable to be the grouping variable Defaults to NULL. Only applicable if stat equals "boxplot".
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". If identity is selected, data provided must be grouped by the x_var and facet_var with ymin, lower, middle, upper, ymax variables. Note "identity" does not provide outliers.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param size_point The size of the outliers. Defaults to 1.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale. Only applicable where facet_scales equals "fixed" or "free_x".
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
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
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(price_thousands = (price / 1000)) 
#'
#' ggplot_boxplot_facet(plot_data, cut, price_thousands, color)
ggplot_boxplot_facet <-
  function(data,
           x_var,
           y_var = NULL,
           facet_var,
           group_var = NULL, 
           stat = "boxplot",
           pal = NULL,
           width = 0.5,
           alpha = 1,
           size_line = 0.5,
           size_point = 1,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_expand = NULL,
           x_labels = waiver(),
           x_title = "[X title]",
           x_title_wrap = 50,
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
    x_var <- rlang::enquo(x_var) 
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    group_var <- rlang::enquo(group_var) 
    
    x_var_vctr <- dplyr::pull(data, !!x_var) 
    if (stat == "boxplot") y_var_vctr <- dplyr::pull(data, !!y_var)
    else if (stat == "identity") y_var_vctr <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!(is.character(x_var_vctr) | is.factor(x_var_vctr))) stop("Please use a categorical x variable for a boxplot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a boxplot")
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (is.null(pal)) pal <- sv_pal(1)
    else pal <- pal[1]
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_boxplot(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    if (stat == "boxplot") {
      if(rlang::quo_is_null(group_var)) {
        plot <- plot +
          geom_boxplot(
            aes(x = !!x_var, y = !!y_var),
            stat = stat,
            col = "#323232", 
            fill = pal,
            width = width,
            size = size_line, 
            alpha = alpha,
            outlier.alpha = 1
          )
      }
      else if(!rlang::quo_is_null(group_var)) {
        plot <- plot +
          geom_boxplot(
            aes(x = !!x_var, y = !!y_var, group = !!group_var),
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
    }
    else if (stat == "identity") {
      plot <- ggplot(data) +
        coord_cartesian(clip = "off") +
        theme_boxplot(
          font_family = font_family,
          font_size_body = font_size_body,
          font_size_title = font_size_title
        ) +
        geom_boxplot(
          aes(
            x = !!x_var,
            ymin = .data$ymin,
            lower = .data$lower,
            middle = .data$middle,
            upper = .data$upper,
            ymax = .data$ymax
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
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    if(facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- waiver()
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_y")) {
        plot <- plot +
          scale_x_discrete(expand = x_expand, labels = x_labels)
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
      facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow, ncol = facet_ncol)

    return(plot)
  }

#' ggplot boxplot that is coloured
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Defaults to NULL. Required if stat equals "boxplot".
#' @param col_var Unquoted categorical variable to colour the fill of the boxes. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param group_var Unquoted variable to be the grouping variable Defaults to NULL. Only applicable if stat equals "boxplot".
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". If identity is selected, data provided must be grouped by the x_var with ymin, lower, middle, upper, ymax variables. Note "identity" does not provide outliers.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE. 
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_point The size of the outliers. Defaults to 1.
#' @param size_line The size of the outlines of boxplots. Defaults to 0.5.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param y_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to FALSE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.  
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_legend_ncol The number of columns in the legend. Defaults to 1.
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether the colour scale is reversed. Defaults to FALSE. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   filter(cut %in% c("Good", "Ideal")) %>%
#'   filter(color %in% c("D", "E")) %>%
#'   filter(clarity %in% c("I1", "IF", "SI1", "VS1")) %>%
#'   mutate(across(c("color", "cut", "clarity"), ~as.character(.x)))
#' 
#' plot <- ggplot_boxplot_col_facet(plot_data, cut, price, clarity, color)
#' 
#' plot
#' 
#' plotly::ggplotly(plot) %>% 
#'   plotly::layout(boxmode = "group") %>% 
#'   plotly_camera()
ggplot_boxplot_col_facet <-
  function(data,
           x_var,
           y_var = NULL,
           col_var,
           facet_var,
           group_var = NULL, 
           stat = "boxplot",
           pal = NULL,
           pal_rev = FALSE,
           width = 0.5,
           alpha = 1,
           size_line = 0.5,
           size_point = 1,
           title = "[Title]",
           title_wrap = 70,
           subtitle = NULL,
           subtitle_wrap = 80,
           x_expand = NULL,
           x_labels = waiver(),
           x_title = "[X title]",
           x_title_wrap = 50,
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
    x_var <- rlang::enquo(x_var) 
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    group_var <- rlang::enquo(group_var) 
    
    x_var_vctr <- dplyr::pull(data, !!x_var) 
    if (stat == "boxplot") y_var_vctr <- dplyr::pull(data, !!y_var)
    else if (stat == "identity") y_var_vctr <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))
    col_var_vctr <- dplyr::pull(data, !!col_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!(is.character(x_var_vctr) | is.factor(x_var_vctr))) stop("Please use a categorical x variable for a boxplot")
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot")
    if (!(is.character(col_var_vctr) |is.factor(col_var_vctr))) stop("Please use a categorical colour variable for a boxplot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a boxplot")
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_boxplot(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    if (!is.null(col_labels)) labels <- rev(col_labels)
    if (is.null(col_labels)) labels <- waiver()
    
    if (stat == "boxplot") {
      if(rlang::quo_is_null(group_var)) {
        plot <- plot +
          geom_boxplot(
            aes(x = !!x_var, y = !!y_var, fill = !!col_var),
            stat = stat,
            col = "#323232", 
            width = width,
            size = size_line, 
            alpha = alpha,
            outlier.alpha = 1
          )
      }
      else if(!rlang::quo_is_null(group_var)) {
        plot <- plot +
          geom_boxplot(
            aes(x = !!x_var, y = !!y_var, fill = !!col_var, group = !!group_var),
            stat = stat,
            col = "#323232", 
            width = width,
            size = size_line, 
            alpha = alpha,
            outlier.alpha = 1, 
            outlier.size = size_point
          )
      }
    }
    else if (stat == "identity") {
      plot <- ggplot(data) +
        coord_cartesian(clip = "off") +
        theme_boxplot(
          font_family = font_family,
          font_size_body = font_size_body,
          font_size_title = font_size_title
        ) +
        geom_boxplot(
          aes(
            x = !!x_var,
            ymin = .data$ymin,
            lower = .data$lower,
            middle = .data$middle,
            upper = .data$upper,
            ymax = .data$ymax,
            fill = !!col_var
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
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    if(facet_scales %in% c("fixed", "free_x")) y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- waiver()
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_y")) {
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = x_labels)
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
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow, ncol = facet_ncol) +
      guides(fill = guide_legend(
        ncol = col_legend_ncol,
        nrow = col_legend_nrow,
        byrow = TRUE,
        title = stringr::str_wrap(col_title, col_title_wrap)
      )) 
    
    return(plot)
  }
