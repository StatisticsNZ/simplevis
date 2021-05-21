#' @title Horizontal bar ggplot.
#' @description Horizontal bar ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the y scale. Required input.
#' @param y_var Unquoted numeric, date or categorical variable to be on the x scale. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.
#' @param x_expand Adjust the vector of range expansion constants used to add some padding on the y scale. 
#' @param x_labels Adjust the y scale labels through a function that takes the breaks as input and returns labels as output.
#' @param x_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title Y scale title string. Defaults to [Y title].
#' @param x_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param x_trans For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param x_zero_line For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.  
#' @param y_balance For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.
#' @param y_expand Adjust the vector of range expansion constants used to add some padding on the x scale. 
#' @param y_labels Adjust the x scale labels through a function that takes the breaks as input and returns labels as output.
#' @param y_pretty_n For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6. 
#' @param y_reorder For a categorical x variable, TRUE or FALSE of whether the y variable variable is to be reordered by the x variable. Defaults to FALSE.
#' @param y_rev For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param y_title X scale title string. Defaults to "[X title]".
#' @param y_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_zero For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.
#' @param y_zero_line For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.   
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
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
#' ggplot_hbar(plot_data, average_wind, year)
ggplot_hbar <- function(data,
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
                        y_balance = FALSE,
                        y_expand = NULL,
                        y_labels = waiver(),
                        y_pretty_n = 6,
                        y_reorder = FALSE,
                        y_rev = FALSE,
                        y_title = "[X title]",
                        y_title_wrap = 50,
                        y_zero = FALSE,
                        y_zero_line = NULL,
                        x_balance = FALSE,
                        x_expand = NULL,
                        x_labels = waiver(),
                        x_pretty_n = 5,
                        x_title = "[Y title]",
                        x_title_wrap = 50,
                        x_trans = "identity",
                        x_zero = TRUE,
                        x_zero_line = NULL,
                        caption = NULL,
                        caption_wrap = 80,
                        font_family = "Helvetica",
                        font_size_title = NULL,
                        font_size_body = NULL,
                        mobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  y_var <- rlang::enquo(y_var)
  x_var <- rlang::enquo(x_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
  
  if (y_reorder == TRUE) {
    if(y_rev == FALSE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_reorder(.x, !!x_var, .desc = TRUE)))
    } 
    else if(y_rev == TRUE) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_reorder(.x, !!x_var, .desc = FALSE)))
    } 
    y_var_vctr <- dplyr::pull(data, !!y_var)
  } 
  else if (y_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  
  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (is.null(pal)) pal <- sv_pal(1)
  else pal <- pal[1]
  
  if (lubridate::is.Date(y_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  plot <- ggplot(data) +
    theme_hbar(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_col(aes(x = !!y_var, y = !!x_var, text = !!text_var), 
             col = pal, 
             fill = pal, 
             alpha = alpha, 
             size = size_line, 
             width = bar_width)
  
  if (lubridate::is.Date(y_var_vctr) | is.numeric(y_var_vctr)) {
    
    y_zero_list <- sv_y_zero_adjust(y_var_vctr, y_balance = y_balance, y_zero = y_zero, y_zero_line = y_zero_line)
    y_zero <- y_zero_list[[1]]
    y_zero_line <- y_zero_list[[2]]
    
    y_breaks <- y_numeric_breaks(y_var_vctr, y_balance = y_balance, y_pretty_n = y_pretty_n, y_trans = "identity", y_zero = y_zero, mobile = mobile)
    if(y_zero == FALSE & y_balance == FALSE) {
      y_limits <- c(min(y_var_vctr), max(y_var_vctr))
    } else y_limits <- c(min(y_breaks), max(y_breaks))
    
    if(is.null(y_expand)) y_expand <- c(0.5 / (length(y_var_vctr) - 1) * width, 0)
    
    if(mobile == TRUE) {
      y_breaks <- y_limits
      if (min(y_limits) < 0 & max(y_limits > 0)) y_breaks <- c(y_limits[1], 0, y_limits[2])
    }
  }
  
  if (lubridate::is.Date(y_var_vctr)) {
    plot <- plot +
      coord_flip(xlim = c(y_limits[2], y_limits[1])) +
      scale_x_date(
        expand = y_expand,
        breaks = rev(y_breaks),
        labels = y_labels
      )
  }
  else if (is.numeric(y_var_vctr)) {
    plot <- plot +
      coord_flip(xlim = c(y_limits[2], y_limits[1])) +
      scale_x_reverse(expand = y_expand,
                      breaks = rev(y_breaks),
                      labels = y_labels,
                      oob = scales::squish) 

    if(y_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
  }
  else if (is.character(y_var_vctr) | is.factor(y_var_vctr)){
    if(is.null(y_expand)) y_expand <- waiver()
    
    if (mobile == FALSE){
      plot <- plot +
        coord_flip() +
        scale_x_discrete(expand = y_expand, labels = y_labels)
    }
    else if (mobile == TRUE){
      if(is.character(y_labels)) {
        plot <- plot +
          coord_flip() +
          scale_x_discrete(expand = y_expand, labels = function(x) stringr::str_wrap(y_labels, 20))
      }
      else {
        plot <- plot +
          coord_flip() +
          scale_x_discrete(expand = y_expand, labels = function(x) stringr::str_wrap(x, 20))
      }
    }
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
    x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero)
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
      theme_mobile_graph()
  }
  
  return(plot)
}

#' @title Horizontal bar ggplot that is coloured.
#' @description Horizontal bar ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted categorical variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev TRUE or FALSE of whether to reverse the pal.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where mobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where mobile equals TRUE.
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the x scale labels through a function that takes the breaks as input and returns labels as output.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where mobile equals TRUE.
#' @param x_trans A string specifying a transformation for the x axis scale. Defaults to "identity".
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the y scale labels through a function that takes the breaks as input and returns labels as output.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where mobile equals TRUE.
#' @param col_labels Adjust the  x scale labels through a vector.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether bar fill order from left to right is reversed from default. Defaults to FALSE.
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
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   group_by(cut, clarity) %>%
#'   summarise(average_price = mean(price)) %>%
#'   mutate(average_price = round(average_price / 1000, 1)) %>%
#'   ungroup()
#' 
#' ggplot_hbar_col(plot_data, average_price, cut, clarity, 
#'   title = "Average diamond price by cut and clarity", 
#'   x_title = "Average price ($US thousands)", 
#'   y_title = "Cut")
#' 
ggplot_hbar_col <-
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
           x_balance = FALSE,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 6,
           x_title = "[X title]",
           x_title_wrap = 50,
           x_trans = "identity",
           x_zero = TRUE,
           x_zero_line = NULL,
           y_expand = NULL,
           y_labels = waiver(),
           y_rev = FALSE,
           y_title = "[Y title]",
           y_title_wrap = 50,
           col_labels = waiver(),
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
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #categorical var
    col_var <- rlang::enquo(col_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
    if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a horizontal bar plot")
    if (is.numeric(col_var_vctr) | is.logical(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal bar plot")
    
    if (position == "stack" & x_trans != "identity") message("simplevis may not perform correctly using an x scale other than identity where position equals stack")
    if (position == "stack" & x_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and x_zero equal to FALSE")
    
    if (y_rev == FALSE) {
      if (is.factor(y_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      }
      else if (is.character(y_var_vctr)) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(factor(.x))))
      }
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
    
    if (col_rev == FALSE){
      if (is.factor(col_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
      }
      else if (is.character(col_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(factor(.x))))
      }
      col_var_vctr <- dplyr::pull(data, !!col_var)
    }
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == FALSE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      theme_hbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by(dplyr::across(!!y_var)) %>%
        dplyr::summarise(dplyr::across(!!x_var, ~sum(.x, na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      x_var_vctr <- dplyr::pull(data_sum, !!x_var)
    }
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- waiver()
    
    if (mobile == FALSE){
      if(is.null(y_labels)) y_labels <- waiver()
      
      plot <- plot +
        scale_y_discrete(expand = y_expand, labels = y_labels)
    }
    else if (mobile == TRUE){
      if(is.character(y_labels)) {
        plot <- plot +
          scale_y_discrete(expand = y_expand, labels = stringr::str_wrap(y_labels, 20))
      }
      else {
        plot <- plot +
          scale_y_discrete(expand = y_expand, labels = function(x) stringr::str_wrap(x, 20))
      }
    }
    
    if (all(x_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_x_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
    }
    else ({
      x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = mobile)
      x_limits <- c(min(x_breaks), max(x_breaks))
      
      plot <- plot +
        scale_x_continuous(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          labels = x_labels,
          trans = x_trans,
          oob = scales::rescale_none
        )
    })
    
    plot <- plot +
      geom_col(aes(
        x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
        alpha = alpha, size = size_line, width = width, 
        position = position2)
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
    }
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      ) +
      scale_colour_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
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
          ncol = col_legend_ncol, nrow = col_legend_nrow, 
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
    else if (mobile == TRUE){
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 20),
          caption = stringr::str_wrap(caption, 50)
        ) +
        guides(
          fill = guide_legend(ncol = 1, reverse = TRUE, title = stringr::str_wrap(col_title, 15)),
          col = guide_legend(ncol = 1, reverse = TRUE, title = stringr::str_wrap(col_title, 15))
        ) +
        theme_mobile_graph()
    }
    
    return(plot)
  }

#' @title Horizontal bar ggplot that is facetted.
#' @description Horizontal bar ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted categorical variable to be on the y axis. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1.
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale. Only applicable where facet_scales equals "fixed" or "free_y".
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the x scale labels through a function that takes the breaks as input and returns labels as output.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the y scale labels through a function that takes the breaks as input and returns labels as output.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
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
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   group_by(cut, clarity) %>%
#'   summarise(average_price = mean(price)) %>%
#'   mutate(average_price = round(average_price / 1000, 1)) 
#'
#' ggplot_hbar_facet(plot_data, average_price, cut, clarity,
#'    title = "Average diamond price by cut and clarity", 
#'    x_title = "Average price ($US thousands)", 
#'    y_title = "Cut")
#'
ggplot_hbar_facet <-
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
           x_balance = FALSE,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 5,
           x_title = "[X title]",
           x_title_wrap = 50,
           x_trans = "identity",
           x_zero = TRUE,
           x_zero_line = NULL,
           y_rev = FALSE,
           y_labels = waiver(),
           y_expand = NULL,
           y_title = "[Y title]",
           y_title_wrap = 50,
           facet_scales = "fixed",
           facet_ncol = NULL,
           facet_nrow = NULL,
           caption = NULL,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           caption_wrap = 80) { 
    
    data <- dplyr::ungroup(data)
    y_var <- rlang::enquo(y_var) #categorical var
    x_var <- rlang::enquo(x_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
    x_var_vctr <- dplyr::pull(data, !!x_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
    if (is.numeric(y_var_vctr)  | is.logical(y_var_vctr)) stop("Please use a categorical y variable for a horizontal bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal bar plot")
    
    if (y_rev == FALSE) {
      if (is.factor(y_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      }
      else if (is.character(y_var_vctr)) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(factor(.x))))
      }
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (is.null(pal)) pal <- sv_pal(1)
    else pal <- pal[1]
    
    plot <- ggplot(data) +
      theme_hbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(x = !!x_var, y = !!y_var, text = !!text_var), col = pal, fill = pal, alpha = alpha, size = size_line, width = width)
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    if(facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- waiver()
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if (all(x_var_vctr == 0, na.rm = TRUE)) {
        plot <- plot +
          scale_x_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
      }
      else ({
        x_breaks <- x_numeric_breaks(x_var_vctr, x_balance = x_balance, x_pretty_n = x_pretty_n, x_trans = x_trans, x_zero = x_zero, mobile = FALSE)
        x_limits <- c(min(x_breaks), max(x_breaks))
        
        plot <- plot +
          scale_x_continuous(
            expand = x_expand,
            breaks = x_breaks,
            limits = x_limits,
            labels = x_labels,
            trans = x_trans,
            oob = scales::rescale_none
          )
      })
    }
    
    if (facet_scales %in% c("free", "free_x")) {
      plot <- plot +
        scale_x_continuous(expand = x_expand,
                           labels = x_labels,
                           trans = x_trans,
                           oob = scales::rescale_none)
    }
    
    if(is.null(y_labels)) y_labels <- waiver()
    
    plot <- plot +
      scale_y_discrete(expand = y_expand, labels = y_labels)
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
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

#' @title Horizontal bar ggplot that is coloured and facetted.
#' @description Horizontal bar ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted categorical variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev TRUE or FALSE of whether to reverse the pal.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale. Only applicable where facet_scales equals "fixed" or "free_y".
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the x scale labels through a function that takes the breaks as input and returns labels as output.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the y scale labels through a function that takes the breaks as input and returns labels as output.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_labels Adjust the  x scale labels through a vector.
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_na TRUE or FALSE of whether to show NA values of the colour variable. Defaults to TRUE.
#' @param col_rev TRUE or FALSE of whether bar fill order from left to right is reversed from default. Defaults to FALSE.
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
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   group_by(cut, clarity, color) %>%
#'   summarise(average_price = mean(price)) %>%
#'   mutate(average_price = round(average_price / 1000, 1))
#'
#' ggplot_hbar_col_facet(plot_data, average_price, color, clarity, cut,
#'   title = "Average diamond price by colour, clarity and cut", 
#'   x_title = "Average price ($US thousands)", 
#'   y_title = "Colour")
#'
ggplot_hbar_col_facet <-
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
           x_balance = FALSE,
           x_expand = NULL,
           x_labels = waiver(),
           x_pretty_n = 5,
           x_title = "[X title]",
           x_title_wrap = 50,
           x_trans = "identity",
           x_zero = TRUE,
           x_zero_line = NULL,
           y_expand = NULL,
           y_labels = waiver(),
           y_rev = FALSE,
           y_title = "[Y title]",
           y_title_wrap = 50,
           col_labels = waiver(),
           col_legend_ncol = NULL,
           col_legend_nrow = NULL,
           col_na = TRUE,
           col_rev = FALSE,
           col_title = "",
           facet_ncol = NULL,
           facet_nrow = NULL,
           facet_scales = "fixed",
           caption_wrap = 80,
           col_title_wrap = 25,
           caption = NULL,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL) {
    
    data <- dplyr::ungroup(data)
    y_var <- rlang::enquo(y_var) #categorical var
    x_var <- rlang::enquo(x_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
    x_var_vctr <- dplyr::pull(data, !!x_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
    if (is.numeric(y_var_vctr) | is.logical(y_var_vctr)) stop("Please use a categorical y variable for a horizontal bar plot")
    if (is.numeric(col_var_vctr) | is.logical(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal bar plot")
    
    if (position == "stack" & x_trans != "identity") message("simplevis may not perform correctly using an x scale other than identity where position equals stack")
    if (position == "stack" & x_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and x_zero equal to FALSE")
    
    if (y_rev == FALSE) {
      if (is.factor(y_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      }
      else if (is.character(y_var_vctr)) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(factor(.x))))
      }
      y_var_vctr <- dplyr::pull(data, !!y_var)
    }
    
    if (col_rev == FALSE){
      if (is.factor(col_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
      }
      else if (is.character(col_var_vctr)){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(factor(.x))))
      }
      col_var_vctr <- dplyr::pull(data, !!col_var)
    }
    
    if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = FALSE)
    if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = FALSE)
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- sv_pal(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == FALSE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      theme_hbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), alpha = alpha, size = size_line, width = width, position = position2)
    
    x_zero_list <- sv_x_zero_adjust(x_var_vctr, x_balance = x_balance, x_zero = x_zero, x_zero_line = x_zero_line)
    if(facet_scales %in% c("fixed", "free_y")) x_zero <- x_zero_list[[1]]
    x_zero_line <- x_zero_list[[2]]
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- waiver()
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by(dplyr::across(c(!!y_var, !!facet_var))) %>%
        dplyr::summarise(dplyr::across(!!x_var, ~sum(.x, na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      x_var_vctr <- dplyr::pull(data_sum, !!x_var)
    }
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if (all(x_var_vctr == 0, na.rm = TRUE)) {
        plot <- plot +
          scale_x_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
      }
      else ({
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
      })
    }
    if (facet_scales %in% c("free", "free_x")) {
      plot <- plot +
        scale_x_continuous(expand = y_expand,
                           trans = x_trans,
                           labels = x_labels,
                           oob = scales::rescale_none)
    }
    
    if(is.null(y_labels)) y_labels <- waiver()
    
    plot <- plot +
      scale_y_discrete(expand = y_expand, labels = y_labels)
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      ) +
      scale_colour_manual(
        values = pal,
        drop = FALSE,
        labels = col_labels,
        na.translate = col_na,
        na.value = "#A8A8A8"
      ) 
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_vline(xintercept = 0, colour = "#323232", size = 0.3)
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
        ncol = col_legend_ncol, nrow = col_legend_nrow, 
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
    
    return(plot)
  }
