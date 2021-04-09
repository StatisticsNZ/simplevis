# ggplot vbar functions

#' @title Theme for vertical bar ggplots.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#' 
#' ggplot() +
#'   theme_vbar("Courier", 9, 7) +
#'   ggtitle("This is a title of a selected font family and size")
theme_vbar <-
  function(font_family = "Helvetica",
           font_size_title = 11,
           font_size_body = 10) {
    list(
      theme(
        plot.title = element_text(
          family = font_family,
          colour = "#000000",
          size = font_size_title,
          face = "bold",
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          family = font_family,
          colour = "#000000",
          size = font_size_body,
          face = "plain",
          hjust = 0.5
        ),
        plot.caption = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          face = "plain",
          hjust = 0.99
        ),
        plot.margin = margin(
          t = 5,
          l = 5,
          b = 5,
          r = 20
        ),
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#D3D3D3", size = 0.2),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        strip.background = element_rect(colour = "white", fill = "white"),
        text = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body
        ),
        strip.text = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          hjust = 0.475
        ),
        axis.title.x = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          margin = margin(t = 10)
        ),
        axis.title.y = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          margin = margin(r = 10)
        ),
        axis.text.x = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body
        ),
        axis.text.y = element_text(
          family = font_family,
          colour = "#323232",
          hjust = 1,
          size = font_size_body
        ),
        axis.line = element_line(colour = "#323232", size = 0.3),
        axis.ticks = element_line(colour = "#323232", size = 0.3),
        legend.text = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          margin = margin(r = 10),
          hjust = 0
        ),
        legend.title = element_text(
          family = font_family,
          colour = "#323232",
          size = font_size_body,
          margin = margin(r = 20)
        ),
        legend.margin = margin(t = 20, b = 20),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm")
      )
    )
  }

#' @title Vertical bar ggplot.
#' @description Vertical bar ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param line_size The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_na_inf TRUE or FALSE of whether to make NA y_var values infinity with a light grey colour to emphasise them. Defaults to FALSE.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. Defaults to NULL, which is TRUE if there are positive and negative values in y_var. Otherwise it is FALSE.  
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
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
                        line_size = 0.5,
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
                        y_na_inf = FALSE,
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
                        isMobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var) #numeric var
  text_var <- rlang::enquo(text_var)
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
  
  min_y_var_vctr <- min(y_var_vctr, na.rm = TRUE)
  max_y_var_vctr <- max(y_var_vctr, na.rm = TRUE)
  
  y_above_and_below_zero <- ifelse(min_y_var_vctr < 0 & max_y_var_vctr > 0, TRUE, FALSE)
  
  if(y_above_and_below_zero == TRUE) y_zero <- FALSE
  
  if(is.null(y_zero_line)) {
    if(y_above_and_below_zero == TRUE | y_balance == TRUE) y_zero_line <- TRUE
    else(y_zero_line <- FALSE)
  }
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }

  if (is.null(pal)) pal <- pal_default(1)
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
    geom_col(aes(x = !!x_var, y = !!y_var, text = !!text_var), col = pal, fill = pal, alpha = alpha, size = line_size, width = bar_width)
  
  if (lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr)) {
    
    x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
    x_limits <- c(min(x_var_vctr), max(x_var_vctr))
    if(is.null(x_expand)) x_expand <- c(0.5 / (length(x_var_vctr) - 1) * width, 0)

    if(isMobile == TRUE) {
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
  
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    if (y_balance == TRUE) {
      y_var_vctr <- abs(y_var_vctr)
      y_var_vctr <- c(-y_var_vctr, y_var_vctr)
    }
    if (y_zero == TRUE) {
      y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n)
      if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
      y_limits <- c(min(y_breaks), max(y_breaks))
    }
    else if (y_zero == FALSE) {
      if(y_trans != "log10") y_breaks <- pretty(y_var_vctr, n = y_pretty_n)
      if(y_trans == "log10") {
        y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n) 
        y_breaks <- c(1, y_breaks[y_breaks > 1])
      }
      y_limits <- c(min(y_breaks), max(y_breaks))
    }
    
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
  
  if(y_na_inf == TRUE) {
    na_data <- dplyr::filter(data, is.na(!!y_var))
    
    if(nrow(na_data) != 0) {
      if(y_limits[1] >= 0 & y_limits[2] > 0){
        plot <- plot +
          geom_col(aes(x = !!y_var, y = y_limits[2], text = !!text_var),
                   col = "#F5F5F5", fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                   data = na_data)
      }
      else if(y_limits[1] < 0 & y_limits[2] <= 0) {
        plot <- plot +
          geom_col(aes(x = !!y_var, y = y_limits[1], text = !!text_var),
                   col = "#F5F5F5", fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                   data = na_data)        
      }
      else if(y_limits[1] < 0 & y_limits[2] > 0) {
        ggplotly_adjust <- (y_limits[2] - y_limits[1]) / 1000000 # hack to fix ggplotly bug #1929
        
        plot <- plot +
          geom_col(aes(x = !!y_var, y = y_limits[2], text = !!text_var),
                   col = "#F5F5F5", fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                   data = na_data) +
          geom_col(aes(x = !!y_var, y = y_limits[1] + ggplotly_adjust, text = !!text_var),
                   col = "#F5F5F5", fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                   data = na_data)
      }
    }
  }

  if(y_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (isMobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      )
  }
  else if (isMobile == TRUE) {
    plot <- plot +
      theme(plot.title.position = "plot") +
      theme(plot.caption.position = "plot") +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(x_title, 20),
        y = stringr::str_wrap(y_title, 30),
        caption = stringr::str_wrap(caption, 50)
      ) +
      theme(axis.text.x = element_text(hjust = 0.75))
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
#' @param line_size The size of the outlines of bars.
#' @param title Title string. Defaults to [Title].
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_title X axis title string. Defaults to [X title].
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_na_inf TRUE or FALSE of whether to make NA y_var values infinity with a light grey colour to emphasise them. Defaults to FALSE.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. Defaults to NULL, which is TRUE if there are positive and negative values in y_var. Otherwise it is FALSE.  
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_labels_ncol The number of columns in the legend. Defaults to 1.
#' @param col_labels_nrow The number of rows in the legend.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where isMobile equals TRUE.
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
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
           line_size = 0.5,
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
           y_na_inf = FALSE,
           y_pretty_n = 5,
           y_title = "[Y title]",
           y_title_wrap = 50,
           y_trans = "identity",
           y_zero = TRUE,
           y_zero_line = NULL,
           col_labels = NULL,
           col_labels_ncol = NULL,
           col_labels_nrow = NULL,
           col_title = "",
           col_title_wrap = 25,
           caption = NULL,
           caption_wrap = 80,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           isMobile = FALSE) {
    
    data <- dplyr::ungroup(data)
    y_var <- rlang::enquo(y_var) #numeric var
    x_var <- rlang::enquo(x_var) #categorical var
    col_var <- rlang::enquo(col_var) #categorical var
    text_var <- rlang::enquo(text_var)
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
    x_var_vctr <- dplyr::pull(data, !!x_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a vertical bar plot")
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (position == "stack" & y_trans != "identity") message("simplevis may not perform correctly using a y scale other than identity where position equals stack")
    if (position == "stack" & y_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and y_zero equal to FALSE")
    
    min_y_var_vctr <- min(y_var_vctr, na.rm = TRUE)
    max_y_var_vctr <- max(y_var_vctr, na.rm = TRUE)
    
    y_above_and_below_zero <- ifelse(min_y_var_vctr < 0 & max_y_var_vctr > 0, TRUE, FALSE)
    
    if(y_above_and_below_zero == TRUE) y_zero <- FALSE
    
    if(is.null(y_zero_line)) {
      if(y_above_and_below_zero == TRUE | y_balance == TRUE) y_zero_line <- TRUE
      else(y_zero_line <- FALSE)
    }

    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
    else bar_unit <- 1
    
    bar_width <- bar_unit * width
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      n_col <- length(levels(col_var_vctr))
    }
    else n_col <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- pal_default(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    if (!is.null(col_labels)) labels <- rev(col_labels)
    if (is.null(col_labels)) labels <- waiver()
    
    plot <- ggplot(data) +
      theme_vbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
               alpha = alpha, size = line_size, width = bar_width, position = position2)
    
    if (lubridate::is.Date(x_var_vctr) | is.numeric(x_var_vctr)) {
      
      x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
      if(is.null(x_expand)) x_expand <- waiver()

      if(isMobile == TRUE) {
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
    
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (all(y_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = y_expand, breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
    }
    else ({
      if (y_balance == TRUE) {
        y_var_vctr <- abs(y_var_vctr)
        y_var_vctr <- c(-y_var_vctr, y_var_vctr)
      }
      if (y_zero == TRUE) {
        y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n)
        if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      else if (y_zero == FALSE) {
        if(y_trans != "log10") y_breaks <- pretty(y_var_vctr, n = y_pretty_n)
        if(y_trans == "log10") {
          y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n) 
          y_breaks <- c(1, y_breaks[y_breaks > 1])
        }
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      
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
    
    if(y_na_inf == FALSE) {
      plot <- plot +
        geom_col(aes(
          x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
          alpha = alpha, size = line_size, width = width, 
          position = position2)
    }
    else if(y_na_inf == TRUE) {
      data <- data %>% 
        dplyr::mutate(col_var2 = ifelse(is.na(!!y_var), NA, as.character(!!col_var))) %>%
        dplyr::mutate(col_var2 = forcats::fct_rev(forcats::fct_explicit_na(.data$col_var2, "Not available"))) 
      
      if(is.character(x_var_vctr)) {
        all_na <- data %>% 
          group_by(!!x_var) %>%
          summarise(all_na = all(is.na(!!y_var))) %>% 
          filter(all_na == TRUE) %>% 
          mutate(dplyr::across(!!x_var, ~as.character(.x))) %>% 
          pull(!!x_var)
        
        data <- data %>% 
          dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_reorder(.x, !!y_var, .fun = stats::median, na.rm = TRUE)))  %>% 
          dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_relevel(.x, all_na)))  
      }
      
      if(y_limits[1] >= 0 & y_limits[2] > 0) {
        data <- data %>%
          dplyr::mutate(y_var2 = ifelse(is.na(!!y_var), y_limits[2], !!y_var))
        
        plot <- plot +
          geom_col(aes(x = !!x_var, y = .data$y_var2, col = .data$col_var2, fill = .data$col_var2, group = !!col_var, text = !!text_var), 
                   alpha = alpha, size = line_size, width = width, position = position2, data = data)
      }
      else if(y_limits[1] < 0 & y_limits[2] <= 0) {
        data <- data %>%
          dplyr::mutate(y_var2 = ifelse(is.na(!!y_var), y_limits[1], !!y_var))
        
        plot <- plot +
          geom_col(aes(x = !!x_var, y = .data$y_var2, col = .data$col_var2, fill = .data$col_var2, group = !!col_var, text = !!text_var), 
                   alpha = alpha, size = line_size, width = width, position = position2, data = data)
      }
      else if(y_limits[1] < 0 & y_limits[2] > 0) {
        data <- data %>%
          dplyr::mutate(col_var3 = .data$col_var2) %>% 
          dplyr::mutate(y_var2 = ifelse(is.na(!!y_var), y_limits[1], !!y_var)) %>%
          dplyr::mutate(y_var3 = ifelse(is.na(!!y_var), y_limits[2], !!y_var))
        
        plot <- plot +
          geom_col(aes(x = !!x_var, y = .data$y_var2, col = .data$col_var2, fill = .data$col_var2, group = !!col_var, text = !!text_var), 
                   alpha = alpha, size = line_size, width = width, position = position2, data = data) +
          geom_col(aes(x = !!x_var, y = .data$y_var3, col = .data$col_var2, fill = .data$col_var2, group = !!col_var, text = !!text_var), 
                   alpha = alpha, size = line_size, width = width, position = position2, data = data)
      }
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
        na.value = "#A8A8A8"
      ) +
      scale_colour_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.value = "#A8A8A8"
      ) 

    if (isMobile == FALSE) {
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
    else if (isMobile == TRUE) {
      plot <- plot +
        theme(plot.title.position = "plot") +
        theme(plot.caption.position = "plot") +
        theme(legend.position = "bottom") +
        theme(legend.justification = "left") +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 30),
          caption = stringr::str_wrap(caption, 50)
        ) +
        guides(fill = guide_legend(
          ncol = 1,
          byrow = TRUE,
          title = stringr::str_wrap(col_title, 15)
        ), 
        col = guide_legend(
          ncol = 1,
          byrow = TRUE,
          title = stringr::str_wrap(col_title, 15)
        )) +
        theme(axis.text.x = element_text(hjust = 0.75))
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
#' @param line_size The size of the outlines of bars. 
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
#' @param y_na_inf TRUE or FALSE of whether to make NA y_var values infinity with a light grey colour to emphasise them. Defaults to FALSE. Only functional where facet_scales = "fixed" or "free_x". 
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. Defaults to NULL, which is TRUE if there are positive and negative values in y_var. Otherwise it is FALSE.  
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
           line_size = 0.5,
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
           y_na_inf = FALSE, 
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
    
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a vertical bar plot")
    
    min_y_var_vctr <- min(y_var_vctr, na.rm = TRUE)
    max_y_var_vctr <- max(y_var_vctr, na.rm = TRUE)
    
    y_above_and_below_zero <- ifelse(min_y_var_vctr < 0 & max_y_var_vctr > 0, TRUE, FALSE)
    
    if(y_above_and_below_zero == TRUE) y_zero <- FALSE
    
    if(is.null(y_zero_line)) {
      if(y_above_and_below_zero == TRUE | y_balance == TRUE) y_zero_line <- TRUE
      else(y_zero_line <- FALSE)
    }
    
    if(is.null(font_size_title)) font_size_title <- 11
    if(is.null(font_size_body)) font_size_body <- 10
    
    if (is.null(pal)) pal <- pal_default(1)
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
      geom_col(aes(x = !!x_var, y = !!y_var, text = !!text_var), col = pal, fill = pal, alpha = alpha, size = line_size, width = bar_width)
    
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
    
    if(is.null(y_expand)) y_expand <- c(0, 0)

    if (facet_scales %in% c("fixed", "free_x")) {
      
      if (y_balance == TRUE) {
        y_var_vctr <- abs(y_var_vctr)
        y_var_vctr <- c(-y_var_vctr, y_var_vctr)
      }
      if (y_zero == TRUE) {
        y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n)
        if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      else if (y_zero == FALSE) {
        if(y_trans != "log10") y_breaks <- pretty(y_var_vctr, n = y_pretty_n)
        if(y_trans == "log10") {
          y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n) 
          y_breaks <- c(1, y_breaks[y_breaks > 1])
        }
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = y_expand,
          breaks = y_breaks,
          limits = y_limits,
          trans = y_trans,
          labels = y_labels,
          oob = scales::squish
        )
      
      if(y_na_inf == TRUE) {
        na_data <- dplyr::filter(data, is.na(!!y_var))
        
        if(nrow(na_data) != 0) {
          if(y_limits[1] >= 0 & y_limits[2] > 0){
            plot <- plot +
              geom_col(aes(x = !!y_var, y = y_limits[2], text = !!text_var),
                       col = "#F5F5F5", fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                       data = na_data)
          }
          else if(y_limits[1] < 0 & y_limits[2] <= 0) {
            plot <- plot +
              geom_col(aes(x = !!y_var, y = y_limits[1], text = !!text_var),
                       col = "#F5F5F5", fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                       data = na_data)        
          }
          else if(y_limits[1] < 0 & y_limits[2] > 0) {
            ggplotly_adjust <- (y_limits[2] - y_limits[1]) / 1000000 # hack to fix ggplotly bug #1929
            
            plot <- plot +
              geom_col(aes(x = !!y_var, y = y_limits[2], text = !!text_var),
                       col = "#F5F5F5", fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                       data = na_data) +
              geom_col(aes(x = !!y_var, y = y_limits[1] + ggplotly_adjust, text = !!text_var),
                       fill = "#F5F5F5", alpha = alpha, size = line_size, width = width, 
                       data = na_data)
          }
        }
      }
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
#' @param line_size The size of the outlines of bars.
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
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. Defaults to NULL, which is TRUE if there are positive and negative values in y_var. Otherwise it is FALSE.  
#' @param col_labels Adjust the  colour scale labels through a vector.
#' @param col_labels_ncol The number of columns in the legend. Defaults to 1.
#' @param col_labels_nrow The number of rows in the legend.
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
           line_size = 0.5,
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
    
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a vertical bar plot")
    
    if(is.null(font_size_title)) font_size_title <- 11
    if(is.null(font_size_body)) font_size_body <- 10
    
    if (position == "stack" & y_trans != "identity") message("simplevis may not perform correctly using a y scale other than identity where position equals stack")
    if (position == "stack" & y_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and y_zero equal to FALSE")
    
    min_y_var_vctr <- min(y_var_vctr, na.rm = TRUE)
    max_y_var_vctr <- max(y_var_vctr, na.rm = TRUE)
    
    y_above_and_below_zero <- ifelse(min_y_var_vctr < 0 & max_y_var_vctr > 0, TRUE, FALSE)
    
    if(y_above_and_below_zero == TRUE) y_zero <- FALSE
    
    if(is.null(y_zero_line)) {
      if(y_above_and_below_zero == TRUE | y_balance == TRUE) y_zero_line <- TRUE
      else(y_zero_line <- FALSE)
    }

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
    
    if (is.null(pal)) pal <- pal_default(n_col)
    else pal <- pal[1:n_col]
    
    if (pal_rev == TRUE) pal <- rev(pal)
    
    plot <- ggplot(data) +
      coord_cartesian() +
      theme_vbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
               alpha = alpha, size = line_size, width = bar_width, position = position2)
    
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
    
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_x")) {
      if (y_balance == TRUE) {
        y_var_vctr <- abs(y_var_vctr)
        y_var_vctr <- c(-y_var_vctr, y_var_vctr)
      }
      if (y_zero == TRUE) {
        y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n)
        if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      else if (y_zero == FALSE) {
        if(y_trans != "log10") y_breaks <- pretty(y_var_vctr, n = y_pretty_n)
        if(y_trans == "log10") {
          y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n) 
          y_breaks <- c(1, y_breaks[y_breaks > 1])
        }
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = y_expand,
          breaks = y_breaks,
          limits = y_limits,
          trans = y_trans,
          labels = y_labels,
          oob = scales::squish
        )
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
        na.value = "#A8A8A8"
      ) +
      scale_colour_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
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
