# ggplot line functions

#' @title Theme for line ggplots.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   theme_line("Courier", 9, 7) +
#'   ggplot2::ggtitle("This is a title of a selected font family and size")
theme_line <-
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
          size = font_size_body
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
        legend.position = "bottom",
        legend.margin = margin(t = 20, b = 20),
        legend.key = element_rect(fill = "white"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm")
      )
    )
  }

#' @title Line ggplot.
#' @description Line ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param points TRUE or FALSE of whether to include points. Defaults to TRUE.
#' @param point_size Size of points. Defaults to 1. Only applicable to where points equals TRUE.
#' @param lines TRUE or FALSE of whether to include lines. Defaults to TRUE.
#' @param size Size of lines. Defaults to 0.5. Only applicable to where lines equals TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param wrap_title Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param wrap_subtitle Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param wrap_x_title Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_y_title Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_caption Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' library(dplyr)
#' 
#' plot_data <- storms %>%
#'   group_by(year) %>%
#'   summarise(wind = round(mean(wind), 2)) 
#'
#'   plot <- ggplot_line(data = plot_data, x_var = year, y_var = wind,
#'       title = "Average wind speed of Atlantic storms, 1975-2015",
#'       x_title = "Year",
#'       y_title = "Average maximum sustained wind speed (knots)")
#'
#'   plot
#'
#'   plotly::ggplotly(plot)
ggplot_line <- function(data,
                        x_var,
                        y_var,
                        tip_var = NULL,
                        x_labels = waiver(),
                        x_pretty_n = 6,
                        y_zero = TRUE,
                        y_zero_line = TRUE,
                        y_trans = "identity",
                        y_labels = waiver(),
                        y_pretty_n = 5,
                        points = TRUE,
                        point_size = 1,
                        lines = TRUE,
                        size = 0.5,
                        pal = NULL,
                        title = "[Title]",
                        subtitle = NULL,
                        x_title = "[X title]",
                        y_title = "[Y title]",
                        caption = NULL,
                        font_family = "Helvetica",
                        font_size_title = NULL,
                        font_size_body = NULL,
                        wrap_title = 70,
                        wrap_subtitle = 80,
                        wrap_x_title = 50,
                        wrap_y_title = 50,
                        wrap_caption = 80,
                        isMobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) #numeric var
  tip_var <- rlang::enquo(tip_var)
  
  x_var_vector <- dplyr::pull(data, !!x_var)
  y_var_vector <- dplyr::pull(data, !!y_var)
  
  if (!(lubridate::is.Date(x_var_vector) | is.numeric(x_var_vector))) stop("Please use a numeric or date x variable for a line plot")
  if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a line plot")
  
  min_y_var_vector <- min(y_var_vector, na.rm = TRUE)
  max_y_var_vector <- max(y_var_vector, na.rm = TRUE)
  if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero == TRUE) {
    y_zero <- FALSE
  }
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }
  
  if (is.null(pal)) pal <- pal_snz
  
  if (points == TRUE) alpha <- 1
  else if (points == FALSE) alpha <- 0
  
  plot <- ggplot(data) +
    coord_cartesian(clip = "off") +
    theme_line(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (lines == TRUE) plot <- plot +
    geom_line(aes(!!x_var, !!y_var, group = 1), col = pal[1], size = size)

  plot <- plot +
    geom_point(aes(!!x_var, !!y_var, text = !!tip_var), col = pal[1], size = point_size, alpha = alpha)

  if(isMobile == FALSE) x_n <- x_pretty_n
  else if(isMobile == TRUE) x_n <- 4

  x_breaks <- pretty(x_var_vector, n = x_n)
  x_limits <- c(min(x_breaks), max(x_breaks))
  
  if (lubridate::is.Date(x_var_vector)) {
    plot <- plot +
      scale_x_date(
        breaks = x_breaks,
        limits = x_limits,
        labels = x_labels
      )
  }
  else if (is.numeric(x_var_vector)) {
    plot <- plot +
      scale_x_continuous(breaks = x_breaks,
                         limits = x_limits,
                         labels = x_labels,
                         oob = scales::rescale_none)
  }
  
  if (all(y_var_vector == 0, na.rm = TRUE)) {
    y_limits <- c(0, 1)
    
    plot <- plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), breaks = c(0, 1), labels = y_labels, limits = y_limits)
  }
  else ({
    if (y_zero == TRUE) {
      if(max_y_var_vector > 0) y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n)
      if(min_y_var_vector < 0) y_breaks <- pretty(c(y_var_vector, 0), n = y_pretty_n)
      
      if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
      y_limits <- c(min(y_breaks), max(y_breaks))
    }
    else if (y_zero == FALSE) {
      if(y_trans != "log10") y_breaks <- pretty(y_var_vector, n = y_pretty_n)
      if(y_trans == "log10") {
        y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n) 
        y_breaks <- c(1, y_breaks[y_breaks > 1])
      }
      y_limits <- c(min(y_breaks), max(y_breaks))
    }
    
    plot <- plot +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = y_breaks,
        limits = y_limits,
        trans = y_trans,
        labels = y_labels,
        oob = scales::rescale_none
      )
  })

  if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero_line == TRUE) {
    plot <- plot +
      ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }

  if (isMobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, wrap_title),
        subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
        x = stringr::str_wrap(x_title, wrap_x_title),
        y = stringr::str_wrap(y_title, wrap_y_title),
        caption = stringr::str_wrap(caption, wrap_caption)
      )
  }
  else if (isMobile == TRUE) {
    plot <- plot +
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

#' @title Line ggplot that is coloured.
#' @description Line ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable for lines and points to be coloured by. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param col_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to FALSE.
#' @param points TRUE or FALSE of whether to include points. Defaults to TRUE.
#' @param point_size Size of points. Defaults to 1. Only applicable to where points equals TRUE.
#' @param lines TRUE or FALSE of whether to include lines. Defaults to TRUE.
#' @param size Size of lines. Defaults to 0.5. Only applicable to where lines equals TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param rev_pal Reverses the palette. Defaults to FALSE.
#' @param legend_ncol The number of columns in the legend.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param caption Caption title string. Defaults to NULL.
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param wrap_title Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param wrap_subtitle Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param wrap_x_title Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_y_title Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. Not applicable where isMobile equals TRUE.
#' @param wrap_caption Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
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
#' plot <- ggplot_line_col(data = plot_data, x_var = year, y_var = wind, col_var = status)
#'
#' plot
#'
#' plotly::ggplotly(plot)
ggplot_line_col <-
  function(data,
           x_var,
           y_var,
           col_var,
           tip_var = NULL,
           x_labels = waiver(),
           x_pretty_n = 6,
           y_zero = TRUE,
           y_zero_line = TRUE,
           y_trans = "identity",
           y_labels = waiver(),
           y_pretty_n = 5,
           col_drop = FALSE,
           points = TRUE,
           point_size = 1,
           lines = TRUE,
           size = 0.5,
           pal = NULL,
           rev_pal = FALSE,
           legend_ncol = 3,
           title = "[Title]",
           subtitle = NULL,
           x_title = "[X title]",
           y_title = "[Y title]",
           col_title = "",
           caption = NULL,
           legend_labels = NULL,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           wrap_title = 70,
           wrap_subtitle = 80,
           wrap_x_title = 50,
           wrap_y_title = 50,
           wrap_col_title = 25,
           wrap_caption = 80,
           isMobile = FALSE) {
    
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    data <- data %>% 
      dplyr::ungroup() %>%
      arrange(!!x_var) #fix ggplotly legend bug
    
    x_var_vector <- dplyr::pull(data, !!x_var)
    y_var_vector <- dplyr::pull(data, !!y_var)
    col_var_vector <- dplyr::pull(data, !!col_var)
    
    if (!(lubridate::is.Date(x_var_vector) | is.numeric(x_var_vector))) stop("Please use a numeric or date x variable for a line plot")
    if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a line plot")
    if (is.numeric(col_var_vector)) stop("Please use a categorical colour variable for a line plot")
    
    min_y_var_vector <- min(y_var_vector, na.rm = TRUE)
    max_y_var_vector <- max(y_var_vector, na.rm = TRUE)
    if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero == TRUE) {
      y_zero <- FALSE
    }
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (is.null(pal)) pal <- pal_snz
    
    if (points == TRUE) alpha <- 1
    else if (points == FALSE) alpha <- 0
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_line(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    if (lines == TRUE) plot <- plot +
      geom_line(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var), size = size)

    plot <- plot +
      geom_point(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var, text = !!tip_var),
        size = point_size, alpha = alpha)

    if (rev_pal == TRUE) pal <- rev(pal)
    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
    
    if(isMobile == FALSE) x_n <- x_pretty_n
    else if(isMobile == TRUE) x_n <- 4
    
    x_breaks <- pretty(x_var_vector, n = x_n)
    x_limits <- c(min(x_breaks), max(x_breaks))
    
    if (lubridate::is.Date(x_var_vector)) {
      plot <- plot +
        scale_x_date(
          breaks = x_breaks,
          limits = x_limits,
          labels = x_labels
        )
    }
    else if (is.numeric(x_var_vector)) {
      plot <- plot +
        scale_x_continuous(breaks = x_breaks,
                           limits = x_limits,
                           labels = x_labels)
    }
    
    if (all(y_var_vector == 0, na.rm = TRUE)) {
      y_limits <- c(0, 1)
      
      plot <- plot +
        ggplot2::scale_y_continuous(breaks = c(0, 1), labels = y_labels, limits = y_limits)
    }
    else ({
      if (y_zero == TRUE) {
        if(max_y_var_vector > 0) y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n)
        if(min_y_var_vector < 0) y_breaks <- pretty(c(y_var_vector, 0), n = y_pretty_n)
        
        if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      else if (y_zero == FALSE) {
        if(y_trans != "log10") y_breaks <- pretty(y_var_vector, n = y_pretty_n)
        if(y_trans == "log10") {
          y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n) 
          y_breaks <- c(1, y_breaks[y_breaks > 1])
        }
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
  
      plot <- plot +
        scale_y_continuous(
          expand = c(0, 0),
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
        drop = col_drop,
        labels = labels,
        na.value = "#A8A8A8"
      ) 

    if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero_line == TRUE) {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }

    if (isMobile == FALSE) {
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, wrap_title),
          subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
          x = stringr::str_wrap(x_title, wrap_x_title),
          y = stringr::str_wrap(y_title, wrap_y_title),
          caption = stringr::str_wrap(caption, wrap_caption)
        ) +
        guides(col = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title)))
    }
    else if (isMobile == TRUE) {
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 30),
          caption = stringr::str_wrap(caption, 50)
        )  +
        guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15)))
    }
    
    return(plot)
  }

#' @title Line ggplot that is facetted.
#' @description Line ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. Not applicable where isMobile equals TRUE.
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param points TRUE or FALSE of whether to include points. Defaults to TRUE.
#' @param point_size Size of points. Defaults to 1. Only applicable to where points equals TRUE.
#' @param lines TRUE or FALSE of whether to include lines. Defaults to TRUE.
#' @param size Size of lines. Defaults to 0.5. Only applicable to where lines equals TRUE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_nrow The number of rows of facetted plots. Defaults to NULL, which generally chooses 2 rows. Not applicable to where isMobile is TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param wrap_title Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param wrap_subtitle Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param wrap_x_title Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_y_title Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_caption Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
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
#'  plot <- ggplot_line_facet(data = plot_data, x_var = year, y_var = wind, facet_var = status)
#'
#'  plot
#'
#'  plotly::ggplotly(plot)
ggplot_line_facet <-
  function(data,
           x_var,
           y_var,
           facet_var,
           tip_var = NULL,
           x_labels = waiver(),
           x_pretty_n = 5,
           y_zero = TRUE,
           y_zero_line = TRUE,
           y_trans = "identity",
           y_labels = waiver(),
           y_pretty_n = 5,
           facet_scales = "fixed",
           facet_nrow = NULL,
           points = TRUE,
           point_size = 1,
           lines = TRUE,
           size = 0.5,
           pal = NULL,
           title = "[Title]",
           subtitle = NULL,
           x_title = "[X title]",
           y_title = "[Y title]",
           caption = NULL,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           wrap_title = 70,
           wrap_subtitle = 80,
           wrap_x_title = 50,
           wrap_y_title = 50,
           wrap_caption = 80,
           isMobile = FALSE) {
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    x_var_vector <- dplyr::pull(data, !!x_var)
    y_var_vector <- dplyr::pull(data, !!y_var)
    facet_var_vector <- dplyr::pull(data, !!facet_var)
    
    if (!(lubridate::is.Date(x_var_vector) | is.numeric(x_var_vector))) stop("Please use a numeric or date x variable for a line plot")
    if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a line plot")
    if (is.numeric(facet_var_vector)) stop("Please use a categorical facet variable for a line plot")
    
    min_y_var_vector <- min(y_var_vector, na.rm = TRUE)
    max_y_var_vector <- max(y_var_vector, na.rm = TRUE)
    if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero == TRUE) {
      y_zero <- FALSE
    }
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (is.null(pal)) pal <- pal_snz
    
    if (points == TRUE) alpha <- 1
    else if (points == FALSE) alpha <- 0
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_line(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      )
    
    if (lines == TRUE) plot <- plot +
      geom_line(aes(!!x_var, !!y_var, group = 1), col = pal[1], size = size) 

    plot <- plot +
      geom_point(aes(!!x_var, !!y_var, text = !!tip_var), col = pal[1], size = point_size, alpha = alpha)

    if (facet_scales %in% c("fixed", "free_y")) {
      
      if(isMobile == FALSE) x_n <- x_pretty_n
      else if(isMobile == TRUE) x_n <- 4
      
      x_breaks <- pretty(x_var_vector, n = x_n)
      x_limits <- c(min(x_breaks), max(x_breaks))
      
      if (lubridate::is.Date(x_var_vector)) {
        plot <- plot +
          scale_x_date(
            breaks = x_breaks,
            limits = x_limits,
            labels = x_labels
          )
      }
      else if (is.numeric(x_var_vector)) {
        plot <- plot +
          scale_x_continuous(breaks = x_breaks,
                             limits = x_limits,
                             labels = x_labels)
      }
    }
    
    if (facet_scales %in% c("fixed", "free_x")) {
      if (y_zero == TRUE) {
        if(max_y_var_vector > 0) y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n)
        if(min_y_var_vector < 0) y_breaks <- pretty(c(y_var_vector, 0), n = y_pretty_n)
        
        if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      else if (y_zero == FALSE) {
        if(y_trans != "log10") y_breaks <- pretty(y_var_vector, n = y_pretty_n)
        if(y_trans == "log10") {
          y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n) 
          y_breaks <- c(1, y_breaks[y_breaks > 1])
        }
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = c(0, 0),
          breaks = y_breaks,
          limits = y_limits,
          trans = y_trans,
          labels = y_labels,
          oob = scales::rescale_none
        )
    }
    else if (facet_scales %in% c("free", "free_y")) {
      plot <- plot +
        scale_y_continuous(expand = c(0, 0),
                           trans = y_trans,
                           labels = y_labels,
                           oob = scales::rescale_none)
    }
    
    if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero_line == TRUE) {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }

    if (isMobile == FALSE) {
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) <= 3) facet_nrow <- 1
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) > 3) facet_nrow <- 2
      
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, wrap_title),
          subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
          x = stringr::str_wrap(x_title, wrap_x_title),
          y = stringr::str_wrap(y_title, wrap_y_title),
          caption = stringr::str_wrap(caption, wrap_caption)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow)
    }
    else if (isMobile == TRUE) {
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 30),
          caption = stringr::str_wrap(caption, 50)
        )  +
        facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = 1)
      
    }
    
    return(plot)
  }

#' @title Line ggplot that is coloured and facetted.
#' @description Line ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric or date variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable for lines and points to be coloured by. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. Not applicable where isMobile equals TRUE.
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param col_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to FALSE.
#' @param points TRUE or FALSE of whether to include points. Defaults to TRUE.
#' @param point_size Size of points. Defaults to 1. Only applicable to where points equals TRUE.
#' @param lines TRUE or FALSE of whether to include lines. Defaults to TRUE.
#' @param size Size of lines. Defaults to 0.5. Only applicable to where lines equals TRUE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_nrow The number of rows of facetted plots. Defaults to NULL, which generally chooses 2 rows. Not applicable to where isMobile is TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param rev_pal Reverses the palette. Defaults to FALSE.
#' @param legend_ncol The number of columns in the legend.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param caption Caption title string. Defaults to NULL.
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param wrap_title Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param wrap_subtitle Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param wrap_x_title Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_y_title Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. Not applicable where isMobile equals TRUE.
#' @param wrap_caption Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile.
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
#'  plot <- ggplot_line_col_facet(data = plot_data, x_var = year, y_var = wind, col_var = status,
#'                                 facet_var = status)
#'
#'  plot
#'
#'  plotly::ggplotly(plot)
ggplot_line_col_facet <-
  function(data,
           x_var,
           y_var,
           col_var,
           facet_var,
           tip_var = NULL,
           x_labels = waiver(),
           x_pretty_n = 5,
           y_zero = TRUE,
           y_zero_line = TRUE,
           y_trans = "identity",
           y_labels = waiver(),
           y_pretty_n = 5,
           col_drop = FALSE,
           facet_scales = "fixed",
           facet_nrow = NULL,
           points = TRUE,
           point_size = 1,
           lines = TRUE,
           size = 0.5,
           pal = NULL,
           rev_pal = FALSE,
           legend_ncol = 3,
           title = "[Title]",
           subtitle = NULL,
           x_title = "[X title]",
           y_title = "[Y title]",
           col_title = "",
           caption = NULL,
           legend_labels = NULL,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           wrap_title = 70,
           wrap_subtitle = 80,
           wrap_x_title = 50,
           wrap_y_title = 50,
           wrap_col_title = 25,
           wrap_caption = 80,
           isMobile = FALSE) {
    
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    data <- data %>% 
      dplyr::ungroup() %>%
      arrange(!!x_var) #fix ggplotly legend bug

    x_var_vector <- dplyr::pull(data, !!x_var)
    y_var_vector <- dplyr::pull(data, !!y_var)
    col_var_vector <- dplyr::pull(data, !!col_var)
    facet_var_vector <- dplyr::pull(data, !!facet_var)
    
    if (!(lubridate::is.Date(x_var_vector) | is.numeric(x_var_vector))) stop("Please use a numeric or date x variable for a line plot")
    if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a line plot")
    if (is.numeric(col_var_vector)) stop("Please use a categorical colour variable for a line plot")
    if (is.numeric(facet_var_vector)) stop("Please use a categorical facet variable for a line plot")
    
    min_y_var_vector <- min(y_var_vector, na.rm = TRUE)
    max_y_var_vector <- max(y_var_vector, na.rm = TRUE)
    if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero == TRUE) {
      y_zero <- FALSE
    }
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (is.null(pal)) pal <- pal_snz
    
    if (points == TRUE) alpha <- 1
    else if (points == FALSE) alpha <- 0
    
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_line(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) 
    
    if (lines == TRUE) plot <- plot +
      geom_line(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var), size = size)

    plot <- plot +
      geom_point(aes(!!x_var, !!y_var, col = !!col_var, group = !!col_var, text = !!tip_var),
        size = point_size, alpha = alpha)

    if (rev_pal == TRUE) pal <- rev(pal)
    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
    
    plot <- plot +
      scale_color_manual(
        values = pal,
        drop = col_drop,
        labels = labels,
        na.value = "#A8A8A8"
      )
    
    if (facet_scales %in% c("fixed", "free_y")) {
      
      if(isMobile == FALSE) x_n <- x_pretty_n
      else if(isMobile == TRUE) x_n <- 4
      
      x_breaks <- pretty(x_var_vector, n = x_n)
      x_limits <- c(min(x_breaks), max(x_breaks))
      
      if (lubridate::is.Date(x_var_vector)) {
        plot <- plot +
          scale_x_date(
            breaks = x_breaks,
            limits = x_limits,
            labels = x_labels
          )
      }
      else if (is.numeric(x_var_vector)) {
        plot <- plot +
          scale_x_continuous(breaks = x_breaks,
                             limits = x_limits,
                             labels = x_labels)
      }
      
    }
    
    if (facet_scales %in% c("fixed", "free_x")) {
      if (y_zero == TRUE) {
        if(max_y_var_vector > 0) y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n)
        if(min_y_var_vector < 0) y_breaks <- pretty(c(y_var_vector, 0), n = y_pretty_n)
        
        if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      else if (y_zero == FALSE) {
        if(y_trans != "log10") y_breaks <- pretty(y_var_vector, n = y_pretty_n)
        if(y_trans == "log10") {
          y_breaks <- pretty(c(0, y_var_vector), n = y_pretty_n) 
          y_breaks <- c(1, y_breaks[y_breaks > 1])
        }
        y_limits <- c(min(y_breaks), max(y_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = c(0, 0),
          breaks = y_breaks,
          limits = y_limits,
          trans = y_trans,
          labels = y_labels,
          oob = scales::rescale_none
        )
    }
    else if (facet_scales %in% c("free", "free_y")) {
      plot <- plot +
        scale_y_continuous(expand = c(0, 0),
                           trans = y_trans,
                           labels = y_labels,
                           oob = scales::rescale_none)
    }
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = col_drop,
        labels = labels,
        na.value = "#A8A8A8"
      ) 
    
    if(min_y_var_vector < 0 & max_y_var_vector > 0 & y_zero_line == TRUE) {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }

    if (isMobile == FALSE) {
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) <= 3) facet_nrow <- 1 
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) > 3) facet_nrow <- 2
      
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, wrap_title),
          subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
          x = stringr::str_wrap(x_title, wrap_x_title),
          y = stringr::str_wrap(y_title, wrap_y_title),
          caption = stringr::str_wrap(caption, wrap_caption)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow) +
        guides(col = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title)))
    }
    else if (isMobile == TRUE) {
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 30),
          caption = stringr::str_wrap(caption, 50)
        )  +
        facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = 1) +
        guides(col = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15)))
    }
    
    return(plot)
  }
