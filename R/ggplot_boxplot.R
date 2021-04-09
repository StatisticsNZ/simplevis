# ggplot boxplot functions

#' @title Theme for box ggplots.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#' 
#' ggplot() +
#'   theme_boxplot("Courier", 9, 7) +
#'   ggtitle("This is a title of a selected font family and size")
theme_boxplot <-
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

#' @title Boxplot ggplot.
#' @description Boxplot ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Defaults to NULL. Required if stat equals "boxplot".
#' @param group_var Unquoted variable to be the grouping variable Defaults to NULL. Only applicable if stat equals "boxplot".
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". If identity is selected, data provided must be grouped by the x_var with ymin, lower, middle, upper, ymax variables. Note "identity" does not provide outliers.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param width Width of the box. Defaults to 0.5.
#' @param alpha The alpha of the fill. Defaults to 0.1. 
#' @param point_size The size of the outliers. Defaults to 1.
#' @param line_size The size of the outlines of boxplots. Defaults to 0.5.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Only applicable to a x variable that is categorical or date.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
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
#'   c('ymin','lower','middle','upper','ymax')))) %>%
#'   tidyr::unnest_wider(boxplot_stats)
#'
#' ggplot_boxplot(plot_data, Species, Petal.Length, stat = "identity")
#' 
ggplot_boxplot <- function(data,
                       x_var,
                       y_var = NULL,
                       group_var = NULL,
                       stat = "boxplot",
                       pal = NULL,
                       width = 0.5,
                       alpha = 0.1,
                       line_size = 0.5,
                       point_size = 1, 
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
                       isMobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) 
  y_var <- rlang::enquo(y_var) #numeric var
  group_var <- rlang::enquo(group_var)
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  if (stat == "boxplot") y_var_vctr <- dplyr::pull(data, !!y_var)
  else if (stat == "identity") y_var_vctr <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))

  if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot")

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
          col = pal, 
          fill = pal,
          width = width,
          size = line_size, 
          alpha = alpha,
          outlier.alpha = 1, 
          outlier.size = point_size 
        )
    }
    else if(!rlang::quo_is_null(group_var)) {
      plot <- plot +
        geom_boxplot(
          aes(x = !!x_var, y = !!y_var, group = !!group_var),
          stat = stat,
          col = pal, 
          fill = pal,
          width = width,
          size = line_size, 
          alpha = alpha,
          outlier.alpha = 1, 
          outlier.size = point_size
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
        col = pal, 
        fill = pal,
        width = width,
        size = line_size, 
        alpha = alpha,
        outlier.alpha = 1, 
        outlier.size = point_size
      )
  }

  if(is.null(x_expand)) x_expand <- waiver()
  if(is.null(y_expand)) y_expand <- c(0, 0)
  
  if (lubridate::is.Date(x_var_vctr)) {
    if(isMobile == FALSE) x_n <- x_pretty_n
    else if(isMobile == TRUE) x_n <- 4
    
    x_breaks <- pretty(x_var_vctr, n = x_n)
    
    plot <- plot +
      scale_x_date(
        expand = x_expand,
        breaks = x_breaks,
        labels = x_labels
      )
  }
  else if (is.numeric(x_var_vctr)) {
    if(isMobile == FALSE) x_n <- x_pretty_n
    else if(isMobile == TRUE) x_n <- 4
    
    x_breaks <- pretty(x_var_vctr, n = x_n)
    
    plot <- plot +
      scale_x_continuous(expand = x_expand,
                         breaks = x_breaks,
                         labels = x_labels,
                         oob = scales::rescale_none)
  }
  else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if (isMobile == FALSE){
      if(is.null(y_labels)) y_labels <- waiver()
      
      plot <- plot +
        scale_x_discrete(expand = x_expand, labels = x_labels)
    }
    else if (isMobile == TRUE){
      if(is.character(x_labels)) {
        plot <- plot +
          scale_x_discrete(expand = x_expand, labels = stringr::str_wrap(x_labels, 20))
      }
      else {
        plot <- plot +
          scale_x_discrete(expand = x_expand, labels = function(x) stringr::str_wrap(x, 20))
      }
    }
  }
  
  if (all(y_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(breaks = c(0, 1), labels = y_labels, limits = c(0, 1))
  }
  else ({
    if (y_balance == TRUE) {
      y_var_vctr <- abs(y_var_vctr)
      y_var_vctr <- c(-y_var_vctr, y_var_vctr)
    }
    if (y_zero == TRUE) { 
      if(max_y_var_vctr > 0) y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n)
      if(min_y_var_vctr < 0) y_breaks <- pretty(c(y_var_vctr, 0), n = y_pretty_n)
      
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
    
    if(isMobile == TRUE) {
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

  if (isMobile == FALSE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        x = stringr::str_wrap(x_title, x_title_wrap),
        y = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) 
  }
  else if (isMobile == TRUE){
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
      coord_flip() +
      theme(panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2)) +
      theme(panel.grid.major.y = element_blank()) +
      theme(axis.text.x = element_text(hjust = 1))  
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
#' @param alpha The alpha of the fill. Defaults to 0.1. 
#' @param line_size The size of the outlines of boxplots. Defaults to 0.5.
#' @param point_size The size of the outliers. Defaults to 1.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_labels Adjust the  x scale labels through a function or vector.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. Only applicable to a x variable that is categorical or date.
#' @param x_title X axis title string. Defaults to "[X title]".
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale. Only applicable where facet_scales equals "fixed" or "free_x".
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param y_labels Adjust the  y scale labels through a function or vector.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_title Y axis title string. Defaults to "[Y title]".
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param y_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. Defaults to NULL, which is TRUE if there are positive and negative values in y_var. Otherwise it is FALSE.  
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
#'   mutate(price_thousands = (price / 1000)) %>%
#'   slice_sample(prop = 0.05)
#'
#' ggplot_boxplot_facet(plot_data, cut, price_thousands, color)
#'
ggplot_boxplot_facet <-
  function(data,
           x_var,
           y_var = NULL,
           facet_var,
           group_var = NULL, 
           stat = "boxplot",
           pal = NULL,
           width = 0.5,
           alpha = 0.1,
           line_size = 0.5,
           point_size = 1,
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
    x_var <- rlang::enquo(x_var) 
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    group_var <- rlang::enquo(group_var) 
    
    x_var_vctr <- dplyr::pull(data, !!x_var) 
    if (stat == "boxplot") y_var_vctr <- dplyr::pull(data, !!y_var)
    else if (stat == "identity") y_var_vctr <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (!is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a boxplot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a boxplot")
    
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
            col = pal, 
            fill = pal,
            width = width,
            size = line_size, 
            alpha = alpha,
            outlier.alpha = 1
          )
      }
      else if(!rlang::quo_is_null(group_var)) {
        plot <- plot +
          geom_boxplot(
            aes(x = !!x_var, y = !!y_var, group = !!group_var),
            stat = stat,
            col = pal, 
            fill = pal,
            width = width,
            size = line_size, 
            alpha = alpha,
            outlier.alpha = 1, 
            outlier.size = point_size
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
          col = pal, 
          fill = pal,
          width = width,
          size = line_size, 
          alpha = alpha,
          outlier.alpha = 1, 
          outlier.size = point_size
        )
    }
    
    if(is.null(x_expand)) x_expand <- waiver()
    if(is.null(y_expand)) y_expand <- c(0, 0)
    
    if (facet_scales %in% c("fixed", "free_y")) {
      
      if (lubridate::is.Date(x_var_vctr)) {
        x_n <- x_pretty_n
        x_breaks <- pretty(x_var_vctr, n = x_n)
        
        plot <- plot +
          scale_x_date(
            expand = x_expand,
            breaks = x_breaks,
            labels = x_labels
          )
      }
      else if (is.numeric(x_var_vctr)) {
        x_n <- x_pretty_n
        x_breaks <- pretty(x_var_vctr, n = x_n)
        
        plot <- plot +
          scale_x_continuous(expand = x_expand,
                             breaks = x_breaks,
                             labels = x_labels,
                             oob = scales::rescale_none)
      }
      else if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
        plot <- plot +
          scale_x_discrete(expand = x_expand, labels = x_labels)
      }
      
    }
    
    if (facet_scales %in% c("fixed", "free_x")) {
      if (y_balance == TRUE) {
        y_var_vctr <- abs(y_var_vctr)
        y_var_vctr <- c(-y_var_vctr, y_var_vctr)
      }
      if (y_zero == TRUE) {
        if(max_y_var_vctr > 0) y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n)
        if(min_y_var_vctr < 0) y_breaks <- pretty(c(y_var_vctr, 0), n = y_pretty_n)
        
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
