# ggplot hbar functions

#' @title Theme for horizontal bar ggplots.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   theme_hbar("Courier", 9, 7) +
#'   ggplot2::ggtitle("This is a title of a selected font family and size")
theme_hbar <-
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
        panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
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
          hjust = 0.425
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
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm")
      )
    )
  }

#' @title Horizontal bar ggplot.
#' @description Horizontal bar ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted categorical variable to be on the y axis. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE.  
#' @param x_trans A string specifying a transformation for the x axis scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param na_grey TRUE or FALSE of whether to provide wide grey bars for NA y_var values. Defaults to FALSE.
#' @param title Title string. Defaults to [Title].
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param x_title X axis title string. Defaults to [X title].
#' @param y_title Y axis title string. Defaults to [Y title].
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
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   group_by(cut) %>%
#'   summarise(average_price = mean(price)) %>%
#'   mutate(average_price_thousands = round(average_price / 1000, 1)) 
#'
#' plot <- ggplot_hbar(data = plot_data, x_var = average_price_thousands, y_var = cut,
#'    title = "Average diamond price by cut",
#'    x_title = "Average price ($US thousands)",
#'    y_title = "Cut")
#'
#' plot
#' 
#' plotly::ggplotly(plot)
ggplot_hbar <- function(data,
                        x_var,
                        y_var,
                        tip_var = NULL,
                        x_labels = waiver(),
                        x_zero = TRUE,
                        x_zero_line = TRUE,
                        x_trans = "identity",
                        x_pretty_n = 6,
                        y_rev = FALSE,
                        y_labels = waiver(),
                        pal = NULL,
                        width = 0.75, 
                        na_grey = FALSE,
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
  y_var <- rlang::enquo(y_var) #categorical var
  tip_var <- rlang::enquo(tip_var)
  
  x_var_vector <- dplyr::pull(data, !!x_var)
  y_var_vector <- dplyr::pull(data, !!y_var)
  
  if (!is.numeric(x_var_vector)) stop("Please use a numeric x variable for a horizontal bar plot")
  if (is.numeric(y_var_vector)) stop("Please use a categorical y variable for a horizontal bar plot")
  
  min_x_var_vector <- min(x_var_vector, na.rm = TRUE)
  max_x_var_vector <- max(x_var_vector, na.rm = TRUE)
  if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero == TRUE) {
    x_zero <- FALSE
  }
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }
  
  if (is.factor(y_var_vector) & y_rev == FALSE){
    data <- data %>%
      dplyr::mutate(!!y_var := forcats::fct_rev(!!y_var))
  }
  else if (is.character(y_var_vector)) {
    if (y_rev == FALSE){
      data <- data %>%
        dplyr::mutate(!!y_var := forcats::fct_reorder(!!y_var, !!x_var))
    }
    else if (y_rev == TRUE){
      data <- data %>%
        dplyr::mutate(!!y_var := forcats::fct_reorder(!!y_var, !!x_var, .desc = TRUE))
    }
  }
  
  if (is.null(pal)) pal <- pal_snz
  
  plot <- ggplot(data) +
    coord_flip() +
    theme_hbar(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
      geom_col(aes(x = !!y_var, y = !!x_var, text = !!tip_var), fill = pal[1], width = width)

  if (all(x_var_vector == 0, na.rm = TRUE)) {
    x_limits <- c(0, 1)
    
    plot <- plot +
      ggplot2::scale_x_continuous(expand = c(0, 0), breaks = c(0, 1), labels = x_labels, limits = x_limits)
  }
  else ({
    
    if(isMobile == FALSE) x_n <- x_pretty_n
    else if(isMobile == TRUE) x_n <- 4
    
    if (x_zero == TRUE) {
      if(max_x_var_vector > 0) x_breaks <- pretty(c(0, x_var_vector), n = x_n)
      if(min_x_var_vector < 0) x_breaks <- pretty(c(x_var_vector, 0), n = x_n)
      
      if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
      x_limits <- c(min(x_breaks), max(x_breaks))
    }
    else if (x_zero == FALSE) {
      if(x_trans != "log10") x_breaks <- pretty(x_var_vector, n = x_n)
      if(x_trans == "log10") {
        x_breaks <- pretty(c(0, x_var_vector), n = x_n) 
        x_breaks <- c(1, x_breaks[x_breaks > 1])
      }
      x_limits <- c(min(x_breaks), max(x_breaks))
    }
    
    plot <- plot +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = x_breaks,
        limits = x_limits,
        labels = x_labels,
        trans = x_trans,
        oob = scales::rescale_none
      )
  })
  
  if(na_grey == TRUE) {
    
    na_data <- data %>% 
      filter(is.na(!!x_var)) %>% 
      add_tip(c(rlang::as_name(y_var), rlang::as_name(x_var)))

    if(nrow(na_data) != 0){
      if(x_limits[2] > 0){
        plot <- plot +
          geom_col(aes(x = !!y_var, y = x_limits[2], text = .data$tip_text),
                   fill = "#F0F0F0", width = (1 + (1 - width)),
                   data = na_data)
      }
      if(x_limits[1] < 0){
        plot <- plot +
          geom_col(aes(x = !!y_var, y = x_limits[1], text = .data$tip_text),
                   fill = "#F0F0F0", width = (1 + (1 - width)),
                   data = na_data)
      }
    }
  }
  
  plot <- plot +
    scale_x_discrete(labels = y_labels)
  
  if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero_line == TRUE) {
    plot <- plot +
      ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }

  if (isMobile == FALSE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, wrap_title),
        subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
        y = stringr::str_wrap(x_title, wrap_x_title),
        x = stringr::str_wrap(y_title, wrap_y_title),
        caption = stringr::str_wrap(caption, wrap_caption)
      ) 
  }
  else if (isMobile == TRUE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        y = stringr::str_wrap(x_title, 20),
        x = stringr::str_wrap(y_title, 20),
        caption = stringr::str_wrap(caption, 50)
      ) 
  }
  
  return(plot)
}

#' @title Horizontal bar ggplot that is coloured.
#' @description Horizontal bar ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted categorical variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE.
#' @param x_trans A string specifying a transformation for the x axis scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param col_rev TRUE or FALSE of whether bar fill order from left to right is reversed from default. Defaults to FALSE.
#' @param col_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to FALSE.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param legend_ncol The number of columns in the legend.
#' @param width Width of bars. Defaults to 0.75.
#' @param title Title string. Defaults to [Title].
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param x_title X axis title string. Defaults to [X title].
#' @param y_title Y axis title string. Defaults to [Y title].
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
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   group_by(cut, clarity) %>%
#'   summarise(average_price = mean(price)) %>%
#'   mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
#'   ungroup()
#' 
#' plot <- ggplot_hbar_col(data = plot_data, 
#'                         x_var = average_price_thousands, 
#'                         y_var = cut, 
#'                         col_var = clarity, 
#'                         legend_ncol = 4,
#'                         title = "Average diamond price by cut and clarity", 
#'                         x_title = "Average price ($US thousands)", 
#'                         y_title = "Cut")
#' 
#' plot
#'
#' plotly::ggplotly(plot)
ggplot_hbar_col <-
  function(data,
           x_var,
           y_var,
           col_var,
           tip_var = NULL,
           x_labels = waiver(),
           x_zero = TRUE,
           x_zero_line = TRUE,
           x_trans = "identity",
           x_pretty_n = 6,
           y_rev = FALSE,
           y_labels = waiver(),
           col_rev = FALSE,
           col_drop = FALSE,
           position = "stack",
           pal = NULL,
           legend_ncol = 3,
           width = 0.75, 
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
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #categorical var
    col_var <- rlang::enquo(col_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    x_var_vector <- dplyr::pull(data, !!x_var)
    y_var_vector <- dplyr::pull(data, !!y_var)
    col_var_vector <- dplyr::pull(data, !!col_var)
    
    if (!is.numeric(x_var_vector)) stop("Please use a numeric x variable for a horizontal bar plot")
    if (is.numeric(y_var_vector)) stop("Please use a categorical y variable for a horizontal bar plot")
    if (is.numeric(col_var_vector)) stop("Please use a categorical colour variable for a horizontal bar plot")
    
    if (position == "stack" & x_trans != "identity") message("simplevis may not perform correctly using an x scale other than identity where position equals stack")
    if (position == "stack" & x_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and x_zero equal to FALSE")
    
    min_x_var_vector <- min(x_var_vector, na.rm = TRUE)
    max_x_var_vector <- max(x_var_vector, na.rm = TRUE)
    if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero == TRUE) {
      x_zero <- FALSE
    }
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (y_rev == FALSE){
      data <- data %>%
        dplyr::mutate(!!y_var := forcats::fct_rev(!!y_var))
    }
    if (col_rev == FALSE){
      data <- data %>%
        dplyr::mutate(!!col_var := forcats::fct_rev(!!col_var))
    }
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (is.null(pal)) pal <- pal_snz
    
    plot <- ggplot(data) +
      coord_flip() +
      theme_hbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(
        x = !!y_var, y = !!x_var, fill = !!col_var, text = !!tip_var), width = width, position = position2)

    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
    
    if (is.factor(col_var_vector) & !is.null(levels(col_var_vector))) {
      pal <- pal[1:length(levels(col_var_vector))]
    }
    else pal <- pal[1:length(unique(col_var_vector))]
    pal <- rev(pal)
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by_at(vars(!!y_var)) %>%
        dplyr::summarise_at(vars(!!x_var), list( ~ (sum(., na.rm = TRUE)))) %>%
        dplyr::ungroup()
      
      x_var_vector <- dplyr::pull(data_sum, !!x_var)
      
    }
    
    if (all(x_var_vector == 0, na.rm = TRUE)) {
      x_limits <- c(0, 1)
      
      plot <- plot +
        ggplot2::scale_x_continuous(expand = c(0, 0), breaks = c(0, 1), labels = x_labels, limits = x_limits)
    }
    else ({
      
      if(isMobile == FALSE) x_n <- x_pretty_n
      else if(isMobile == TRUE) x_n <- 4
      
      if (x_zero == TRUE) {
        if(max_x_var_vector > 0) x_breaks <- pretty(c(0, x_var_vector), n = x_n)
        if(min_x_var_vector < 0) x_breaks <- pretty(c(x_var_vector, 0), n = x_n)
        
        if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      else if (x_zero == FALSE) {
        if(x_trans != "log10") x_breaks <- pretty(x_var_vector, n = x_n)
        if(x_trans == "log10") {
          x_breaks <- pretty(c(0, x_var_vector), n = x_n) 
          x_breaks <- c(1, x_breaks[x_breaks > 1])
        }
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      
      if(position == "stack" & all(dplyr::between(x_var_vector, 99, 101))) x_limits <- c(0, 100)
      
      plot <- plot +
        scale_y_continuous(
          expand = c(0, 0),
          breaks = x_breaks,
          limits = x_limits,
          labels = x_labels,
          trans = x_trans,
          oob = scales::rescale_none
        )
    })
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = col_drop,
        labels = labels,
        na.value = "#A8A8A8"
      ) +
      scale_x_discrete(labels = y_labels)

    if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero_line == TRUE) {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }

    if (isMobile == FALSE){
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, wrap_title),
          subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
          y = stringr::str_wrap(x_title, wrap_x_title),
          x = stringr::str_wrap(y_title, wrap_y_title),
          caption = stringr::str_wrap(caption, wrap_caption)
        ) +
        guides(fill = guide_legend(
          ncol = legend_ncol,
          byrow = TRUE,
          reverse = TRUE,
          title = stringr::str_wrap(col_title, wrap_col_title)
        ))
    }
    else if (isMobile == TRUE){
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          y = stringr::str_wrap(x_title, 20),
          x = stringr::str_wrap(y_title, 20),
          caption = stringr::str_wrap(caption, 50)
        ) +
        guides(fill = guide_legend(
          ncol = 1,
          byrow = TRUE,
          reverse = TRUE,
          title = stringr::str_wrap(col_title, 15)
        )) 
    }
    
    return(plot)
  }

#' @title Horizontal bar ggplot that is facetted.
#' @description Horizontal bar ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted categorical variable to be on the y axis. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE.
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. Not applicable where isMobile equals TRUE.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_nrow The number of rows of facetted plots. Defaults to NULL, which generally chooses 2 rows. Not applicable to where isMobile is TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param na_grey TRUE or FALSE of whether to provide wide grey bars for NA y_var values. Defaults to FALSE. Only applicable where facet_scales = "fixed" or "free_y". 
#' @param title Title string. Defaults to [Title].
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param x_title X axis title string. Defaults to [X title].
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults NULL.
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
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   group_by(cut, clarity) %>%
#'   summarise(average_price = mean(price)) %>%
#'   mutate(average_price_thousands = round(average_price / 1000, 1)) 
#'
#' plot <- ggplot_hbar_facet(data = plot_data, x_var = average_price_thousands,
#'                           y_var = cut, facet_var = clarity,
#'                          title = "Average diamond price by cut and clarity", 
#'                          x_title = "Average price ($US thousands)", 
#'                          y_title = "Cut")
#'
#' plot
#'
#' plotly::ggplotly(plot)
ggplot_hbar_facet <-
  function(data,
           x_var,
           y_var,
           facet_var,
           tip_var = NULL,
           x_labels = waiver(),
           x_zero = TRUE,
           x_zero_line = TRUE,
           x_trans = "identity",
           x_pretty_n = 5,
           y_rev = FALSE,
           y_labels = waiver(),
           facet_scales = "fixed",
           facet_nrow = NULL,
           pal = NULL,
           width = 0.75, 
           title = "[Title]",
           na_grey = FALSE,
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
    y_var <- rlang::enquo(y_var) #categorical var
    x_var <- rlang::enquo(x_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    y_var_vector <- dplyr::pull(data, !!y_var)
    x_var_vector <- dplyr::pull(data, !!x_var)
    facet_var_vector <- dplyr::pull(data, !!facet_var)
    
    if (is.numeric(y_var_vector)) stop("Please use a numeric x variable for a horizontal bar plot")
    if (!is.numeric(x_var_vector)) stop("Please use a categorical y variable for a horizontal bar plot")
    if (is.numeric(facet_var_vector)) stop("Please use a categorical facet variable for a horizontal bar plot")
    
    min_x_var_vector <- min(x_var_vector, na.rm = TRUE)
    max_x_var_vector <- max(x_var_vector, na.rm = TRUE)
    if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero == TRUE) {
      x_zero <- FALSE
    }
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (is.factor(y_var_vector) & y_rev == FALSE){
      data <- data %>%
        dplyr::mutate(!!y_var := forcats::fct_rev(!!y_var))
    }
    else if (is.character(y_var_vector)) {
      if (y_rev == FALSE){
        data <- data %>%
          dplyr::mutate(!!y_var := forcats::fct_rev(!!y_var))
      }
    }
    
    if (is.null(pal)) pal <- pal_snz
    
    plot <- ggplot(data) +
      coord_flip() +
      theme_hbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(x = !!y_var, y = !!x_var, text = !!tip_var), fill = pal[1], width = width)
    

    if (facet_scales %in% c("fixed", "free_y")) {
      if(isMobile == FALSE) x_n <- x_pretty_n
      else if(isMobile == TRUE) x_n <- 4
      
      if (x_zero == TRUE) {
        if(max_x_var_vector > 0) x_breaks <- pretty(c(0, x_var_vector), n = x_n)
        if(min_x_var_vector < 0) x_breaks <- pretty(c(x_var_vector, 0), n = x_n)
        
        if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      else if (x_zero == FALSE) {
        if(x_trans != "log10") x_breaks <- pretty(x_var_vector)
        if(x_trans == "log10") {
          x_breaks <- pretty(c(0, x_var_vector)) 
          x_breaks <- c(1, x_breaks[x_breaks > 1])
        }
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = c(0, 0),
          breaks = x_breaks,
          limits = x_limits,
          labels = x_labels,
          trans = x_trans,
          oob = scales::rescale_none
        )
      
      if(na_grey == TRUE) {
        
        na_data <- data %>% 
          filter(is.na(!!x_var)) %>% 
          add_tip(c(rlang::as_name(y_var), rlang::as_name(x_var)))
        
          if(x_limits[2] > 0){
            plot <- plot +
              geom_col(aes(x = !!y_var, y = x_limits[2], text = .data$tip_text),
                       fill = "#F0F0F0", width = (1 + (1 - width)),
                       data = na_data)
          }
          if(x_limits[1] < 0){
            plot <- plot +
              geom_col(aes(x = !!y_var, y = x_limits[1], text = .data$tip_text),
                       fill = "#F0F0F0", width = (1 + (1 - width)),
                       data = na_data)
          }
        }
    }
    
    if (facet_scales %in% c("free", "free_x")) {
      plot <- plot +
        scale_y_continuous(expand = c(0, 0),
                           labels = x_labels,
                           trans = x_trans,
                           oob = scales::rescale_none)
    }
    
    plot <- plot +
      scale_x_discrete(labels = y_labels)
    
    if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero_line == TRUE) {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }
    
    if (isMobile == FALSE){
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) <= 3) facet_nrow <- 1 
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) > 3) facet_nrow <- 2
      
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, wrap_title),
          subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
          y = stringr::str_wrap(x_title, wrap_x_title),
          x = stringr::str_wrap(y_title, wrap_y_title),
          caption = stringr::str_wrap(caption, wrap_caption)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow)
    }
    else if (isMobile == TRUE){
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          y = stringr::str_wrap(x_title, 20),
          x = stringr::str_wrap(y_title, 20),
          caption = stringr::str_wrap(caption, 50)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = 1)
    }
    
    return(plot)
  }

#' @title Horizontal bar ggplot that is coloured and facetted.
#' @description Horizontal bar ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x axis. Required input.
#' @param y_var Unquoted categorical variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param tip_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot). Defaults to NULL.
#' @param x_labels Argument to adjust the format of the x scale labels.
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero line in for when values are above and below zero. Defaults to TRUE.
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. Not applicable where isMobile equals TRUE.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param col_rev TRUE or FALSE of whether bar fill order from left to right is reversed from default. Defaults to FALSE.
#' @param col_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to FALSE.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_nrow The number of rows of facetted plots. Defaults to NULL, which generally chooses 2 rows. Not applicable to where isMobile is TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param legend_ncol The number of columns in the legend.
#' @param width Width of bars. Defaults to 0.75.
#' @param title Title string. Defaults to [Title].
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param x_title X axis title string. Defaults to [X title].
#' @param y_title Y axis title string. Defaults to [Y title].
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
#' plot_data <- ggplot2::diamonds %>%
#'   mutate(cut = stringr::str_to_sentence(cut)) %>%
#'   group_by(cut, clarity, color) %>%
#'   summarise(average_price = mean(price)) %>%
#'   mutate(average_price_thousands = round(average_price / 1000, 1))
#'
#' plot <- ggplot_hbar_col_facet(data = plot_data, x_var = average_price_thousands,
#'                               y_var = color, col_var = clarity, facet_var = cut,
#'                               title = "Average diamond price by colour, clarity and cut", 
#'                               x_title = "Average price ($US thousands)", 
#'                               y_title = "Colour")
#'
#' plot
#'
#' plotly::ggplotly(plot)
ggplot_hbar_col_facet <-
  function(data,
           x_var,
           y_var,
           col_var,
           facet_var,
           tip_var = NULL,
           x_labels = waiver(),
           x_zero = TRUE,
           x_zero_line = TRUE,
           x_trans = "identity",
           x_pretty_n = 5,
           y_rev = FALSE,
           y_labels = waiver(),
           col_rev = FALSE,
           col_drop = FALSE,
           position = "stack",
           facet_scales = "fixed",
           facet_nrow = NULL,
           pal = NULL,
           legend_ncol = 3,
           width = 0.75, 
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
    
    data <- dplyr::ungroup(data)
    y_var <- rlang::enquo(y_var) #categorical var
    x_var <- rlang::enquo(x_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    y_var_vector <- dplyr::pull(data, !!y_var)
    x_var_vector <- dplyr::pull(data, !!x_var)
    col_var_vector <- dplyr::pull(data, !!col_var)
    facet_var_vector <- dplyr::pull(data, !!facet_var)
    
    if (is.numeric(y_var_vector)) stop("Please use a numeric x variable for a horizontal bar plot")
    if (!is.numeric(x_var_vector)) stop("Please use a categorical y variable for a horizontal bar plot")
    if (is.numeric(facet_var_vector)) stop("Please use a categorical facet variable for a horizontal bar plot")
    
    if (position == "stack" & x_trans != "identity") message("simplevis may not perform correctly using an x scale other than identity where position equals stack")
    if (position == "stack" & x_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and x_zero equal to FALSE")
    
    min_x_var_vector <- min(x_var_vector, na.rm = TRUE)
    max_x_var_vector <- max(x_var_vector, na.rm = TRUE)
    if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero == TRUE) {
      x_zero <- FALSE
    }
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (y_rev == FALSE){
      data <- data %>%
        dplyr::mutate(!!y_var := forcats::fct_rev(!!y_var))
    }
    if (col_rev == FALSE){
      data <- data %>%
        dplyr::mutate(!!col_var := forcats::fct_rev(!!col_var))
    }
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (is.null(pal)) pal <- pal_snz
    
    plot <- ggplot(data) +
      coord_flip() +
      theme_hbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_col(aes(x = !!y_var, y = !!x_var, fill = !!col_var, text = !!tip_var), width = width, position = position2)

    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
    
    if (is.factor(col_var_vector) & !is.null(levels(col_var_vector))) {
      pal <- pal[1:length(levels(col_var_vector))]
    }
    else pal <- pal[1:length(unique(col_var_vector))]
    pal <- rev(pal)
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by_at(vars(!!y_var, !!facet_var)) %>%
        dplyr::summarise_at(vars(!!x_var), list( ~ (sum(., na.rm = TRUE)))) %>%
        dplyr::ungroup()
      
      x_var_vector <- dplyr::pull(data_sum, !!x_var)
    }
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if(isMobile == FALSE) x_n <- x_pretty_n
      else if(isMobile == TRUE) x_n <- 4
      
      if (x_zero == TRUE) {
        if(max_x_var_vector > 0) x_breaks <- pretty(c(0, x_var_vector), n = x_n)
        if(min_x_var_vector < 0) x_breaks <- pretty(c(x_var_vector, 0), n = x_n)
        
        if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      else if (x_zero == FALSE) {
        if(x_trans != "log10") x_breaks <- pretty(x_var_vector, n = x_n)
        if(x_trans == "log10") {
          x_breaks <- pretty(c(0, x_var_vector), n = x_n) 
          x_breaks <- c(1, x_breaks[x_breaks > 1])
        }
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = c(0, 0),
          breaks = x_breaks,
          limits = x_limits,
          trans = x_trans,
          labels = x_labels,
          oob = scales::rescale_none
        )
    }
    if (facet_scales %in% c("free", "free_x")) {
      plot <- plot +
        scale_y_continuous(expand = c(0, 0),
                           trans = x_trans,
                           labels = x_labels,
                           oob = scales::rescale_none)
    }
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = col_drop,
        labels = labels,
        na.value = "#A8A8A8"
      ) +
      scale_x_discrete(labels = y_labels)
    
    if(min_x_var_vector < 0 & max_x_var_vector > 0 & x_zero_line == TRUE) {
      plot <- plot +
        ggplot2::geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }

    if (isMobile == FALSE){
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) <= 3) facet_nrow <- 1
      if (is.null(facet_nrow) & length(unique(facet_var_vector)) > 3) facet_nrow <- 2
      
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, wrap_title),
          subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
          y = stringr::str_wrap(x_title, wrap_x_title),
          x = stringr::str_wrap(y_title, wrap_y_title),
          caption = stringr::str_wrap(caption, wrap_caption)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow) +
        guides(fill = guide_legend(
          ncol = legend_ncol,
          byrow = TRUE,
          reverse = TRUE,
          title = stringr::str_wrap(col_title, wrap_col_title)
        ))
      
    }
    else if (isMobile == TRUE){
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          y = stringr::str_wrap(x_title, 20),
          x = stringr::str_wrap(y_title, 20),
          caption = stringr::str_wrap(caption, 50)
        ) +
        guides(fill = guide_legend(
          ncol = 1,
          byrow = TRUE,
          reverse = TRUE,
          title = stringr::str_wrap(col_title, 15)
        )) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = 1)
    }
    
    return(plot)
  }
