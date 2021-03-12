# ggplot hbar functions

#' @title Theme for horizontal bar ggplots.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#' 
#' ggplot() +
#'   theme_hbar("Courier", 9, 7) +
#'   ggtitle("This is a title of a selected font family and size")
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
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.  
#' @param x_trans A string specifying a transformation for the x axis scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_na_bar TRUE or FALSE of whether to make NA x_var values infinity with a light grey colour to emphasise them. Defaults to FALSE.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param title Title string. Defaults to [Title].
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param x_title X axis title string. Defaults to [X title].
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
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
#'   mutate(average_price = round(average_price / 1000, 1)) 
#'
#' ggplot_hbar(plot_data, average_price, cut,
#'    title = "Average diamond price by cut",
#'    x_title = "Average price ($US thousands)",
#'    y_title = "Cut")
#'
ggplot_hbar <- function(data,
                        x_var,
                        y_var,
                        tip_var = NULL,
                        x_labels = waiver(),
                        x_zero = TRUE,
                        x_zero_line = NULL,
                        x_trans = "identity",
                        x_pretty_n = 6,
                        x_expand = NULL,
                        x_balance = FALSE,
                        x_na_bar = FALSE,
                        y_rev = FALSE,
                        y_labels = NULL,
                        y_expand = NULL,
                        pal = NULL,
                        width = 0.75, 
                        title = "[Title]",
                        subtitle = NULL,
                        x_title = "[X title]",
                        y_title = "[Y title]",
                        caption = NULL,
                        font_family = "Helvetica",
                        font_size_title = NULL,
                        font_size_body = NULL,
                        title_wrap = 70,
                        subtitle_wrap = 80,
                        x_title_wrap = 50,
                        y_title_wrap = 50,
                        caption_wrap = 80,
                        isMobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) #numeric var
  y_var <- rlang::enquo(y_var) #categorical var
  tip_var <- rlang::enquo(tip_var)
  
  x_var_vctr <- dplyr::pull(data, !!x_var)
  y_var_vctr <- dplyr::pull(data, !!y_var)
  
  if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a horizontal bar plot")
  
  min_x_var_vctr <- min(x_var_vctr, na.rm = TRUE)
  max_x_var_vctr <- max(x_var_vctr, na.rm = TRUE)
  
  x_above_and_below_zero <- ifelse(min_x_var_vctr < 0 & max_x_var_vctr > 0, TRUE, FALSE)
  
  if(x_above_and_below_zero == TRUE) x_zero <- FALSE
  
  if(is.null(x_zero_line)) {
    if(x_above_and_below_zero == TRUE | x_balance == TRUE) x_zero_line <- TRUE
    else(x_zero_line <- FALSE)
  }
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }
  
  if (is.factor(y_var_vctr) & y_rev == FALSE){
    data <- data %>%
      dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
  }
  else if (is.character(y_var_vctr)) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_reorder(.x, !!x_var, .desc = y_rev)))
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
  
  if(is.null(x_expand)) x_expand <- c(0, 0)
  if(is.null(y_expand)) y_expand <- waiver()
  
  if (all(x_var_vctr == 0, na.rm = TRUE)) {
    plot <- plot +
      scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
  }
  else ({
    if (x_balance == TRUE) {
      x_var_vctr <- abs(x_var_vctr)
      x_var_vctr <- c(-x_var_vctr, x_var_vctr)
    }
    if (x_zero == TRUE) {
      x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n)
      if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
      x_limits <- c(min(x_breaks), max(x_breaks))
    }
    else if (x_zero == FALSE) {
      if(x_trans != "log10") x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
      if(x_trans == "log10") {
        x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n) 
        x_breaks <- c(1, x_breaks[x_breaks > 1])
      }
      x_limits <- c(min(x_breaks), max(x_breaks))
    }
    
    if(isMobile == TRUE) {
      x_breaks <- x_limits
      if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
    }
    
    plot <- plot +
      scale_y_continuous(
        expand = x_expand,
        breaks = x_breaks,
        limits = x_limits,
        labels = x_labels,
        trans = x_trans,
        oob = scales::rescale_none
      )
  })
  
  if(x_na_bar == TRUE) {
    na_data <- dplyr::filter(data, is.na(!!x_var))
    
    if(nrow(na_data) != 0) {
      if(x_limits[1] >= 0 & x_limits[2] > 0){
        plot <- plot +
          geom_col(aes(x = !!y_var, y = x_limits[2], text = !!tip_var),
                   fill = "#F5F5F5", width = width, 
                   data = na_data)
      }
      else if(x_limits[1] < 0 & x_limits[2] <= 0) {
        plot <- plot +
          geom_col(aes(x = !!y_var, y = x_limits[1], text = !!tip_var),
                   fill = "#F5F5F5", width = width, 
                   data = na_data)        
      }
      else if(x_limits[1] < 0 & x_limits[2] > 0) {
        ggplotly_adjust <- (x_limits[2] - x_limits[1]) / 1000000 # hack to fix ggplotly bug #1929
        
        plot <- plot +
          geom_col(aes(x = !!y_var, y = x_limits[2], text = !!tip_var),
                   fill = "#F5F5F5", width = width, 
                   data = na_data) +
          geom_col(aes(x = !!y_var, y = x_limits[1] + ggplotly_adjust, text = !!tip_var),
                   fill = "#F5F5F5", width = width, 
                   data = na_data)
      }
    }
  }
  
  if (isMobile == FALSE){
    if(is.null(y_labels)) y_labels <- waiver()
    
    plot <- plot +
      scale_x_discrete(expand = y_expand, labels = y_labels)
  }
  else if (isMobile == TRUE){
    if(is.character(y_labels)) {
      plot <- plot +
        scale_x_discrete(expand = y_expand, labels = stringr::str_wrap(y_labels, 20))
    }
    else {
      plot <- plot +
        scale_x_discrete(expand = y_expand, labels = function(x) stringr::str_wrap(x, 20))
    }
  }
  
  if(x_zero_line == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
  }
  
  if (isMobile == FALSE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        y = stringr::str_wrap(x_title, x_title_wrap),
        x = stringr::str_wrap(y_title, y_title_wrap),
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
        y = stringr::str_wrap(x_title, 20),
        x = stringr::str_wrap(y_title, 20),
        caption = stringr::str_wrap(caption, 50)
      ) +
      theme(axis.text.x = element_text(hjust = 0.75))
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
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.
#' @param x_trans A string specifying a transformation for the x axis scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. Not applicable where isMobile equals TRUE.
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale.
#' @param x_na_bar TRUE or FALSE of whether to make NA x_var values infinity with a light grey colour to emphasise them. Defaults to FALSE.
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param col_rev TRUE or FALSE of whether bar fill order from left to right is reversed from default. Defaults to FALSE.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param pal_rev TRUE or FALSE of whether to reverse the pal.
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
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. Not applicable where isMobile equals TRUE.
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. Not applicable where isMobile equals TRUE.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
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
           tip_var = NULL,
           x_labels = waiver(),
           x_zero = TRUE,
           x_zero_line = NULL,
           x_trans = "identity",
           x_pretty_n = 6,
           x_expand = NULL,
           x_balance = FALSE,
           x_na_bar = FALSE,
           y_rev = FALSE,
           y_labels = waiver(),
           y_expand = NULL,
           col_rev = FALSE,
           position = "stack",
           pal = NULL,
           pal_rev = FALSE,
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
           title_wrap = 70,
           subtitle_wrap = 80,
           x_title_wrap = 50,
           y_title_wrap = 50,
           wrap_col_title = 25,
           caption_wrap = 80,
           isMobile = FALSE) {
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #numeric var
    y_var <- rlang::enquo(y_var) #categorical var
    col_var <- rlang::enquo(col_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
    y_var_vctr <- dplyr::pull(data, !!y_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    
    if (!is.numeric(x_var_vctr)) stop("Please use a numeric x variable for a horizontal bar plot")
    if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a horizontal bar plot")
    if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a horizontal bar plot")
    if (x_na_bar == TRUE & position == "stack") stop("Please use a position of dodge for where x_na_bar equals TRUE")
    
    if (position == "stack" & x_trans != "identity") message("simplevis may not perform correctly using an x scale other than identity where position equals stack")
    if (position == "stack" & x_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and x_zero equal to FALSE")
    
    min_x_var_vctr <- min(x_var_vctr, na.rm = TRUE)
    max_x_var_vctr <- max(x_var_vctr, na.rm = TRUE)
    
    x_above_and_below_zero <- ifelse(min_x_var_vctr < 0 & max_x_var_vctr > 0, TRUE, FALSE)
    
    if(x_above_and_below_zero == TRUE) x_zero <- FALSE
    
    if(is.null(x_zero_line)) {
      if(x_above_and_below_zero == TRUE | x_balance == TRUE) x_zero_line <- TRUE
      else(x_zero_line <- FALSE)
    }
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if(!is.logical(col_var_vctr)){
      if (y_rev == FALSE){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      }
      if (col_rev == FALSE){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
      }
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
      ) 
    
    if (!is.null(legend_labels)) labels <- rev(legend_labels)
    if (is.null(legend_labels)) labels <- waiver()
    
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      pal <- pal[1:length(levels(col_var_vctr))]
    }
    else pal <- pal[1:length(unique(col_var_vctr))]
    
    if(pal_rev == FALSE) pal <- rev(pal)
    
    if (!is.null(pal) & x_na_bar == TRUE) { 
      if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
        names(pal) <- levels(col_var_vctr)
      }
      else names(pal) <- unique(col_var_vctr)
      
      pal <- c(pal, "Not available" = "#f5f5f5")
    }
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by(dplyr::across(!!y_var)) %>%
        dplyr::summarise(dplyr::across(!!x_var, ~sum(.x, na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      x_var_vctr <- dplyr::pull(data_sum, !!x_var)
    }
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- waiver()
    
    if (isMobile == FALSE){
      if(is.null(y_labels)) y_labels <- waiver()
      
      plot <- plot +
        scale_x_discrete(expand = y_expand, labels = y_labels)
    }
    else if (isMobile == TRUE){
      if(is.character(y_labels)) {
        plot <- plot +
          scale_x_discrete(expand = y_expand, labels = stringr::str_wrap(y_labels, 20))
      }
      else {
        plot <- plot +
          scale_x_discrete(expand = y_expand, labels = function(x) stringr::str_wrap(x, 20))
      }
    }
    
    if (all(x_var_vctr == 0, na.rm = TRUE)) {
      plot <- plot +
        scale_y_continuous(expand = x_expand, breaks = c(0, 1), labels = x_labels, limits = c(0, 1))
    }
    else ({
      if (x_balance == TRUE) {
        x_var_vctr <- abs(x_var_vctr)
        x_var_vctr <- c(-x_var_vctr, x_var_vctr)
      }
      if (x_zero == TRUE) {
        x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n)
        if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      else if (x_zero == FALSE) {
        if(x_trans != "log10") x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
        if(x_trans == "log10") {
          x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n) 
          x_breaks <- c(1, x_breaks[x_breaks > 1])
        }
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      
      if(position == "stack" & all(dplyr::between(x_var_vctr, 99, 101))) x_limits <- c(0, 100)
      
      if(isMobile == TRUE) {
        x_breaks <- x_limits
        if (min(x_limits) < 0 & max(x_limits > 0)) x_breaks <- c(x_limits[1], 0, x_limits[2])
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          labels = x_labels,
          trans = x_trans,
          oob = scales::rescale_none
        )
    })
    
    if(x_na_bar == FALSE) {
      plot <- plot +
        geom_col(aes(
          x = !!y_var, y = !!x_var, fill = !!col_var, text = !!tip_var), 
          width = width, 
          position = position2)
    }
    else if(x_na_bar == TRUE) {
      data <- data %>% 
        dplyr::mutate(col_var2 = ifelse(is.na(!!x_var), NA, as.character(!!col_var))) %>%
        dplyr::mutate(col_var2 = forcats::fct_rev(forcats::fct_explicit_na(.data$col_var2, "Not available"))) 
      
      if(is.character(y_var_vctr)) {
        all_na <- data %>% 
          group_by(!!y_var) %>%
          summarise(all_na = all(is.na(!!x_var))) %>% 
          filter(all_na == TRUE) %>% 
          mutate(dplyr::across(!!y_var, ~as.character(.x))) %>% 
          pull(!!y_var)
        
        data <- data %>% 
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_reorder(.x, !!x_var, .fun = stats::median, na.rm = TRUE)))  %>% 
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_relevel(.x, all_na)))  
      }
      
      if(x_limits[1] >= 0 & x_limits[2] > 0) {
        data <- data %>%
          dplyr::mutate(x_var2 = ifelse(is.na(!!x_var), x_limits[2], !!x_var))
        
        plot <- plot +
          geom_col(aes(x = !!y_var, y = .data$x_var2, fill = .data$col_var2, group = !!col_var, text = !!tip_var), 
                   width = width, position = position2, data = data)
      }
      else if(x_limits[1] < 0 & x_limits[2] <= 0) {
        data <- data %>%
          dplyr::mutate(x_var2 = ifelse(is.na(!!x_var), x_limits[1], !!x_var))
        
        plot <- plot +
          geom_col(aes(x = !!y_var, y = .data$x_var2, fill = .data$col_var2, group = !!col_var, text = !!tip_var), 
                   width = width, position = position2, data = data)
      }
      else if(x_limits[1] < 0 & x_limits[2] > 0) {
        data <- data %>%
          dplyr::mutate(col_var3 = .data$col_var2) %>% 
          dplyr::mutate(x_var2 = ifelse(is.na(!!x_var), x_limits[1], !!x_var)) %>%
          dplyr::mutate(x_var3 = ifelse(is.na(!!x_var), x_limits[2], !!x_var))
        
        plot <- plot +
          geom_col(aes(x = !!y_var, y = .data$x_var2, fill = .data$col_var2, group = !!col_var, text = !!tip_var), 
                   width = width, position = position2, data = data) +
          geom_col(aes(x = !!y_var, y = .data$x_var3, fill = .data$col_var2, group = !!col_var, text = !!tip_var), 
                   width = width, position = position2, data = data)
      }
    }
    
    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.value = "#A8A8A8"
      )
    
    if (isMobile == FALSE){
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, title_wrap),
          subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
          y = stringr::str_wrap(x_title, x_title_wrap),
          x = stringr::str_wrap(y_title, y_title_wrap),
          caption = stringr::str_wrap(caption, caption_wrap)
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
        theme(plot.title.position = "plot") +
        theme(plot.caption.position = "plot") +
        theme(legend.justification = "left") +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          y = stringr::str_wrap(x_title, 20),
          x = stringr::str_wrap(y_title, 20),
          caption = stringr::str_wrap(caption, 50)
        ) +
        theme(axis.text.x = element_text(hjust = 0.75)) +
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
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_na_bar TRUE or FALSE of whether to make NA x_var values infinity with a light grey colour to emphasise them. Defaults to FALSE. Only applicable where facet_scales = "fixed" or "free_y". 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale. Only applicable where facet_scales equals "fixed" or "free_y".
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_nrow The number of rows of facetted plots. Defaults to NULL, which generally chooses 2 rows. 
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param width Width of bars. Defaults to 0.75.
#' @param title Title string. Defaults to [Title].
#' @param subtitle Subtitle string. Defaults to [Subtitle].
#' @param x_title X axis title string. Defaults to [X title].
#' @param y_title Y axis title string. Defaults to [Y title].
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults NULL.
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
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
           tip_var = NULL,
           x_labels = waiver(),
           x_zero = TRUE,
           x_zero_line = NULL,
           x_trans = "identity",
           x_pretty_n = 5,
           x_expand = NULL,
           x_balance = FALSE,
           x_na_bar = FALSE,
           y_rev = FALSE,
           y_labels = waiver(),
           y_expand = NULL,
           facet_scales = "fixed",
           facet_nrow = NULL,
           pal = NULL,
           width = 0.75, 
           title = "[Title]",
           subtitle = NULL,
           x_title = "[X title]",
           y_title = "[Y title]",
           caption = NULL,
           font_family = "Helvetica",
           font_size_title = NULL,
           font_size_body = NULL,
           title_wrap = 70,
           subtitle_wrap = 80,
           x_title_wrap = 50,
           y_title_wrap = 50,
           caption_wrap = 80) { 
    
    data <- dplyr::ungroup(data)
    y_var <- rlang::enquo(y_var) #categorical var
    x_var <- rlang::enquo(x_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
    x_var_vctr <- dplyr::pull(data, !!x_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a horizontal bar plot")
    if (!is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a horizontal bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal bar plot")
    
    min_x_var_vctr <- min(x_var_vctr, na.rm = TRUE)
    max_x_var_vctr <- max(x_var_vctr, na.rm = TRUE)
    
    x_above_and_below_zero <- ifelse(min_x_var_vctr < 0 & max_x_var_vctr > 0, TRUE, FALSE)
    
    if(x_above_and_below_zero == TRUE) x_zero <- FALSE
    
    if(is.null(x_zero_line)) {
      if(x_above_and_below_zero == TRUE | x_balance == TRUE) x_zero_line <- TRUE
      else(x_zero_line <- FALSE)
    }

    if(is.null(font_size_title)) font_size_title <- 11
    if(is.null(font_size_body)) font_size_body <- 10

    if (is.factor(y_var_vctr) & y_rev == FALSE){
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
    }
    else if (is.character(y_var_vctr)) {
      if (y_rev == FALSE){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
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
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- waiver()

    if (facet_scales %in% c("fixed", "free_y")) {
      if (x_balance == TRUE) {
        x_var_vctr <- abs(x_var_vctr)
        x_var_vctr <- c(-x_var_vctr, x_var_vctr)
      }
      if (x_zero == TRUE) {
        x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n)
        if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      else if (x_zero == FALSE) {
        if(x_trans != "log10") x_breaks <- pretty(x_var_vctr)
        if(x_trans == "log10") {
          x_breaks <- pretty(c(0, x_var_vctr)) 
          x_breaks <- c(1, x_breaks[x_breaks > 1])
        }
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          labels = x_labels,
          trans = x_trans,
          oob = scales::rescale_none
        )
      
      if(x_na_bar == TRUE) {
        na_data <- dplyr::filter(data, is.na(!!x_var))
        
        if(nrow(na_data) != 0) {
          if(x_limits[1] >= 0 & x_limits[2] > 0){
            plot <- plot +
              geom_col(aes(x = !!y_var, y = x_limits[2], text = !!tip_var),
                       fill = "#F5F5F5", width = width, 
                       data = na_data)
          }
          else if(x_limits[1] < 0 & x_limits[2] <= 0) {
            plot <- plot +
              geom_col(aes(x = !!y_var, y = x_limits[1], text = !!tip_var),
                       fill = "#F5F5F5", width = width, 
                       data = na_data)        
          }
          else if(x_limits[1] < 0 & x_limits[2] > 0) {
            ggplotly_adjust <- (x_limits[2] - x_limits[1]) / 1000000 # hack to fix ggplotly bug #1929
            
            plot <- plot +
              geom_col(aes(x = !!y_var, y = x_limits[2], text = !!tip_var),
                       fill = "#F5F5F5", width = width, 
                       data = na_data) +
              geom_col(aes(x = !!y_var, y = x_limits[1] + ggplotly_adjust, text = !!tip_var),
                       fill = "#F5F5F5", width = width, 
                       data = na_data)
          }
        }
      }
    }

    if (facet_scales %in% c("free", "free_x")) {
      plot <- plot +
        scale_y_continuous(expand = x_expand,
                           labels = x_labels,
                           trans = x_trans,
                           oob = scales::rescale_none)
    }
    
    if(is.null(y_labels)) y_labels <- waiver()
    
    plot <- plot +
      scale_x_discrete(expand = y_expand, labels = y_labels)

    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }
    
    if (is.null(facet_nrow) & length(unique(facet_var_vctr)) <= 3) facet_nrow <- 1 
    if (is.null(facet_nrow) & length(unique(facet_var_vctr)) > 3) facet_nrow <- 2
      
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        y = stringr::str_wrap(x_title, x_title_wrap),
        x = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow)

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
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.
#' @param x_trans A string specifying a transformation for the x scale. Defaults to "identity".
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_expand A vector of range expansion constants used to add some padding on the x scale. 
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale. Only applicable where facet_scales equals "fixed" or "free_y".
#' @param y_rev TRUE or FALSE of whether bar order from top to bottom is reversed from default. Defaults to FALSE.
#' @param y_labels Argument to adjust the format of the y scale labels.
#' @param y_expand A vector of range expansion constants used to add some padding on the y scale. 
#' @param col_rev TRUE or FALSE of whether bar fill order from left to right is reversed from default. Defaults to FALSE.
#' @param position Whether bars are positioned by "stack" or "dodge". Defaults to "stack".
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_nrow The number of rows of facetted plots. Defaults to NULL, which generally chooses 2 rows. 
#' @param pal Character vector of hex codes. Defaults to NULL, which selects a default palette.
#' @param pal_rev TRUE or FALSE of whether to reverse the pal.
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
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
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
           tip_var = NULL,
           x_labels = waiver(),
           x_zero = TRUE,
           x_zero_line = NULL,
           x_trans = "identity",
           x_pretty_n = 5,
           x_expand = NULL,
           x_balance = FALSE,
           y_rev = FALSE,
           y_labels = waiver(),
           y_expand = NULL,
           col_rev = FALSE,
           position = "stack",
           facet_scales = "fixed",
           facet_nrow = NULL,
           pal = NULL,
           pal_rev = FALSE,
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
           title_wrap = 70,
           subtitle_wrap = 80,
           x_title_wrap = 50,
           y_title_wrap = 50,
           wrap_col_title = 25,
           caption_wrap = 80) {
    
    data <- dplyr::ungroup(data)
    y_var <- rlang::enquo(y_var) #categorical var
    x_var <- rlang::enquo(x_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    tip_var <- rlang::enquo(tip_var)
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
    x_var_vctr <- dplyr::pull(data, !!x_var)
    col_var_vctr <- dplyr::pull(data, !!col_var)
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
    
    if (is.numeric(y_var_vctr)) stop("Please use a numeric y variable for a horizontal bar plot")
    if (!is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a horizontal bar plot")
    if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a horizontal bar plot")
    
    if (position == "stack" & x_trans != "identity") message("simplevis may not perform correctly using an x scale other than identity where position equals stack")
    if (position == "stack" & x_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and x_zero equal to FALSE")
    
    min_x_var_vctr <- min(x_var_vctr, na.rm = TRUE)
    max_x_var_vctr <- max(x_var_vctr, na.rm = TRUE)
    
    x_above_and_below_zero <- ifelse(min_x_var_vctr < 0 & max_x_var_vctr > 0, TRUE, FALSE)
    
    if(x_above_and_below_zero == TRUE) x_zero <- FALSE
    
    if(is.null(x_zero_line)) {
      if(x_above_and_below_zero == TRUE | x_balance == TRUE) x_zero_line <- TRUE
      else(x_zero_line <- FALSE)
    }

    if(is.null(font_size_title)) font_size_title <- 11
    if(is.null(font_size_body)) font_size_body <- 10

    if (!is.logical(col_var_vctr)){
      if (y_rev == FALSE){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
      }
      if (col_rev == FALSE){
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col_var, ~forcats::fct_rev(.x)))
      }
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

    if (!is.null(legend_labels)) labels <- rev(legend_labels)
    if (is.null(legend_labels)) labels <- waiver()
    
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(y_expand)) y_expand <- waiver()

    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      pal <- pal[1:length(levels(col_var_vctr))]
    }
    else pal <- pal[1:length(unique(col_var_vctr))]
    
    if(pal_rev == FALSE) pal <- rev(pal)
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by(dplyr::across(c(!!y_var, !!facet_var))) %>%
        dplyr::summarise(dplyr::across(!!x_var, ~sum(.x, na.rm = TRUE))) %>%
        dplyr::ungroup()
      
      x_var_vctr <- dplyr::pull(data_sum, !!x_var)
    }
    
    if (facet_scales %in% c("fixed", "free_y")) {
      if (x_balance == TRUE) {
        x_var_vctr <- abs(x_var_vctr)
        x_var_vctr <- c(-x_var_vctr, x_var_vctr)
      }
      if (x_zero == TRUE) {
        x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n)
        if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      else if (x_zero == FALSE) {
        if(x_trans != "log10") x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
        if(x_trans == "log10") {
          x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n) 
          x_breaks <- c(1, x_breaks[x_breaks > 1])
        }
        x_limits <- c(min(x_breaks), max(x_breaks))
      }
      
      plot <- plot +
        scale_y_continuous(
          expand = x_expand,
          breaks = x_breaks,
          limits = x_limits,
          trans = x_trans,
          labels = x_labels,
          oob = scales::rescale_none
        )
    }
    if (facet_scales %in% c("free", "free_x")) {
      plot <- plot +
        scale_y_continuous(expand = y_expand,
                           trans = x_trans,
                           labels = x_labels,
                           oob = scales::rescale_none)
    }
    
    if(is.null(y_labels)) y_labels <- waiver()
    
    plot <- plot +
      scale_x_discrete(expand = y_expand, labels = y_labels)

    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = FALSE,
        labels = labels,
        na.value = "#A8A8A8"
      ) 

    if(x_zero_line == TRUE) {
      plot <- plot +
        geom_hline(yintercept = 0, colour = "#323232", size = 0.3)
    }

    if (is.null(facet_nrow) & length(unique(facet_var_vctr)) <= 3) facet_nrow <- 1
    if (is.null(facet_nrow) & length(unique(facet_var_vctr)) > 3) facet_nrow <- 2
    
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        y = stringr::str_wrap(x_title, x_title_wrap),
        x = stringr::str_wrap(y_title, y_title_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow) +
      guides(fill = guide_legend(
        ncol = legend_ncol,
        byrow = TRUE,
        reverse = TRUE,
        title = stringr::str_wrap(col_title, wrap_col_title)
      ))

    return(plot)
  }
