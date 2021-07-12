#' @title Tile ggplot that is coloured.
#' @description Tile ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param label_var Unquoted variable to label the tiles. 
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param pal_label Hex code colour for labels. Defaults to "#FFFFFF".
#' @param width Width of bars. Defaults to 1.
#' @param alpha The alpha of the fill. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param title Title string. Defaults to NULL.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 100. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 100. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.
#' @param x_na TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.
#' @param x_rev TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.
#' @param y_na TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.
#' @param y_rev TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles.
#' @param col_labels A function or vector to modify colour scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case, and numeric variable labels to pretty labels with an internal function. Use ggplot2::waiver() to keep colour labels untransformed.   
#' @param col_labels_dp For numeric colour variables and where col_labels equals NULL, the number of decimal places. Defaults to 1 for "quantile" col_method, and the lowest dp within the col_cuts vector for "bin".
#' @param col_legend_ncol The number of columns in the legend. 
#' @param col_legend_nrow The number of rows in the legend.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "quantile".
#' @param col_na TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(dplyr)
#' library(palmerpenguins)
#' 
#' plot_data <- penguins %>% 
#'   group_by(species, sex) %>% 
#'     summarise(body_mass_g = round(mean(body_mass_g, na.rm = TRUE)), 0) %>% 
#'     mutate(label = glue::glue("{prettyNum(body_mass_g, big.mark = ',')} g"))
#' 
#' gg_tile_col(plot_data, 
#'             x_var = sex, 
#'             y_var = species, 
#'             col_var = body_mass_g, 
#'             label_var = label) 
#'             
gg_tile_col <- function(data,
                       x_var,
                       y_var,
                       col_var,
                       label_var = NULL,
                       text_var = NULL,
                       pal = NULL,
                       pal_rev = FALSE,
                       pal_label = "#FFFFFF",
                       width = 1,
                       alpha = 1,
                       size_line = 0.5,
                       title = NULL,
                       title_wrap = 100,
                       subtitle = NULL,
                       subtitle_wrap = 100,
                       x_expand = NULL,
                       x_labels = NULL,
                       x_na = TRUE,
                       x_rev = FALSE,
                       x_title = NULL,
                       x_title_wrap = 50,
                       y_expand = NULL,
                       y_labels = waiver(),
                       y_na = TRUE,
                       y_rev = FALSE,
                       y_title = NULL,
                       y_title_wrap = 50,
                       col_cuts = NULL,
                       col_labels = NULL,
                       col_labels_dp = NULL,
                       col_legend_ncol = NULL,
                       col_legend_nrow = NULL,
                       col_method = NULL,
                       col_na = TRUE,
                       col_title = NULL,
                       col_title_wrap = 25,
                       caption = NULL,
                       caption_wrap = 80,
                       font_family = "",
                       font_size_title = NULL,
                       font_size_body = NULL,
                       mobile = FALSE)
{
  
  data <- dplyr::ungroup(data)
  y_var <- rlang::enquo(y_var) #numeric var
  x_var <- rlang::enquo(x_var) 
  col_var <- rlang::enquo(col_var) #categorical var
  label_var <- rlang::enquo(label_var)
  text_var <- rlang::enquo(text_var)
  
  if (x_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (col_na == FALSE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  if (is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a vertical bar plot")
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a vertical bar plot")
  # if (is.numeric(col_var_vctr)) stop("Please use a categorical colour variable for a vertical bar plot")
  
  if(is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if(is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if(is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(., levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  if (x_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if (y_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }

  if(is.null(font_size_title)) font_size_title <- sv_font_size_title(mobile = mobile)
  if(is.null(font_size_body)) font_size_body <- sv_font_size_body(mobile = mobile)
  
  if (lubridate::is.Date(x_var_vctr)) bar_unit <- 365
  else bar_unit <- 1
  
  bar_width <- bar_unit * width
  
  if(!rlang::quo_is_null(label_var)) {
    data <- data %>% 
      dplyr::mutate(label_var2 = !!label_var) 
  }
  
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
      col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
      if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      if(is.null(col_labels_dp)) col_labels_dp <- 1
    }
    else if (col_method == "bin") {
      if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr)
      else({
        if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
        if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      })
      if(is.null(col_labels_dp)) col_labels_dp <- sv_max_dp(col_cuts)
    }
    
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~cut(.x, col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if(is.null(col_labels)) col_labels <- sv_numeric_bin_labels(col_cuts, col_labels_dp)
    
    col_n <- length(col_cuts) - 1
    if (is.null(pal)) pal <- pal_viridis_reorder(col_n)
    else pal <- pal[1:col_n]
  }
  else if (col_method == "category") {
    if (is.factor(col_var_vctr) & !is.null(levels(col_var_vctr))) {
      col_n <- length(levels(col_var_vctr))
    }
    else col_n <- length(unique(col_var_vctr))
    
    if (is.null(pal)) pal <- pal_d3_reorder(col_n)
    else pal <- pal[1:col_n]
    
    if(is.null(col_labels)) col_labels <- function(x) stringr::str_to_sentence(x)
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)

  plot <- ggplot(data) +
    theme_no_gridlines(font_family = font_family, font_size_body = font_size_body, font_size_title = font_size_title) +
    geom_tile(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
             alpha = alpha, 
             size = size_line, 
             width = bar_width) +
    theme(axis.line = element_blank()) +
    theme(axis.ticks = element_blank())
    
  if(!rlang::quo_is_null(label_var)) {
    plot <- plot + 
      geom_text(aes(x = !!x_var, y = !!y_var, label = .data$label_var2), col = pal_label)
  }

  if (is.character(x_var_vctr) | is.factor(x_var_vctr)){
    if(is.null(x_expand)) x_expand <- c(0, 0)
    if(is.null(x_labels)) x_labels <- function(x) stringr::str_to_sentence(x)
    
    plot <- plot +
      scale_x_discrete(expand = x_expand, labels = x_labels)
  } 
  
  if (is.character(y_var_vctr) | is.factor(y_var_vctr)){
    if(is.null(y_expand)) y_expand <- c(0, 0)
    if(is.null(y_labels)) y_labels <- function(x) stringr::str_to_sentence(x)
    
    plot <- plot +
      scale_y_discrete(expand = y_expand, labels = y_labels)
  } 
  
  plot <- plot +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na()
    ) +
    scale_colour_manual(
      values = pal,
      drop = FALSE,
      labels = col_labels,
      na.value = pal_na()
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
        ncol = col_legend_ncol,
        byrow = TRUE,
        title = stringr::str_wrap(col_title, col_title_wrap)
      ), 
      col = guide_legend(
        ncol = col_legend_ncol, nrow = col_legend_nrow, 
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
        fill = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 20)),
        col = guide_legend(ncol = 1, title = stringr::str_wrap(col_title, 20))
      ) +
      theme_mobile_extra()
  }
  
  return(plot)
}