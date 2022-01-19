#' @title Tile ggplot that is coloured.
#' @description Tile ggplot that is coloured, but not facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be plotted untransformed. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted categorical variable to colour the tiles Required input.
#' @param label_var Unquoted variable to label the tiles. 
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param pal_label Hex code for the label font colour. Defaults to "#323232".
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_fill The opacity of the fill. Defaults to 1.  
#' @param alpha_line The opacity of the outline. Defaults to 1.
#' @param size_line The size of the outlines of bars. 
#' @param size_label The size of the of labels. Defaults to 3.5.
#' @param size_height Height of tiles. Defaults to 1.
#' @param size_width Width of tiles. Defaults to 1.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 60. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 60. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use function(x) x to keep labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_rev TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use function(x) x to keep labels untransformed.
#' @param y_na_rm TRUE or FALSE of whether to include y_var NA values. Defaults to FALSE.
#' @param y_rev TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles.
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case for categorical colour variables and scales::label_comma() for numeric. Use function(x) x to keep labels untransformed.  
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_method The method of colouring features, either "bin", "quantile", "continuous", or "category." If numeric, defaults to "bin".
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_breaks_n For a numeric colour variable, the desired number of intervals on the colour scale. 
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 75. 
#' @param theme A ggplot2 theme.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(dplyr)
#' library(palmerpenguins)
#' 
#' plot_data <- penguins %>% 
#'   group_by(species, sex) %>% 
#'   summarise(bill_length_mm = round(mean(bill_length_mm, na.rm = TRUE), 1)) 
#'   
#' gg_tile_col(plot_data, 
#'   x_var = sex, 
#'   y_var = species, 
#'   col_var = bill_length_mm, 
#'   label_var = bill_length_mm) 
#'             
gg_tile_col <- function(data,
                        x_var,
                        y_var,
                        col_var,
                        label_var = NULL,
                        text_var = NULL,
                        pal = NULL,
                        pal_label = "#323232",
                        pal_na = "#7F7F7F",
                        pal_rev = FALSE,
                        alpha_fill = 1,
                        alpha_line = 1,
                        size_line = 0.5,
                        size_label = 3.5,
                        size_height = 1, 
                        size_width = 1,
                        title = NULL,
                        title_wrap = 75,
                        subtitle = NULL,
                        subtitle_wrap = 75,
                        x_expand = c(0, 0),
                        x_labels = snakecase::to_sentence_case,
                        x_na_rm = FALSE,
                        x_rev = FALSE,
                        x_title = NULL,
                        x_title_wrap = 50,
                        y_expand = c(0, 0),
                        y_labels = snakecase::to_sentence_case,
                        y_na_rm = FALSE,
                        y_rev = FALSE,
                        y_title = NULL,
                        y_title_wrap = 50,
                        col_cuts = NULL,
                        col_intervals_right = TRUE,
                        col_labels = NULL,
                        col_legend_none = FALSE,
                        col_method = NULL,
                        col_na_rm = FALSE,
                        col_breaks_n = 4,
                        col_title = NULL,
                        col_title_wrap = 25,
                        caption = NULL,
                        caption_wrap = 75,
                        theme = gg_theme(),
                        mobile = FALSE) {
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) #categorical var
  y_var <- rlang::enquo(y_var) #categorical var
  col_var <- rlang::enquo(col_var) 
  label_var <- rlang::enquo(label_var)
  text_var <- rlang::enquo(text_var)
  
  #na's
  if (x_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  
  #vectors
  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  
  #warnings
  if (is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a tile plot")
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a tile plot")
  
  if (!is.null(col_method)) {
    if (!col_method %in% c("continuous", "bin", "quantile", "category")) stop("Please use a colour method of 'continuous', 'bin', 'quantile' or 'category'")
  }
  
  #logical to factor
  if (is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if (is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #reverse
  if (x_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if (y_rev == FALSE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  
  #colour
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "continuous"
  }
  
  if (col_method == "continuous") {
    if (is.null(pal)) pal <- viridis::viridis(20)
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
    if (is.null(col_labels)) col_labels <- scales::label_comma()
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    if (col_method %in% c("quantile", "bin")) {
      if (col_method == "quantile") {
        if (is.null(col_cuts)) col_cuts <- seq(0, 1, 1 / col_breaks_n)
        else {
          if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
          if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
        }  
        col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
        if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      }
      else if (col_method == "bin") {
        if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
        else {
          if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
          if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
        }
      }
      
      if (is.null(col_labels)) col_labels <- scales::label_comma()
      
      if (is.function(col_labels)) {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE, format_fun = col_labels)))
        
        col_labels <- sv_interval_labels_chr
      }
      else {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE)))
      }
      
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
      
      if (is.null(col_labels)) col_labels <- snakecase::to_sentence_case
    }
  }  
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)
  pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    geom_tile(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
              size = size_line, width = size_width, height = size_height) 
  
  if(!rlang::quo_is_null(label_var)) {
    plot <- plot + 
      geom_text(aes(x = !!x_var, y = !!y_var, label = !!label_var), size = size_label, col = pal_label)
  }
  
  #x and y scales
  plot <- plot +
    scale_x_discrete(expand = x_expand, labels = x_labels) +
    scale_y_discrete(expand = y_expand, labels = y_labels)
  
  #colour
  if (mobile == TRUE) col_title_wrap <- 20
  
  if (col_method == "continuous") {
    plot <- plot +
      scale_colour_gradientn(
        colors = pal_line,
        labels = col_labels,
        breaks = col_cuts,
        na.value = pal_na_line,
        name = stringr::str_wrap(col_title, col_title_wrap)) +
      scale_fill_gradientn(
        colors = pal_fill,
        labels = col_labels,
        breaks = col_cuts,
        na.value = pal_na_fill,
        name = stringr::str_wrap(col_title, col_title_wrap)) +
      guides(fill = "none")
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    plot <- plot +
      scale_colour_manual(
        values = pal_line,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na_line,
        name = stringr::str_wrap(col_title, col_title_wrap)
      ) +
      scale_fill_manual(
        values = pal_fill,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na_fill,
        name = stringr::str_wrap(col_title, col_title_wrap)
      )
    
    if (mobile == TRUE) {
      plot <- plot +
        guides(col = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))
    }
  }
  
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")

  #titles
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
      theme_mobile_extra()
  }
  
  return(plot)
}

#' @title Tile ggplot that is coloured and facetted.
#' @description Tile ggplot that is coloured and facetted.
#' @param data An ungrouped summarised tibble or dataframe in a structure to be plotted untransformed. Required input.
#' @param x_var Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.
#' @param y_var Unquoted numeric variable to be on the y scale. Required input.
#' @param col_var Unquoted categorical variable to colour the tiles. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param label_var Unquoted variable to label the tiles. 
#' @param text_var Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.
#' @param pal Character vector of hex codes. 
#' @param pal_label Hex code for the label font colour. Defaults to "#323232".
#' @param pal_na The hex code or name of the NA colour to be used.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param alpha_fill The opacity of the fill. Defaults to 1.  
#' @param alpha_line The opacity of the outline. Defaults to 1. 
#' @param size_line The size of the outlines of bars.
#' @param size_label The size of the of labels. Defaults to 3.5.
#' @param size_height Height of tiles. Defaults to 1.
#' @param size_width Width of tiles. Defaults to 1.
#' @param title Title string. 
#' @param title_wrap Number of characters to wrap the title to. Defaults to 60. 
#' @param subtitle Subtitle string. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 60. 
#' @param x_expand A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param x_labels A function or named vector to modify x scale labels. If NULL, categorical variable labels are converted to sentence case. Use function(x) x to keep labels untransformed.
#' @param x_na_rm TRUE or FALSE of whether to include x_var NA values. Defaults to FALSE.
#' @param x_rev TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.
#' @param x_title X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param x_title_wrap Number of characters to wrap the x title to. Defaults to 50. 
#' @param y_expand A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions. 
#' @param y_labels A function or named vector to modify y scale labels. If NULL, categorical variable labels are converted to sentence case. Use function(x) x to keep labels untransformed.
#' @param y_na_rm TRUE or FALSE of whether to include y_var NA values. Defaults to FALSE.
#' @param y_rev TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.
#' @param y_title y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param y_title_wrap Number of characters to wrap the y title to. Defaults to 50. 
#' @param col_breaks_n For a numeric colour variable, the desired number of intervals on the colour scale. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles.
#' @param col_intervals_right For a numeric colour variable, TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @param col_labels A function or named vector to modify colour scale labels. Defaults to snakecase::to_sentence_case for categorical colour variables and scales::label_comma() for numeric. Use function(x) x to keep labels untransformed.  
#' @param col_legend_none TRUE or FALSE of whether to remove the legend.
#' @param col_method The method of colouring features, either "bin", "quantile", "continuous", or "category." If numeric, defaults to "bin".
#' @param col_na_rm TRUE or FALSE of whether to include col_var NA values. Defaults to FALSE.
#' @param col_title Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.
#' @param facet_labels A function or named vector to modify facet scale labels. Defaults to converting labels to sentence case. Use function(x) x to keep labels untransformed.
#' @param facet_na_rm TRUE or FALSE of whether to include facet_var NA values. Defaults to FALSE.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param facet_nrow The number of rows of facetted plots. 
#' @param facet_rev TRUE or FALSE of whether the facet variable variable is reversed. Defaults to FALSE.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 75. 
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' @export
#' @examples
#' library(simplevis)
#' library(dplyr)
#' library(palmerpenguins)
#' 
#' plot_data <- penguins %>% 
#'     group_by(species, sex, island) %>% 
#'     summarise(bill_length_mm = round(mean(bill_length_mm, na.rm = TRUE), 1)) 
#'     
#' gg_tile_col_facet(plot_data,
#'     x_var = sex,
#'     y_var = island,
#'     col_var = bill_length_mm,
#'     facet_var = species,
#'     label_var = bill_length_mm)
#'     
gg_tile_col_facet <- function(data,
                              x_var,
                              y_var,
                              col_var,
                              facet_var,
                              label_var = NULL,
                              text_var = NULL,
                              pal = NULL,
                              pal_label = "#323232",
                              pal_na = "#7F7F7F",
                              pal_rev = FALSE,
                              alpha_fill = 1,
                              alpha_line = 1,
                              size_line = 0.5,
                              size_label = 3.5,
                              size_height = 1,
                              size_width = 1,
                              title = NULL,
                              title_wrap = 75,
                              subtitle = NULL,
                              subtitle_wrap = 75,
                              x_expand = c(0, 0),
                              x_labels = snakecase::to_sentence_case,
                              x_na_rm = FALSE,
                              x_rev = FALSE,
                              x_title = NULL,
                              x_title_wrap = 50,
                              y_expand = c(0, 0),
                              y_labels = snakecase::to_sentence_case,
                              y_na_rm = FALSE,
                              y_rev = FALSE,
                              y_title = NULL,
                              y_title_wrap = 50,
                              col_breaks_n = 4,
                              col_cuts = NULL,
                              col_intervals_right = TRUE,
                              col_labels = NULL,
                              col_legend_none = FALSE,
                              col_method = NULL,
                              col_na_rm = FALSE,
                              col_title = NULL,
                              col_title_wrap = 25,
                              facet_labels = snakecase::to_sentence_case,
                              facet_na_rm = FALSE,
                              facet_ncol = NULL,
                              facet_nrow = NULL,
                              facet_rev = FALSE,
                              facet_scales = "fixed",
                              caption = NULL,
                              caption_wrap = 75,
                              theme = gg_theme())
{
  
  #ungroup
  data <- dplyr::ungroup(data)
  
  #quote
  x_var <- rlang::enquo(x_var) #categorical var
  y_var <- rlang::enquo(y_var) #categorical var
  col_var <- rlang::enquo(col_var) 
  facet_var <- rlang::enquo(facet_var) 
  label_var <- rlang::enquo(label_var)
  text_var <- rlang::enquo(text_var)
  
  #na's
  if (x_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!x_var))
  }
  if (y_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!y_var))
  }
  if (col_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!col_var))
  }
  if (facet_na_rm == TRUE) {
    data <- data %>% 
      dplyr::filter(!is.na(!!facet_var))
  }
  
  #vectors
  y_var_vctr <- dplyr::pull(data, !!y_var)
  x_var_vctr <- dplyr::pull(data, !!x_var)
  col_var_vctr <- dplyr::pull(data, !!col_var)
  facet_var_vctr <- dplyr::pull(data, !!facet_var)
  
  #warnings
  if (is.numeric(x_var_vctr)) stop("Please use a categorical x variable for a tile plot")
  if (is.numeric(y_var_vctr)) stop("Please use a categorical y variable for a tile plot")
  if (is.numeric(facet_var_vctr)) stop("Please use a categorical facet variable for a tile plot")
  
  if (!is.null(col_method)) {
    if (!col_method %in% c("continuous", "bin", "quantile", "category")) stop("Please use a colour method of 'continuous', 'bin', 'quantile' or 'category'")
  }
  
  #logical to factor
  if (is.logical(x_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!x_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if (is.logical(y_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!y_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  if (is.logical(col_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!col_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    col_var_vctr <- dplyr::pull(data, !!col_var)
  }
  if (is.logical(facet_var_vctr)) {
    data <- data %>% 
      dplyr::mutate(dplyr::across(!!facet_var, ~factor(.x, levels = c("TRUE", "FALSE"))))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #titles sentence case
  if (is.null(x_title)) x_title <- snakecase::to_sentence_case(rlang::as_name(x_var))
  if (is.null(y_title)) y_title <- snakecase::to_sentence_case(rlang::as_name(y_var))
  if (is.null(col_title)) col_title <- snakecase::to_sentence_case(rlang::as_name(col_var))
  
  #reverse
  if (x_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!x_var, ~forcats::fct_rev(.x)))
    
    x_var_vctr <- dplyr::pull(data, !!x_var)
  }
  if (y_rev == FALSE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!y_var, ~forcats::fct_rev(.x)))
    
    y_var_vctr <- dplyr::pull(data, !!y_var)
  }
  
  if (facet_rev == TRUE) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!facet_var, ~forcats::fct_rev(.x)))
    
    facet_var_vctr <- dplyr::pull(data, !!facet_var)
  }
  
  #colour
  if (is.null(col_method)) {
    if (!is.numeric(col_var_vctr)) col_method <- "category"
    else if (is.numeric(col_var_vctr)) col_method <- "continuous"
  }
  
  if (col_method == "continuous") {
    if (is.null(pal)) pal <- viridis::viridis(20)
    if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
    if (is.null(col_labels)) col_labels <- scales::label_comma()
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    if (col_method %in% c("quantile", "bin")) {
      if (col_method == "quantile") {
        if (is.null(col_cuts)) col_cuts <- seq(0, 1, 1 / col_breaks_n)
        else {
          if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
          if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
        }  
        col_cuts <- stats::quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
        if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
      }
      else if (col_method == "bin") {
        if (is.null(col_cuts)) col_cuts <- pretty(col_var_vctr, col_breaks_n)
        else {
          if (!(dplyr::first(col_cuts) %in% c(0, -Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
          if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
        }
      }
      
      if (is.null(col_labels)) col_labels <- scales::label_comma()
      
      if (is.function(col_labels)) {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE, format_fun = col_labels)))
        
        col_labels <- sv_interval_labels_chr
      }
      else {
        data <- data %>%
          dplyr::mutate(
            dplyr::across(!!col_var, 
                          ~ cut_format(.x, col_cuts,
                                       right = col_intervals_right, include.lowest = TRUE, dig.lab = 50, ordered_result = TRUE)))
      }
      
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
      
      if (is.null(col_labels)) col_labels <- snakecase::to_sentence_case
    }
  }  
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  pal_fill <- scales::alpha(pal, alpha = alpha_fill)
  pal_na_fill <- scales::alpha(pal_na, alpha = alpha_fill)
  pal_line <- scales::alpha(pal, alpha = alpha_line)
  pal_na_line <- scales::alpha(pal_na, alpha = alpha_line)
  
  #fundamentals
  plot <- ggplot(data) +
    theme +
    geom_tile(aes(x = !!x_var, y = !!y_var, col = !!col_var, fill = !!col_var, text = !!text_var), 
              size = size_line, width = size_width, height = size_height) 
  
  if(!rlang::quo_is_null(label_var)) {
    plot <- plot + 
      geom_text(aes(x = !!x_var, y = !!y_var, label = !!label_var), size = size_label, col = pal_label)
  }
  
  #colour
  if (col_method == "continuous") {
    plot <- plot +
      scale_colour_gradientn(
        colors = pal_line,
        labels = col_labels,
        breaks = col_cuts,
        na.value = pal_na_line,
        name = stringr::str_wrap(col_title, col_title_wrap))  +
      scale_fill_gradientn(
        colors = pal_fill,
        labels = col_labels,
        breaks = col_cuts,
        na.value = pal_na_fill,
        name = stringr::str_wrap(col_title, col_title_wrap)) +
      guides(fill = "none")
  }
  else if (col_method %in% c("quantile", "bin", "category")) {
    plot <- plot +
      scale_colour_manual(
        values = pal_line,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na_line,
        name = stringr::str_wrap(col_title, col_title_wrap)
      ) +
      scale_fill_manual(
        values = pal_fill,
        drop = FALSE,
        labels = col_labels,
        na.value = pal_na_fill,
        name = stringr::str_wrap(col_title, col_title_wrap)
      )
  }
  
  if (col_legend_none == TRUE) plot <- plot +
    theme(legend.position = "none")

  #x & y scales, titles, and facetting
  plot <- plot +
    scale_x_discrete(expand = x_expand, labels = x_labels) +
    scale_y_discrete(expand = y_expand, labels = y_labels) +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      x = stringr::str_wrap(x_title, x_title_wrap),
      y = stringr::str_wrap(y_title, y_title_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(vars(!!facet_var), 
               labeller = as_labeller(facet_labels), 
               scales = facet_scales, 
               ncol = facet_ncol, 
               nrow = facet_nrow)
  
  return(plot)
}