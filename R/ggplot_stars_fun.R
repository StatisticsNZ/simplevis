# ggplot stars functions

#' @title Theme for ggplot maps of arrays.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#' 
#' ggplot() +
#'   theme_stars("Courier", 9, 7) +
#'   ggtitle("This is a title of a selected font family and size")
theme_stars <-
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
          size = font_size_body
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
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
        legend.key = element_rect(fill = "white"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm")
      )
    )
  }

#' @title Map of an array in ggplot.
#' @description Map of an array in ggplot that is not coloured and not facetted. 
#' @param data A stars object with 2 dimensions x and y. Required input.
#' @param pal Character vector of hex codes, or provided objects with pal_ prefixes.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile. 
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_stars(data = example_stars)
ggplot_stars <- function(data,
                         pal = NULL,
                         title = "[Title]",
                         subtitle = NULL,
                         caption = NULL,
                         font_family = "Helvetica",
                         font_size_title = NULL,
                         font_size_body = NULL,
                         title_wrap = 70,
                         subtitle_wrap = 80,
                         caption_wrap = 80,
                         isMobile = FALSE) {
  
  if (class(data)[1] != "stars") stop("Please use an stars object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }
  
  plot <- ggplot() +
    theme_stars(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    coord_equal() #this is applicable only when no boundary

  data <- data %>%
    tibble::as_tibble()
  
  if (is.null(pal)) pal <- pal_snz[1]
  
  plot <- plot +
    geom_raster(aes(x = .data$x, y = .data$y),
                fill = pal,
                alpha = 0.1,
                data = data) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  if (isMobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
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
        caption = stringr::str_wrap(caption, 50)
      )
  }
  
  return(plot)
}

#' @title Map of an array in ggplot that is coloured.
#' @description Map of an array in ggplot that is coloured, but not facetted. 
#' @param data A stars object with 2 dimensions x and y, and 1 attribute layer that will be coloured. Required input.
#' @param col_method The method of colouring grid, either "bin", "quantile" or "category." Defaults to "quantile".
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param pal Character vector of hex codes, or provided objects with pal_ prefixes. Defaults to viridis.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param legend_ncol The number of columns in the legend.
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param caption Caption title string. Defaults to NULL.
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. Not applicable where isMobile equals TRUE.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile. 
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_stars_col(data = example_stars, 
#'    col_method = "quantile", col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
#'    title = "Site medians, 2013-17")
ggplot_stars_col <- function(data,
                             col_method = "quantile",
                             col_cuts = NULL,
                             pal = NULL,
                             pal_rev = FALSE,
                             legend_ncol = 3,
                             legend_digits = 1,
                             title = "[Title]",
                             subtitle = NULL,
                             col_title = "",
                             caption = NULL,
                             legend_labels = NULL,
                             font_family = "Helvetica",
                             font_size_title = NULL,
                             font_size_body = NULL,
                             title_wrap = 70,
                             subtitle_wrap = 80,
                             wrap_col_title = 25,
                             caption_wrap = 80,
                             isMobile = FALSE) {
  
  if (class(data)[1] != "stars") stop("Please use an stars object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)){
    if (isMobile == FALSE) font_size_title <- 11
    else if (isMobile == TRUE) font_size_title <- 15
  }
  if(is.null(font_size_body)){
    if (isMobile == FALSE) font_size_body <- 10
    else if (isMobile == TRUE) font_size_body <- 14
  }
  
  plot <- ggplot() +
    theme_stars(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  data <- data %>% 
    dplyr::select(col_var = 1)
  
  col_var_vctr <- dplyr::pull(data, .data$col_var)
  
  if (col_method == "category") {
    data <- data %>%
      tibble::as_tibble()
    
    col_cuts <- unique(col_var_vctr)
    max_bin_cut <- max(col_cuts, na.rm = TRUE)
    min_bin_cut <- min(col_cuts, na.rm = TRUE)
    col_cuts <- seq(min_bin_cut, max_bin_cut + 1, 1)
    no_bins <- length(col_cuts)
    
    data <- data %>%
      dplyr::mutate(dplyr::across(c(-.data$x, -.data$y), ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if (is.null(pal)) pal <- pal_point_set1[1:(length(col_cuts) - 1)]
    if (!is.null(pal)) pal <- pal[1:(length(col_cuts) - 1)]
    if (is.null(legend_labels)) labels <- LETTERS[1:length(col_cuts) - 1]
    if (!is.null(legend_labels)) labels <- legend_labels
  }
  else if (col_method == "bin") {
    if (is.null(col_cuts)) {
      data <- data %>%
        tibble::as_tibble()
      
      col_cuts <- pretty(col_var_vctr)
      
      data <- data %>%
        dplyr::mutate(dplyr::across(c(-.data$x, -.data$y), ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
      if (!is.null(legend_labels)) labels <- legend_labels
      
    }
    else if (!is.null(col_cuts)) {
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      
      data <- data %>%
        tibble::as_tibble() %>%
        dplyr::mutate(dplyr::across(c(-.data$x, -.data$y), ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
      if (!is.null(legend_labels)) labels <- legend_labels
    }
  }
  else if (col_method == "quantile") {
    if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
    else {
      if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
      if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
    }  
    
    data <- data %>% 
      tibble::as_tibble()
    
    col_cuts <- quantile(col_var_vctr, probs = col_cuts, na.rm = TRUE)
    if (anyDuplicated(col_cuts) > 0) stop("col_cuts do not provide unique breaks")
    
    data <- data %>%
      dplyr::mutate(dplyr::across(c(-.data$x, -.data$y), ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
    if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
    if (!is.null(legend_labels)) labels <- legend_labels
  }
  
  if (pal_rev == TRUE) pal <- rev(pal)
  
  plot <- plot +
    geom_raster(aes(x = .data$x, y = .data$y, fill = .data$col_var), data = data) +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = labels,
      na.value = "transparent",
      na.translate = FALSE
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_equal() #this is applicable only when no boundary
  
  if (isMobile == FALSE) {
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, title_wrap),
        subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
        caption = stringr::str_wrap(caption, caption_wrap)
      ) +
      guides(fill = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title)))
  }
  else if (isMobile == TRUE) {
    plot <- plot +
      theme(plot.title.position = "plot") +
      theme(plot.caption.position = "plot") +
      theme(legend.justification = "left") +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        caption = stringr::str_wrap(caption, 50)
      )  +
      guides(fill = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15)))
  }
  
  return(plot)
}

#' @title Map of an array in ggplot that is facetted.
#' @description Map of an array in ggplot that is facetted, but not coloured. 
#' @param data A stars object with 2 dimensions, x and y, and multiple named attribute layers with usual convention of lower case and underscores. These attribute layers will be facetted. Required input.
#' @param pal Character vector of hex codes, or provided objects with pal_ prefixes.
#' @param facet_nrow The number of rows of facetted plots. 
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param caption Caption title string. Defaults to NULL.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @return A ggplot object.
#' @export
#' @examples
#' map_data1 <- example_stars %>%
#'   rlang::set_names("Variable A")
#'
#' map_data2 <- example_stars_2 %>%
#'   rlang::set_names("Variable B")
#'
#' map_data <- c(map_data1, map_data2)
#'
#' ggplot_stars_facet(data = map_data)
ggplot_stars_facet <- function(data,
                               pal = NULL,
                               facet_nrow = NULL,
                               title = "[Title]",
                               subtitle = NULL,
                               caption = NULL,
                               font_family = "Helvetica",
                               font_size_title = NULL,
                               font_size_body = NULL,
                               title_wrap = 70,
                               subtitle_wrap = 80,
                               caption_wrap = 80) {
  
  if (class(data)[1] != "stars") stop("Please use an stars object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)) font_size_title <- 11
  if(is.null(font_size_body)) font_size_body <- 10
  
  plot <- ggplot() +
    theme_stars(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    coord_equal() #this is applicable only when no boundary

  data <- data %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(c(-.data$x,-.data$y), names_to = "facet_var", values_to = "col_var")
  
  if (is.null(pal)) pal <- pal_snz[1]
  
  plot <- plot +
    geom_raster(aes(x = .data$x, y = .data$y),
                fill = pal,
                alpha = 0.1,
                data = data) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  if (is.null(facet_nrow) & length(unique(data$facet_var)) <= 3) facet_nrow <- 1
  if (is.null(facet_nrow) & length(unique(data$facet_var)) > 3) facet_nrow <- 2
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(
      ~ .data$facet_var,
      scales = "fixed",
      nrow = facet_nrow,
      labeller = labeller(
        facet_var = function(x)
          stringr::str_to_sentence(stringr::str_replace_all(x, "\\.", " "))
      )
    )

  return(plot)
}

#' @title Map of an array in ggplot that is coloured and facetted.
#' @description Map of an array in ggplot that is coloured and facetted. 
#' @param data A stars object with 2 dimensions, x and y, and multiple named attribute layers with usual convention of lower case and underscores. Each attribute layer will be a facet. Required input.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." Defaults to "quantile". Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_quantile_by_facet TRUE of FALSE whether quantiles should be calculated for each group of the facet variable. Defaults to TRUE.
#' @param pal Character vector of hex codes, or provided objects with pal_ prefixes. Defaults to viridis.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param facet_nrow The number of rows of facetted plots. 
#' @param legend_ncol The number of columns in the legend.
#' @param legend_digits Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param title Title string. Defaults to "[Title]".
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param caption Caption title string. Defaults to NULL.
#' @param legend_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param wrap_col_title Number of characters to wrap the colour title to. Defaults to 25. 
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @return A ggplot object.
#' @export
#' @examples
#' map_data1 <- example_stars %>%
#'   rlang::set_names("Variable A")
#'
#' map_data2 <- example_stars_2 %>%
#'   rlang::set_names("Variable B")
#'
#' map_data <- c(map_data1, map_data2)
#'
#' ggplot_stars_col_facet(data = map_data, 
#'    col_method = "quantile", col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
#'    title = "Site medians, 2013-17")
ggplot_stars_col_facet <- function(data,
                                   col_method = "quantile",
                                   col_quantile_by_facet = TRUE,
                                   col_cuts = NULL,
                                   pal = NULL,
                                   pal_rev = FALSE,
                                   facet_nrow = NULL,
                                   legend_ncol = 3,
                                   legend_digits = 1,
                                   title = "[Title]",
                                   subtitle = NULL,
                                   col_title = "",
                                   caption = NULL,
                                   legend_labels = NULL,
                                   font_family = "Helvetica",
                                   font_size_title = NULL,
                                   font_size_body = NULL,
                                   title_wrap = 70,
                                   subtitle_wrap = 80,
                                   wrap_col_title = 25,
                                   caption_wrap = 80) {
  
  if (class(data)[1] != "stars") stop("Please use an stars object as data input")
  if (is.na(sf::st_crs(data))) stop("Please assign a coordinate reference system")
  
  if(is.null(font_size_title)) font_size_title <- 11
  if(is.null(font_size_body)) font_size_body <- 10
  
  plot <- ggplot() +
    theme_stars(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    ) +
    coord_equal() #this is applicable only when no boundary

  if (col_method == "category") {
    data <- data %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(c(-.data$x,-.data$y), names_to = "facet_var", values_to = "col_var")
    
    col_cuts <- unique(dplyr::pull(data, .data$col_var))
    max_bin_cut <- max(col_cuts, na.rm = TRUE)
    min_bin_cut <- min(col_cuts, na.rm = TRUE)
    col_cuts <- seq(min_bin_cut, max_bin_cut + 1, 1)
    no_bins <- length(col_cuts)
    
    data <- data %>%
      dplyr::mutate(dplyr::across(.data$col_var, ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
    
    if (is.null(pal)) pal <- pal_point_set1[1:(length(col_cuts) - 1)]
    if (!is.null(pal)) pal <- pal[1:(length(col_cuts) - 1)]
    if (is.null(legend_labels)) labels <- LETTERS[1:length(col_cuts) - 1]
    if (!is.null(legend_labels)) labels <- legend_labels
  }
  else if (col_method == "bin") {
    if (is.null(col_cuts)) {
      data <- data %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = c(-.data$x, -.data$y), names_to = "facet_var", values_to = "col_var")
      
      col_cuts <- pretty(dplyr::pull(data, .data$col_var))
      
      data <- data %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
      if (!is.null(legend_labels)) labels <- legend_labels
      
    }
    else if (!is.null(col_cuts)) {
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      
      data <- data %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = c(-.data$x, -.data$y), names_to = "facet_var", values_to = "col_var") %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(legend_labels)) labels <- numeric_legend_labels(col_cuts, legend_digits)
      if (!is.null(legend_labels)) labels <- legend_labels
    }
  }
  else if (col_method == "quantile") {
    if(is.null(col_cuts)) col_cuts <- seq(0, 1, 0.25)
    else {
      if (dplyr::first(col_cuts) != 0) warning("The first element of the col_cuts vector generally always be 0")
      if (dplyr::last(col_cuts) != 1) warning("The last element of the col_cuts vector should generally be 1")
    }  
    if (col_quantile_by_facet == TRUE) {
      data <- data %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = c(-.data$x, -.data$y), names_to = "facet_var", values_to = "col_var") %>%
        dplyr::group_by(.data$facet_var) %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ percent_rank(.))) %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(legend_labels)) labels <- paste0(numeric_legend_labels(col_cuts * 100, 0), "\u1D57\u02B0 percentile")
      if (!is.null(legend_labels)) labels <- legend_labels
    }
    else if (col_quantile_by_facet == FALSE) {
      data <- data %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = c(-.data$x, -.data$y), names_to = "facet_var", values_to = "col_var") %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ percent_rank(.))) %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(legend_labels)) labels <- paste0(numeric_legend_labels(col_cuts * 100, 0), "\u1D57\u02B0 percentile")
      if (!is.null(legend_labels)) labels <- legend_labels
    }
  }
  
  if (pal_rev == TRUE)
    pal <- rev(pal)
  
  plot <- plot +
    geom_raster(aes(x = .data$x, y = .data$y, fill = .data$col_var), data = data) +
    scale_fill_manual(
      values = pal,
      drop = FALSE,
      labels = labels,
      na.value = "transparent",
      na.translate = FALSE
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  if (is.null(facet_nrow) & length(unique(data$facet_var)) <= 3) facet_nrow <- 1
  if (is.null(facet_nrow) & length(unique(data$facet_var)) > 3) facet_nrow <- 2
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    guides(fill = guide_legend(ncol = legend_ncol, byrow = TRUE, title = stringr::str_wrap(col_title, wrap_col_title))) +
    facet_wrap(
      ~ .data$facet_var,
      scales = "fixed",
      nrow = facet_nrow,
      labeller = labeller(
        facet_var = function(x)
          stringr::str_to_sentence(stringr::str_replace_all(x, "\\.", " "))
      )
    )

  return(plot)
}
