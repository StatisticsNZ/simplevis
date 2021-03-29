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
        legend.key = element_rect(fill = "white"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm")
      )
    )
  }

#' @title Map of an array in ggplot.
#' @description Map of an array in ggplot that is not coloured and not facetted. 
#' @param data A stars object with 2 dimensions x and y. Required input.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use isMobile = input$isMobile. 
#' @return A ggplot object.
#' @export
#' @examples
#' ggplot_stars(data = example_stars)
ggplot_stars <- function(data,
                         pal = NULL,
                         title = "[Title]",
                         title_wrap = 70,
                         subtitle = NULL,
                         subtitle_wrap = 80,
                         caption = NULL,
                         caption_wrap = 80,
                         font_family = "Helvetica",
                         font_size_title = NULL,
                         font_size_body = NULL,
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
  
  if (is.null(pal)) pal <- viridis::viridis(4)[2]
  else pal <- pal[1]
  
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
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. Not applicable where isMobile equals TRUE.
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. Not applicable where isMobile equals TRUE.
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels_dp Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param col_method The method of colouring grid, either "bin", "quantile" or "category." Defaults to "quantile".
#' @param col_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
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
#' ggplot_stars_col(data = example_stars, 
#'    col_method = "quantile", col_cuts = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
#'    title = "Site medians, 2013-17")
ggplot_stars_col <- function(data,
                             pal = NULL,
                             pal_rev = FALSE,
                             title = "[Title]",
                             title_wrap = 70,
                             subtitle = NULL,
                             subtitle_wrap = 80,
                             col_cuts = NULL,
                             col_labels_dp = 1,
                             col_labels = NULL,
                             col_method = "quantile",
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
    
    n_col <- length(col_cuts) - 1
    if (is.null(pal)) pal <- viridis::viridis(n_col)
    else pal <- pal[1:n_col]
    
    if (is.null(col_labels)) labels <- LETTERS[1:length(col_cuts) - 1]
    if (!is.null(col_labels)) labels <- col_labels
  }
  else if (col_method == "bin") {
    if (is.null(col_cuts)) {
      data <- data %>%
        tibble::as_tibble()
      
      col_cuts <- pretty(col_var_vctr)
      
      data <- data %>%
        dplyr::mutate(dplyr::across(c(-.data$x, -.data$y), ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(col_labels)) labels <-  legend_labels_from_cuts(col_cuts, col_labels_dp)
      if (!is.null(col_labels)) labels <- col_labels
      
    }
    else if (!is.null(col_cuts)) {
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      
      data <- data %>%
        tibble::as_tibble() %>%
        dplyr::mutate(dplyr::across(c(-.data$x, -.data$y), ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(col_labels)) labels <-  legend_labels_from_cuts(col_cuts, col_labels_dp)
      if (!is.null(col_labels)) labels <- col_labels
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
    if (is.null(col_labels)) labels <-  legend_labels_from_cuts(col_cuts, col_labels_dp)
    if (!is.null(col_labels)) labels <- col_labels
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
      guides(fill = guide_legend(ncol = col_labels_ncol, nrow = col_labels_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap)))
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
        caption = stringr::str_wrap(caption, 50)
      )  +
      guides(fill = guide_legend(ncol = 1, byrow = TRUE, title = stringr::str_wrap(col_title, 15)))
      
  }
  
  return(plot)
}

#' @title Map of an array in ggplot that is facetted.
#' @description Map of an array in ggplot that is facetted, but not coloured. 
#' @param data A stars object with 2 dimensions, x and y, and multiple named attribute layers with usual convention of lower case and underscores. These attribute layers will be facetted. Required input.
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_ncol The number of columns of facetted plots. 
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
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
                               facet_ncol = NULL,
                               facet_nrow = NULL,
                               title = "[Title]",
                               title_wrap = 70,
                               subtitle = NULL,
                               subtitle_wrap = 80,
                               caption = NULL,
                               caption_wrap = 80,
                               font_family = "Helvetica",
                               font_size_title = NULL,
                               font_size_body = NULL) {
  
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
  
  if (is.null(pal)) pal <- viridis::viridis(4)[2]
  else pal <- pal[1]

  plot <- plot +
    geom_raster(aes(x = .data$x, y = .data$y),
                fill = pal,
                alpha = 0.1,
                data = data) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    facet_wrap(
      ~ .data$facet_var,
      scales = "fixed",
      ncol = facet_ncol,
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
#' @param pal Character vector of hex codes. Defaults to viridis. Use the pals package to find a suitable palette.
#' @param pal_rev Reverses the palette. Defaults to FALSE.
#' @param title Title string. Defaults to "[Title]".
#' @param title_wrap Number of characters to wrap the title to. Defaults to 70. 
#' @param subtitle Subtitle string. Defaults to "[Subtitle]".
#' @param subtitle_wrap Number of characters to wrap the subtitle to. Defaults to 80. 
#' @param col_cuts A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles. 
#' @param col_labels_dp Select the appropriate number of decimal places for numeric variable auto legend labels. Defaults to 1.
#' @param col_method The method of colouring features, either "bin", "quantile" or "category." Defaults to "quantile". Note all numeric variables are cut to be inclusive of the min in the range, and exclusive of the max in the range (except for the final bucket which includes the highest value).
#' @param col_labels A vector of manual legend label values. Defaults to NULL, which results in automatic labels.
#' @param col_labels_ncol The number of columns in the legend. Defaults to 1.
#' @param col_labels_nrow The number of rows in the legend.
#' @param col_quantile_by_facet TRUE of FALSE whether quantiles should be calculated for each group of the facet variable. Defaults to TRUE.
#' @param col_title Colour title string for the legend. Defaults to NULL.
#' @param col_title_wrap Number of characters to wrap the colour title to. Defaults to 25. 
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots. 
#' @param caption Caption title string. Defaults to NULL.
#' @param caption_wrap Number of characters to wrap the caption to. Defaults to 80. 
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
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
                                   pal = NULL,
                                   pal_rev = FALSE,
                                   title = "[Title]",
                                   title_wrap = 70,
                                   subtitle = NULL,
                                   subtitle_wrap = 80,
                                   col_cuts = NULL,
                                   col_labels_dp = 1,
                                   col_labels = NULL,
                                   col_method = "quantile",
                                   col_labels_ncol = NULL,
                                   col_labels_nrow = NULL,
                                   col_quantile_by_facet = TRUE,
                                   col_title = "",
                                   col_title_wrap = 25,
                                   facet_ncol = NULL, 
                                   facet_nrow = NULL,
                                   caption = NULL,
                                   caption_wrap = 80,
                                   font_family = "Helvetica",
                                   font_size_title = NULL,
                                   font_size_body = NULL) {
  
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
    
    n_col <- length(col_cuts) - 1
    if (is.null(pal)) pal <- viridis::viridis(n_col)
    else pal <- pal[1:n_col]
    
    if (is.null(col_labels)) labels <- LETTERS[1:length(col_cuts) - 1]
    if (!is.null(col_labels)) labels <- col_labels
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
      if (is.null(col_labels)) labels <-  legend_labels_from_cuts(col_cuts, col_labels_dp)
      if (!is.null(col_labels)) labels <- col_labels
      
    }
    else if (!is.null(col_cuts)) {
      if (!(dplyr::first(col_cuts) %in% c(0,-Inf))) warning("The first element of the col_cuts vector should generally be 0 (or -Inf if there are negative values)")
      if (dplyr::last(col_cuts) != Inf) warning("The last element of the col_cuts vector should generally be Inf")
      
      data <- data %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = c(-.data$x, -.data$y), names_to = "facet_var", values_to = "col_var") %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(col_labels)) labels <-  legend_labels_from_cuts(col_cuts, col_labels_dp)
      if (!is.null(col_labels)) labels <- col_labels
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
      if (is.null(col_labels)) labels <- paste0( legend_labels_from_cuts(col_cuts * 100, 0), "\u1D57\u02B0 percentile")
      if (!is.null(col_labels)) labels <- col_labels
    }
    else if (col_quantile_by_facet == FALSE) {
      data <- data %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(cols = c(-.data$x, -.data$y), names_to = "facet_var", values_to = "col_var") %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ percent_rank(.))) %>%
        dplyr::mutate(dplyr::across(.data$col_var, ~ cut(., col_cuts, right = FALSE, include.lowest = TRUE)))
      
      if (is.null(pal)) pal <- viridis::viridis(length(col_cuts) - 1)
      if (is.null(col_labels)) labels <- paste0( legend_labels_from_cuts(col_cuts * 100, 0), "\u1D57\u02B0 percentile")
      if (!is.null(col_labels)) labels <- col_labels
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
  
  plot <- plot +
    labs(
      title = stringr::str_wrap(title, title_wrap),
      subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
      caption = stringr::str_wrap(caption, caption_wrap)
    ) +
    guides(fill = guide_legend(ncol = col_labels_ncol, nrow = col_labels_nrow, byrow = TRUE, title = stringr::str_wrap(col_title, col_title_wrap))) +
    facet_wrap(
      ~ .data$facet_var,
      scales = "fixed",
      ncol = facet_ncol,
      nrow = facet_nrow, 
      labeller = labeller(
        facet_var = function(x)
          stringr::str_to_sentence(stringr::str_replace_all(x, "\\.", " "))
      )
    )

  return(plot)
}
