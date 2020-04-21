# ggplot boxplot functions

#' @title Theme for vertical box ggplots.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' 
#' ggplot2::ggplot() +
#'   theme_box("Courier", 9, 7) +
#'   ggplot2::ggtitle("This is a title of a selected font family and size")
theme_box <-
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
        legend.position = "bottom",
        legend.margin = margin(t = 20, b = 20),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm")
      )
    )
  }

#' @title Vertical box ggplot.
#' @description Vertical box ggplot that is not coloured and not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Defaults to NULL. Required if stat equals "boxplot".
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". If identity is selected, data provided must be grouped by the x_var with ymin, lower, middle, upper, ymax variables. Note "identity" does not provide outliers.
#' @param y_scale_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_scale_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
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
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. In a shinyapp, isMobile should be specified as input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' plot_data <- iris %>%
#' tibble::as_tibble() %>%
#'   dplyr::mutate(Species = stringr::str_to_sentence(Species))
#'
#' plot <- ggplot_box(data = plot_data, x_var = Species, y_var = Petal.Length,
#'                     title = "Iris petal length by species",
#'                     x_title = "Species",
#'                     y_title = "Petal length (cm)")
#'
#' plot
#'
#' plotly::ggplotly(plot, tooltip = "text")
#'
#' plot_data <- iris %>%
#'   dplyr::group_by(Species) %>%
#'   dplyr::summarise(boxplot_stats = list(rlang::set_names(boxplot.stats(Petal.Length)$stats,
#'   c('ymin','lower','middle','upper','ymax')))) %>%
#'   tidyr::unnest_wider(boxplot_stats)
#'
#' ggplot_box(data = plot_data, x_var = Species, y_var = Petal.Length, stat = "identity")
ggplot_box <- function(data,
                       x_var,
                       y_var = NULL,
                       stat = "boxplot",
                       y_scale_zero = TRUE,
                       y_scale_trans = "identity",
                       pal = NULL,
                       title = "[Title]",
                       subtitle = NULL,
                       x_title = "[X title]",
                       y_title = "[Y title]",
                       caption = "",
                       font_family = "Helvetica",
                       font_size_title = 11,
                       font_size_body = 10,
                       wrap_title = 70,
                       wrap_subtitle = 80,
                       wrap_x_title = 50,
                       wrap_y_title = 50,
                       wrap_caption = 80,
                       isMobile = FALSE){
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var) #categorical var
  y_var <- rlang::enquo(y_var) #numeric var
  
  x_var_vector <- dplyr::pull(data, !!x_var)
  if (stat == "boxplot") y_var_vector <- dplyr::pull(data, !!y_var)
  else if (stat == "identity") y_var_vector <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))
  
  if (is.numeric(x_var_vector)) stop("Please use a categorical x variable for a vertical boxplot")
  if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a vertical boxplot")
  
  if (is.null(pal)) pal <- pal_snz
  
  if (stat == "boxplot") {
    plot <- ggplot(data, aes(x = !!x_var, y = !!y_var)) +
      coord_cartesian(clip = "off") +
      theme_box(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      ) +
      geom_boxplot(
        stat = stat,
        fill = pal[1],
        width = 0.5,
        alpha = 0.9
      )
  }
  else if (stat == "identity") {
    plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_box(
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
        fill = pal[1],
        width = 0.5,
        alpha = 0.9
      )
  }
  
  if (y_scale_zero == TRUE) {
    y_scale_breaks <- pretty(c(0, y_var_vector))
    if(y_scale_trans == "log10") y_scale_breaks <- c(1, y_scale_breaks[y_scale_breaks > 1])
    y_scale_limits <- c(min(y_scale_breaks), max(y_scale_breaks))
  }
  else if (y_scale_zero == FALSE) {
    if(y_scale_trans != "log10") y_scale_breaks <- pretty(y_var_vector)
    if(y_scale_trans == "log10") {
      y_scale_breaks <- pretty(c(0, y_var_vector)) 
      y_scale_breaks <- c(1, y_scale_breaks[y_scale_breaks > 1])
    }
    y_scale_limits <- c(min(y_scale_breaks), max(y_scale_breaks))
  }
  
  plot <- plot +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = y_scale_breaks,
      limits = y_scale_limits,
      trans = y_scale_trans,
      oob = scales::rescale_none
    )
  
  if (isMobile == FALSE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, wrap_title),
        subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
        x = stringr::str_wrap(x_title, wrap_x_title),
        y = stringr::str_wrap(y_title, wrap_y_title),
        caption = stringr::str_wrap(caption, wrap_caption)
      ) +
      scale_x_discrete(
        labels = function(x)
          stringr::str_wrap(x, 15)
      )
  }
  else if (isMobile == TRUE){
    plot <- plot +
      labs(
        title = stringr::str_wrap(title, 40),
        subtitle = stringr::str_wrap(subtitle, 40),
        x = stringr::str_wrap(x_title, 20),
        y = stringr::str_wrap(y_title, 20),
        caption = stringr::str_wrap(caption, 50)
      ) +
      coord_flip() +
      scale_x_discrete(
        labels = function(x)
          stringr::str_wrap(x, 30)
      ) +
      theme(panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2)) +
      theme(panel.grid.major.y = element_blank())
    
  }
  
  return(plot)
}

#' @title Vertical box ggplot that is facetted.
#' @description Vertical box ggplot that is facetted, but not coloured.
#' @param data An tibble or dataframe. Required input.
#' @param x_var Unquoted categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Defaults to NULL. Required if stat equals "boxplot".
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param stat String of "boxplot" or "identity". Defaults to "boxplot". If identity is selected, data provided must be grouped by the x_var and facet_var with ymin, lower, middle, upper, ymax variables. Note "identity" does not provide outliers.
#' @param y_scale_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_scale_trans TRUEransformation of y-axis scale (e.g. "signed_sqrt"). Defaults to "identity", which has no transformation.
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
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. In a shinyapp, isMobile should be specified as input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' 
#' plot_data <- ggplot2::diamonds %>%
#'   dplyr::mutate(price_thousands = (price / 1000)) %>%
#'   dplyr::sample_frac(0.05)
#'
#' plot <- ggplot_box_facet(data = plot_data, x_var = cut, y_var = price_thousands, facet_var = color)
#'
#' plot
#'
#' plotly::ggplotly(plot, tooltip = "text")
ggplot_box_facet <-
  function(data,
           x_var,
           y_var = NULL,
           facet_var,
           stat = "boxplot",
           y_scale_zero = TRUE,
           y_scale_trans = "identity",
           facet_scales = "fixed",
           facet_nrow = NULL,
           pal = NULL,
           title = "[Title]",
           subtitle = NULL,
           x_title = "[X title]",
           y_title = "[Y title]",
           caption = "",
           font_family = "Helvetica",
           font_size_title = 11,
           font_size_body = 10,
           wrap_title = 70,
           wrap_subtitle = 80,
           wrap_x_title = 50,
           wrap_y_title = 50,
           wrap_caption = 80,
           isMobile = FALSE){
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #categorical var
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    
    x_var_vector <- dplyr::pull(data, !!x_var) 
    if (stat == "boxplot") y_var_vector <- dplyr::pull(data, !!y_var)
    else if (stat == "identity") y_var_vector <- c(dplyr::pull(data, .data$ymin), dplyr::pull(data, .data$ymax))
    facet_var_vector <- dplyr::pull(data, !!facet_var)
    
    if (is.numeric(x_var_vector)) stop("Please use a categorical x variable for a vertical boxplot")
    if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a vertical boxplot")
    if (is.numeric(facet_var_vector)) stop("Please use a categorical facet variable for a vertical boxplot")
    
    if (is.null(pal)) pal <- pal_snz
    
    if (stat == "boxplot") {
      plot <- ggplot(data, aes(x = !!x_var, y = !!y_var)) +
          coord_cartesian(clip = "off") +
          theme_box(
            font_family = font_family,
            font_size_body = font_size_body,
            font_size_title = font_size_title
          ) +
          geom_boxplot(
            stat = stat,
            fill = pal[1],
            width = 0.5,
            alpha = 0.9
          )
    }
    else if (stat == "identity") {
      plot <- ggplot(data) +
      coord_cartesian(clip = "off") +
      theme_box(
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
        fill = pal[1],
        width = 0.5,
        alpha = 0.9
      )
    }

    if (facet_scales %in% c("fixed", "free_x")) {
      if (y_scale_zero == TRUE) y_scale_breaks <- pretty(c(0, y_var_vector))
      else if (y_scale_zero == FALSE) y_scale_breaks <- pretty(y_var_vector)
      y_scale_max_breaks <- max(y_scale_breaks)
      y_scale_min_breaks <- min(y_scale_breaks)
      if (y_scale_zero == TRUE) y_scale_limits <- c(0, y_scale_max_breaks)
      else if (y_scale_zero == FALSE) y_scale_limits <- c(y_scale_min_breaks, y_scale_max_breaks)

      plot <- plot +
        scale_y_continuous(
          expand = c(0, 0),
          breaks = y_scale_breaks,
          limits = y_scale_limits,
          trans = y_scale_trans,
          oob = scales::rescale_none
        )
    }
    else if (facet_scales %in% c("free", "free_y")) {
      plot <- plot +
        scale_y_continuous(expand = c(0, 0),
                           trans = y_scale_trans,
                           oob = scales::rescale_none)
    }
    
    if (isMobile == FALSE){
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
        scale_x_discrete(
          labels = function(x)
            stringr::str_wrap(stringr::str_replace_all(x, "__.+$", ""), 15)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, nrow = facet_nrow)
    }
    else if (isMobile == TRUE){
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 20),
          caption = stringr::str_wrap(caption, 50)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = 1) +
        coord_flip() +
        scale_x_discrete(
          labels = function(x)
            stringr::str_wrap(stringr::str_replace_all(x, "__.+$", ""), 30)
        ) +
        theme(panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2)) +
        theme(panel.grid.major.y = element_blank())
      
    }
    
    return(plot)
  }
