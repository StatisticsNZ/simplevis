# ggplot vbar functions

#' @title Theme for vertical bar ggplots.
#' @param font_family Font family to use. Defaults to "Helvetica".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   theme_vbar("Courier", 9, 7) +
#'   ggplot2::ggtitle("This is a title of a selected font family and size")
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
        legend.position = "bottom",
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
#' @param hover_var Unquoted variable to be an additional hover variable for when used inside plotly::ggplotly(). Defaults to NULL.
#' @param x_scale_date_format Date format for x axis labels.
#' @param y_scale_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_scale_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_scale_labels Argument to adjust the format of the y scale labels.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param width Width of bars. Defaults to 0.75.
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
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. In a shinyapp, isMobile should be specified as input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' plot_data <- dplyr::storms %>%
#'   dplyr::group_by(year) %>%
#'   dplyr::summarise(average_wind = round(mean(wind), 2)) 
#'
#' plot <- ggplot_vbar(data = plot_data, x_var = year, y_var = average_wind,
#'       title = "Average wind speed of Atlantic storms, 1975-2015",
#'       x_title = "Year",
#'       y_title = "Average maximum sustained wind speed (knots)")
#'
#' plot
#'
#' plotly::ggplotly(plot, tooltip = "text")
ggplot_vbar <- function(data,
                        x_var,
                        y_var,
                        hover_var = NULL,
                        x_scale_date_format = "%Y",
                        y_scale_zero = TRUE,
                        y_scale_trans = "identity",
                        y_scale_labels = waiver(),
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
                        wrap_title = 70,
                        wrap_subtitle = 80,
                        wrap_x_title = 50,
                        wrap_y_title = 50,
                        wrap_caption = 80,
                        isMobile = FALSE) {
  
  data <- dplyr::ungroup(data)
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var) #numeric var
  hover_var <- rlang::enquo(hover_var)
  
  x_var_vector <- dplyr::pull(data, !!x_var)
  y_var_vector <- dplyr::pull(data, !!y_var)
  
  if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a vertical bar plot")
  
  if(min(y_var_vector, na.rm = TRUE) < 0 & y_scale_zero == TRUE) {
    y_scale_zero <- FALSE
    message("y_scale_zero must be FALSE as data contains values less than zero")
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
  
  plot <- ggplot(data, aes(x = !!x_var, y = !!y_var, key = !!hover_var)) +
    coord_cartesian() +
    theme_vbar(
      font_family = font_family,
      font_size_body = font_size_body,
      font_size_title = font_size_title
    )
  
  if (lubridate::is.Date(x_var_vector)) width <- 365 * width

  if (is.null(rlang::get_expr(hover_var))) {
    plot <- plot +
      geom_col(aes(text = paste(
        paste0(
          stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
          ": ",
          !!x_var
        ),
        paste0(
          stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
          ": ",
          !!y_var
        ),
        sep = "<br>"
      )),
      fill = pal[1],
      width = width
      )
  }
  else if (!is.null(rlang::get_expr(hover_var))) {
    plot <- plot +
      geom_col(aes(text = paste(
        paste0(
          stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
          ": ",
          !!x_var
        ),
        paste0(
          stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
          ": ",
          !!y_var
        ),
        paste0(
          stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(hover_var), "_", " ")),
          ": ",
          !!hover_var
        ),
        sep = "<br>"
      )),
      fill = pal[1],
      width = width
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
  
  if (lubridate::is.Date(x_var_vector)) {
    if(isMobile == FALSE) x_scale_n <- 6
    else if(isMobile == TRUE) x_scale_n <- 4
    
    x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
    x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
    if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
    if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
    
    plot <- plot +
      scale_x_date(
        expand = c(0, 0),
        breaks = x_scale_breaks,
        limits = x_scale_limits,
        labels = scales::date_format(x_scale_date_format)
      )
  }
  else if (is.numeric(x_var_vector)) {
    if(isMobile == FALSE) x_scale_n <- 6
    else if(isMobile == TRUE) x_scale_n <- 4
    
    x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
    x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
    if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
    if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
    
    plot <- plot +
      scale_x_continuous(expand = c(0, 0),
                         breaks = x_scale_breaks,
                         limits = x_scale_limits,
                         oob = scales::rescale_none)
  }
  else if (is.character(x_var_vector) | is.factor(x_var_vector)){
    plot <- plot +
      scale_x_discrete(expand = c(0, 0))
  }

  plot <- plot +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = y_scale_breaks,
      limits = y_scale_limits,
      trans = y_scale_trans,
      labels = y_scale_labels,
      oob = scales::rescale_none
    )
  
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
        y = stringr::str_wrap(y_title, 20),
        caption = stringr::str_wrap(caption, 50)
      )
  }
  
  return(plot)
}

#' @title Vertical bar ggplot that is coloured.
#' @description Vertical bar ggplot that is coloured, but not facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param hover_var Unquoted variable to be an additional hover variable for when used inside plotly::ggplotly(). Defaults to NULL.
#' @param x_scale_date_format Date format for x axis labels.
#' @param y_scale_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_scale_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_scale_labels Argument to adjust the format of the y scale labels.
#' @param col_scale_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to FALSE.
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
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. In a shinyapp, isMobile should be specified as input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' plot_data <- dplyr::storms %>%
#'   dplyr::mutate(status = stringr::str_to_sentence(status)) %>%
#'   dplyr::group_by(year, status) %>%
#'   dplyr::summarise(average_wind = round(mean(wind), 2)) 
#'
#' plot <- ggplot_vbar_col(data = plot_data, x_var = year, y_var = average_wind, col_var = status)
#'
#' plot
#'
#' plotly::ggplotly(plot, tooltip = "text")
ggplot_vbar_col <-
  function(data,
           x_var,
           y_var,
           col_var,
           hover_var = NULL,
           x_scale_date_format = "%Y",
           y_scale_zero = TRUE,
           y_scale_trans = "identity",
           y_scale_labels = waiver(),
           col_scale_drop = FALSE,
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
    y_var <- rlang::enquo(y_var) #numeric var
    x_var <- rlang::enquo(x_var) #categorical var
    col_var <- rlang::enquo(col_var) #categorical var
    hover_var <- rlang::enquo(hover_var)
    
    y_var_vector <- dplyr::pull(data, !!y_var)
    x_var_vector <- dplyr::pull(data, !!x_var)
    col_var_vector <- dplyr::pull(data, !!col_var)
    
    if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(col_var_vector)) stop("Please use a categorical colour variable for a vertical bar plot")
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (position == "stack" & y_scale_trans != "identity") message("simplevis may not perform correctly using a y scale other than identity where position equals stack")
    if (position == "stack" & y_scale_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and y_scale_zero equal to FALSE")

    if(min(y_var_vector, na.rm = TRUE) < 0 & y_scale_zero == TRUE) {
      y_scale_zero <- FALSE
      message("y_scale_zero must be FALSE as data contains values less than zero")
    }
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (is.null(pal)) pal <- pal_snz
    
    plot <- ggplot(data, aes(x = !!x_var, y = !!y_var, key = !!hover_var)) +
      coord_cartesian() +
      theme_vbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      )
    
    if (lubridate::is.Date(x_var_vector)) width <- 365 * width
    
    if (is.null(rlang::get_expr(hover_var))) {
      plot <- plot +
        geom_col(aes(
          fill = !!col_var,
          text = paste(
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
              ": ",
              !!x_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(col_var), "_", " ")),
              ": ",
              !!col_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
              ": ",
              !!y_var
            ),
            sep = "<br>"
          )
        ),
        width = width,
        position = position2)
    }
    else if (!is.null(rlang::get_expr(hover_var))) {
      plot <- plot +
        geom_col(aes(
          fill = !!col_var,
          text = paste(
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
              ": ",
              !!x_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(col_var), "_", " ")),
              ": ",
              !!col_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
              ": ",
              !!y_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(hover_var), "_", " ")),
              ": ",
              !!hover_var
            ),
            sep = "<br>"
          )
        ),
        width = width,
        position = position2)
    }
    
    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by_at(vars(!!x_var)) %>%
        dplyr::summarise_at(vars(!!y_var), list( ~ (sum(., na.rm = TRUE)))) %>%
        dplyr::ungroup()
      
      y_var_vector <- dplyr::pull(data_sum, !!y_var)
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
    
    if (lubridate::is.Date(x_var_vector)) {
      if(isMobile == FALSE) x_scale_n <- 6
      else if(isMobile == TRUE) x_scale_n <- 4
      
      x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
      x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
      if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
      if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
      
      plot <- plot +
        scale_x_date(
          expand = c(0, 0),
          breaks = x_scale_breaks,
          limits = x_scale_limits,
          labels = scales::date_format(x_scale_date_format)
        )
    }
    else if (is.numeric(x_var_vector)) {
      if(isMobile == FALSE) x_scale_n <- 6
      else if(isMobile == TRUE) x_scale_n <- 4
      
      x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
      x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
      if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
      if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
      
      plot <- plot +
        scale_x_continuous(expand = c(0, 0),
                           breaks = x_scale_breaks,
                           limits = x_scale_limits,
                           oob = scales::rescale_none)
    }
    else if (is.character(x_var_vector) | is.factor(x_var_vector)){
      plot <- plot +
        scale_x_discrete(expand = c(0, 0))
    }

    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = col_scale_drop,
        labels = labels,
        na.value = "#A8A8A8"
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = y_scale_breaks,
        limits = y_scale_limits,
        trans = y_scale_trans,
        labels = y_scale_labels,
        oob = scales::rescale_none
      )
    
    if (isMobile == FALSE) {
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, wrap_title),
          subtitle = stringr::str_wrap(subtitle, wrap_subtitle),
          x = stringr::str_wrap(x_title, wrap_x_title),
          y = stringr::str_wrap(y_title, wrap_y_title),
          caption = stringr::str_wrap(caption, wrap_caption)
        ) +
        guides(fill = guide_legend(
          ncol = legend_ncol,
          byrow = TRUE,
          reverse = TRUE,
          title = stringr::str_wrap(col_title, wrap_col_title)
        ))
    }
    else if (isMobile == TRUE) {
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 20),
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

#' @title Vertical bar ggplot that is facetted.
#' @description Vertical bar ggplot that is facetted, but not coloured.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param hover_var Unquoted variable to be an additional hover variable for when used inside plotly::ggplotly(). Defaults to NULL.
#' @param x_scale_date_format Date format for x axis labels.
#' @param y_scale_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_scale_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_scale_labels Argument to adjust the format of the y scale labels.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_nrow The number of rows of facetted plots. Defaults to NULL, which generally chooses 2 rows. Not applicable to where isMobile is TRUE.
#' @param pal Character vector of hex codes. Defaults to NULL, which selects the Stats NZ palette.
#' @param width Width of bars. Defaults to 0.75.
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
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. In a shinyapp, isMobile should be specified as input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' plot_data <- dplyr::storms %>%
#'   dplyr::mutate(status = stringr::str_to_sentence(status)) %>%
#'   dplyr::group_by(year, status) %>%
#'   dplyr::summarise(average_wind = round(mean(wind), 2)) 
#'
#' plot <- ggplot_vbar_facet(data = plot_data, x_var = year, y_var = average_wind,
#'                           facet_var = status)
#'
#' plot
#'
#' plotly::ggplotly(plot, tooltip = "text")
ggplot_vbar_facet <-
  function(data,
           x_var,
           y_var,
           facet_var,
           hover_var = NULL,
           x_scale_date_format = "%Y",
           y_scale_zero = TRUE,
           y_scale_trans = "identity",
           y_scale_labels = waiver(),
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
           wrap_title = 70,
           wrap_subtitle = 80,
           wrap_x_title = 50,
           wrap_y_title = 50,
           wrap_caption = 80,
           isMobile = FALSE) {
    
    data <- dplyr::ungroup(data)
    x_var <- rlang::enquo(x_var) #categorical var
    y_var <- rlang::enquo(y_var) #numeric var
    facet_var <- rlang::enquo(facet_var) #categorical var
    hover_var <- rlang::enquo(hover_var)
    
    x_var_vector <- dplyr::pull(data, !!x_var)
    y_var_vector <- dplyr::pull(data, !!y_var)
    facet_var_vector <- dplyr::pull(data, !!facet_var)
    
    if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(facet_var_vector)) stop("Please use a categorical facet variable for a vertical bar plot")
    
    if(min(y_var_vector, na.rm = TRUE) < 0 & y_scale_zero == TRUE) {
      y_scale_zero <- FALSE
      message("y_scale_zero must be FALSE as data contains values less than zero")
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
    
    plot <- ggplot(data, aes(x = !!x_var, y = !!y_var, key = !!hover_var)) +
      coord_cartesian() +
      theme_vbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      )
    
    if (lubridate::is.Date(x_var_vector)) width <- 365 * width
    
    if (is.null(rlang::get_expr(hover_var))) {
      plot <- plot +
        geom_col(aes(text = paste(
          paste0(
            stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
            ": ",
            !!x_var
          ),
          paste0(
            stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(facet_var), "_", " ")),
            ": ",
            !!facet_var
          ),
          paste0(
            stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
            ": ",
            !!y_var
          ),
          sep = "<br>"
        )),
        fill = pal[1],
        width = width
        )
    }
    else if (!is.null(rlang::get_expr(hover_var))) {
      plot <- plot +
        geom_col(aes(text = paste(
          paste0(
            stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
            ": ",
            !!x_var
          ),
          paste0(
            stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(facet_var), "_", " ")),
            ": ",
            !!facet_var
          ),
          paste0(
            stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
            ": ",
            !!y_var
          ),
          paste0(
            stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(hover_var), "_", " ")),
            ": ",
            !!hover_var
          ),
          sep = "<br>"
        )),
        fill = pal[1],
        width = width
        )
    }
    
    if (facet_scales %in% c("fixed", "free_y")) {
      
      if (lubridate::is.Date(x_var_vector)) {
        if(isMobile == FALSE) x_scale_n <- 5
        else if(isMobile == TRUE) x_scale_n <- 4
        
        x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
        x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
        if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
        if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
        
        plot <- plot +
          scale_x_date(
            expand = c(0, 0),
            breaks = x_scale_breaks,
            limits = x_scale_limits,
            labels = scales::date_format(x_scale_date_format)
          )
      }
      else if (is.numeric(x_var_vector)) {
        if(isMobile == FALSE) x_scale_n <- 5
        else if(isMobile == TRUE) x_scale_n <- 4
        
        x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
        x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
        if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
        if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
        
        plot <- plot +
          scale_x_continuous(expand = c(0, 0),
                             breaks = x_scale_breaks,
                             limits = x_scale_limits,
                             oob = scales::rescale_none)
      }
      else if (is.character(x_var_vector) | is.factor(x_var_vector)){
        plot <- plot +
          scale_x_discrete(expand = c(0, 0))
      }

    }
    
    if (facet_scales %in% c("fixed", "free_x")) {
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
          labels = y_scale_labels,
          oob = scales::rescale_none
        )
    }
    else if (facet_scales %in% c("free", "free_y")) {
      plot <- plot +
        scale_y_continuous(expand = c(0, 0),
                           trans = y_scale_trans,
                           labels = y_scale_labels,
                           oob = scales::rescale_none)
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
          y = stringr::str_wrap(y_title, 20),
          caption = stringr::str_wrap(caption, 50)
        ) +
        facet_wrap(vars(!!facet_var), scales = facet_scales, ncol = 1)
    }
    
    return(plot)
  }

#' @title Vertical bar ggplot that is coloured and facetted.
#' @description Vertical bar ggplot that is coloured and facetted.
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric, date or categorical variable to be on the x axis. Required input.
#' @param y_var Unquoted numeric variable to be on the y axis. Required input.
#' @param col_var Unquoted categorical variable to colour the bars. Required input.
#' @param facet_var Unquoted categorical variable to facet the data by. Required input.
#' @param hover_var Unquoted variable to be an additional hover variable for when used inside plotly::ggplotly(). Defaults to NULL.
#' @param x_scale_date_format Date format for x axis labels.
#' @param y_scale_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_scale_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_scale_labels Argument to adjust the format of the y scale labels.
#' @param col_scale_drop TRUE or FALSE of whether to drop unused levels from the legend. Defaults to FALSE.
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
#' @param isMobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. In a shinyapp, isMobile should be specified as input$isMobile.
#' @return A ggplot object.
#' @export
#' @examples
#' plot_data <- dplyr::storms %>%
#'   dplyr::mutate(status = stringr::str_to_sentence(status)) %>%
#'   dplyr::group_by(year, status, name) %>%
#'   dplyr::summarise(average_wind = round(mean(wind), 2)) %>%
#'   dplyr::filter(year %in% 1975:1980) %>%
#'   dplyr::filter(!(status == "Tropical storm" & year == 1980)) %>%
#'   dplyr::filter(name %in% c("Karl", "Juliet", "Jeanne", "Ivan", "Hermine",
#'   "Henri", "Gloria", "Georges", "Frederic"))
#'
#'   plot <- ggplot_vbar_col_facet(data = plot_data, x_var = year, y_var = average_wind,
#'                                 col_var = name, facet_var = status)
#'
#'   plot
#'
#'   plotly::ggplotly(plot, tooltip = "text")
ggplot_vbar_col_facet <-
  function(data,
           x_var,
           y_var,
           col_var,
           facet_var,
           hover_var = NULL,
           x_scale_date_format = "%Y",
           y_scale_zero = TRUE,
           y_scale_trans = "identity",
           y_scale_labels = waiver(),
           col_scale_drop = FALSE,
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
    x_var <- rlang::enquo(x_var) #categorical var
    y_var <- rlang::enquo(y_var) #numeric var
    col_var <- rlang::enquo(col_var) #categorical var
    facet_var <- rlang::enquo(facet_var) #categorical var
    hover_var <- rlang::enquo(hover_var)
    
    x_var_vector <- dplyr::pull(data, !!x_var)
    y_var_vector <- dplyr::pull(data, !!y_var)
    col_var_vector <- dplyr::pull(data, !!col_var)
    facet_var_vector <- dplyr::pull(data, !!facet_var)
    
    if (!is.numeric(y_var_vector)) stop("Please use a numeric y variable for a vertical bar plot")
    if (is.numeric(facet_var_vector)) stop("Please use a categorical facet variable for a vertical bar plot")
    
    if(is.null(font_size_title)){
      if (isMobile == FALSE) font_size_title <- 11
      else if (isMobile == TRUE) font_size_title <- 15
    }
    if(is.null(font_size_body)){
      if (isMobile == FALSE) font_size_body <- 10
      else if (isMobile == TRUE) font_size_body <- 14
    }
    
    if (position == "stack" & y_scale_trans != "identity") message("simplevis may not perform correctly using a y scale other than identity where position equals stack")
    if (position == "stack" & y_scale_zero == FALSE) message("simplevis may not perform correctly with position equal to stack and y_scale_zero equal to FALSE")
    
    if(min(y_var_vector, na.rm = TRUE) < 0 & y_scale_zero == TRUE) {
      y_scale_zero <- FALSE
      message("y_scale_zero must be FALSE as data contains values less than zero")
    }
    
    if (position == "stack") position2 <- "stack"
    else if (position == "dodge") position2 <- position_dodge2(preserve = "single")
    
    if (is.null(pal)) pal <- pal_snz
    
    plot <- ggplot(data, aes(x = !!x_var, y = !!y_var, key = !!hover_var)) +
      coord_cartesian() +
      theme_vbar(
        font_family = font_family,
        font_size_body = font_size_body,
        font_size_title = font_size_title
      )
    
    if (lubridate::is.Date(x_var_vector)) width <- 365 * width
    
    if (is.null(rlang::get_expr(hover_var))) {
      plot <- plot +
        geom_col(aes(
          fill = !!col_var,
          text = paste(
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
              ": ",
              !!x_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(col_var), "_", " ")),
              ": ",
              !!col_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(facet_var), "_", " ")),
              ": ",
              !!facet_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
              ": ",
              !!y_var
            ),
            sep = "<br>"
          )
        ),
        width = width,
        position = position2)
    }
    else if (!is.null(rlang::get_expr(hover_var))) {
      plot <- plot +
        geom_col(aes(
          fill = !!col_var,
          text = paste(
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(x_var), "_", " ")),
              ": ",
              !!x_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(col_var), "_", " ")),
              ": ",
              !!col_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(facet_var), "_", " ")),
              ": ",
              !!facet_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(y_var), "_", " ")),
              ": ",
              !!y_var
            ),
            paste0(
              stringr::str_to_sentence(stringr::str_replace_all(rlang::as_name(hover_var), "_", " ")),
              ": ",
              !!hover_var
            ),
            sep = "<br>"
          )
        ),
        width = width,
        position = position2)
    }
    
    if (!is.null(legend_labels)) labels <- legend_labels
    if (is.null(legend_labels)) labels <- waiver()
    
    if (position == "stack") {
      data_sum <- data %>%
        dplyr::group_by_at(vars(!!x_var, !!facet_var)) %>%
        dplyr::summarise_at(vars(!!y_var), list( ~ (sum(., na.rm = TRUE)))) %>%
        dplyr::ungroup()
      
      y_var_vector <- dplyr::pull(data_sum, !!y_var)
    }

    if (facet_scales %in% c("fixed", "free_y")) {
      
      if (lubridate::is.Date(x_var_vector)) {
        if(isMobile == FALSE) x_scale_n <- 5
        else if(isMobile == TRUE) x_scale_n <- 4
        
        x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
        x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
        if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
        if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
        
        plot <- plot +
          scale_x_date(
            expand = c(0, 0),
            breaks = x_scale_breaks,
            limits = x_scale_limits,
            labels = scales::date_format(x_scale_date_format)
          )
      }
      else if (is.numeric(x_var_vector)) {
        if(isMobile == FALSE) x_scale_n <- 5
        else if(isMobile == TRUE) x_scale_n <- 4
        
        x_scale_breaks <- pretty(x_var_vector, n = x_scale_n)
        x_scale_limits <- c(min(x_scale_breaks), max(x_scale_breaks))
        if(x_scale_limits[1] == min(x_var_vector)) x_scale_limits <- c(NA, x_scale_limits[2])
        if(x_scale_limits[2] == max(x_var_vector)) x_scale_limits <- c(x_scale_limits[1], NA)
        
        plot <- plot +
          scale_x_continuous(expand = c(0, 0),
                             breaks = x_scale_breaks,
                             limits = x_scale_limits,
                             oob = scales::rescale_none)
      }
      else if (is.character(x_var_vector) | is.factor(x_var_vector)){
        plot <- plot +
          scale_x_discrete(expand = c(0, 0))
      }
    }
    
    if (facet_scales %in% c("fixed", "free_x")) {
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
          labels = y_scale_labels,
          oob = scales::rescale_none
        )
    }
    else if (facet_scales %in% c("free", "free_y")) {
      plot <- plot +
        scale_y_continuous(expand = c(0, 0),
                           trans = y_scale_trans,
                           labels = y_scale_labels,
                           oob = scales::rescale_none)
    }
    
    plot <- plot +
      scale_fill_manual(
        values = pal,
        drop = col_scale_drop,
        labels = labels,
        na.value = "#A8A8A8"
      )

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
        guides(fill = guide_legend(
          ncol = legend_ncol,
          byrow = TRUE,
          reverse = TRUE,
          title = stringr::str_wrap(col_title, wrap_col_title)
        ))
      
    }
    else if (isMobile == TRUE) {
      plot <- plot +
        labs(
          title = stringr::str_wrap(title, 40),
          subtitle = stringr::str_wrap(subtitle, 40),
          x = stringr::str_wrap(x_title, 20),
          y = stringr::str_wrap(y_title, 20),
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
