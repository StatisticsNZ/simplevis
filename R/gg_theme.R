#' @title Create a simple theme for a graph. 
#'
#' @param font The font for all text to use. Defaults to "".
#' @param font_title The font for the title. If NULL, inherits from font argument.
#' @param font_subtitle The font for the subtitle. If NULL, inherits from font argument.
#' @param font_body The font for the subtitle. If NULL, inherits from font argument.
#' @param size_title The size of the title font. Defaults to 11.
#' @param size_subtitle The size of the subtitle font. Defaults to 10.
#' @param size_body The size of all text other than the title or subtitle. Defaults to 10.
#' @param size_axis The size of the axis. Defaults to 0.3.
#' @param size_ticks The size of the ticks. Defaults to 0.3.
#' @param size_gridlines The size of the vertical major gridlines. Defaults to 0.2.
#' @param style_title The style of the title font. Defaults to "bold".
#' @param style_subtitle The style of the subtitle font. Defaults to "plain". 
#' @param style_body The style of all text other than the title or subtitle. Defaults to "plain".
#' @param pal_title The colour palette for the title font. Defaults to "#000000".
#' @param pal_subtitle The colour palette for the subtitle font. Defaults to "#000000".
#' @param pal_body The colour palette for all text other than the title or subtitle. Defaults to "#323232".
#' @param pal_axis The colour palette for the axis. Defaults to "#323232".
#' @param pal_ticks The colour palette for the ticks. Defaults to "#323232".
#' @param pal_background A two colour vector. The first colour if for the panel (and legend key). The second colour is for the rest of the background.
#' @param pal_gridlines The colour palette for the vertical major gridlines. Defaults to "#D3D3D3". 
#' @param gridlines_h TRUE or FALSE of whether to show hotizontal gridlines.
#' @param gridlines_v TRUE or FALSE of whether to show vertical gridlines.
#' @param void TRUE or FALSE of whether to drop all axis lines, ticks and x and y labels. Useful for maps. Defaults to FALSE.  
#'
#' @return A ggplot theme.
#' @export
#' 
gg_theme <-
  function(font = "",
           font_title = NULL,
           font_subtitle = NULL,
           font_body = NULL,
           size_title = 11,
           size_subtitle = 10,
           size_body = 10,
           size_axis = 0.3,
           size_ticks = 0.3,
           size_gridlines = 0.2,
           style_title = "bold",
           style_subtitle = "plain",
           style_body = "plain",
           pal_title = "#000000",
           pal_subtitle = "#000000",
           pal_body = "#323232",
           pal_axis = "#323232",
           pal_ticks = "#323232",
           pal_background = c("#ffffff", "#ffffff"),
           pal_gridlines = "#D3D3D3",
           gridlines_h = FALSE, 
           gridlines_v = FALSE,
           void = FALSE) {
    
    if (is.null(font_title)) font_title <- font
    if (is.null(font_subtitle)) font_subtitle <- font
    if (is.null(font_body)) font_body <- font
    
    if (gridlines_h == TRUE) {
      if (gridlines_v == FALSE) { #horizontal
        theme <- theme(
          text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = margin(b = size_body / 2)),
          legend.text = element_text(margin = margin(r = 10), hjust = 0),
          plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
          plot.title.position = "panel",
          plot.caption.position = "plot",
          panel.border = element_blank(),
          panel.spacing = unit(2.5, "lines"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = pal_gridlines, size = size_gridlines),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          panel.background = element_rect(colour = pal_background[1], fill = pal_background[1]), 
          legend.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          strip.background = element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
      else if (gridlines_v == TRUE) { #both
        theme <- theme(
          text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = margin(b = size_body / 2)),
          legend.text = element_text(margin = margin(r = 10), hjust = 0),
          plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
          plot.title.position = "panel",
          plot.caption.position = "plot",
          panel.border = element_blank(),
          panel.spacing = unit(2.5, "lines"),
          plot.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          panel.background = element_rect(colour = pal_background[1], fill = pal_background[1]), 
          panel.grid.major.x = element_line(colour = pal_gridlines, size = size_gridlines),
          panel.grid.major.y = element_line(colour = pal_gridlines, size = size_gridlines),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          strip.background = element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
    }
    else if (gridlines_h == FALSE) {
      if (gridlines_v == FALSE) { #none
        theme <- theme(
          text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = margin(b = size_body / 2)),
          legend.text = element_text(margin = margin(r = 10), hjust = 0),
          plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
          plot.title.position = "panel",
          plot.caption.position = "plot",
          panel.border = element_blank(),
          panel.spacing = unit(2.5, "lines"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          panel.background = element_rect(colour = pal_background[1], fill = pal_background[1]), 
          legend.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          strip.background = element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
      else if (gridlines_v == TRUE) { #vertical
        theme <- theme(
          text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body),
          plot.title = element_text(family = font_title, size = size_title, colour = pal_title, face = style_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, colour = pal_subtitle, face = style_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = font_body, size = size_body, colour = pal_body, face = style_body, hjust = 0.5, margin = margin(b = size_body / 2)),
          legend.text = element_text(margin = margin(r = 10), hjust = 0),
          plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
          plot.title.position = "panel",
          plot.caption.position = "plot",
          panel.border = element_blank(),
          panel.spacing = unit(2.5, "lines"),
          plot.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          panel.background = element_rect(colour = pal_background[1], fill = pal_background[1]), 
          panel.grid.major.x = element_line(colour = pal_gridlines, size = size_gridlines),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.background = element_rect(colour = pal_background[2], fill = pal_background[2]), 
          strip.background = element_rect(colour = pal_background[2], fill = pal_background[2]),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = pal_background[1], fill = pal_background[1]),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
    }
    
    if (void == TRUE) {
      theme <- theme +
        theme(axis.line = element_blank()) +
        theme(axis.ticks = element_blank()) +
        theme(axis.text.x = element_blank()) +
        theme(axis.text.y = element_blank())
    }
    
    return(theme)
  }

#' @title Extra theme elements for ggplot2 graphs on mobile devices
#' @description Extra theme elements for ggplot2 graphs on mobile devices
#' @param void TRUE or FALSE of whether to drop all axis lines, ticks and x and y labels. Useful for maps. Defaults to FALSE.
#'
#' @return A ggplot theme.
#' @keywords internal
theme_mobile_extra <- function(void = FALSE) {
  if (void == FALSE) {
    theme(
      plot.title.position = "plot",
      plot.margin = margin(t = 2, l = 2, b = 0, r = 10),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.justification = "left",
      legend.margin = margin(t = 0, b = 0, l = 0, r = 0),
      axis.text.x = element_text(hjust = 0.75)
    )
  }
  else if (void == TRUE) {
    theme(
      plot.title.position = "plot",
      plot.margin = margin(t = 2, l = 2, b = 0, r = 10),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.justification = "left",
      legend.margin = margin(t = 0, b = 0, l = 0, r = 0)
    )
  }
}