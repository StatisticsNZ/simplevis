#' @title Create a simple theme for a graph. 
#'
#' @param family The font for all text to use. Defaults to "".

#' @param size_title The size of the title font. Defaults to 11.
#' @param size_subtitle The size of the subtitle font. Defaults to 10.
#' @param size_body The size of all font other than the title or subtitle. Defaults to 10.
#' @param size_axis The size of the axis. Defaults to 0.3.
#' @param size_ticks The size of the ticks. Defaults to 0.3.
#' @param size_gridlines The size of the vertical major gridlines. Defaults to 0.2.
#' @param face_title The face of the title font. Defaults to "bold".
#' @param face_subtitle The face of the subtitle font. Defaults to "plain". 
#' @param face_body The face of all font other than the title or subtitle. Defaults to "plain".
#' @param pal_title The colour palette for the title font. Defaults to "#000000".
#' @param pal_subtitle The colour palette for the subtitle font. Defaults to "#000000".
#' @param pal_body The colour palette for all font other than the title or subtitle. Defaults to "#323232".
#' @param pal_axis The colour palette for the axis. Defaults to "#323232".
#' @param pal_ticks The colour palette for the ticks. Defaults to "#323232".
#' @param pal_gridlines The colour palette for the vertical major gridlines. Defaults to "#D3D3D3". 
#' @param gridlines Whether gridlines are "horizontal", "vertical", "both" or "none".  
#'
#' @return A ggplot theme.
#' @export
#' 
gg_theme <-
  function(family = "",
           size_title = 11,
           size_subtitle = 10,
           size_body = 10,
           size_axis = 0.3,
           size_ticks = 0.3,
           size_gridlines = 0.2,
           face_title = "bold",
           face_subtitle = "plain",
           face_body = "plain",
           pal_title = "#000000",
           pal_subtitle = "#000000",
           pal_body = "#323232",
           pal_axis = "#323232",
           pal_ticks = "#323232",
           pal_gridlines = "#D3D3D3",
           gridlines = "horizontal") {
    list(
      if(gridlines == "horizontal") {
        theme(
          text = element_text(family = family, size = size_body, colour = pal_body, face = face_body),
          plot.title = element_text(family = family, size = size_title, colour = pal_title, face = face_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = family, size = size_subtitle, colour = pal_subtitle, face = face_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.5, margin = margin(b = size_body / 2)),
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
          panel.background = element_rect(colour = "transparent", fill = "transparent"),
          strip.background = element_rect(colour = "transparent", fill = "transparent"),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
      else if(gridlines == "vertical") {
        theme(
          text = element_text(family = family, size = size_body, colour = pal_body, face = face_body),
          plot.title = element_text(family = family, size = size_title, colour = pal_title, face = face_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = family, size = size_subtitle, colour = pal_subtitle, face = face_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.5, margin = margin(b = size_body / 2)),
          legend.text = element_text(margin = margin(r = 10), hjust = 0),
          plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
          plot.title.position = "panel",
          plot.caption.position = "plot",
          panel.border = element_blank(),
          panel.spacing = unit(2.5, "lines"),
          panel.grid.major.x = element_line(colour = pal_gridlines, size = size_gridlines),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(colour = "transparent", fill = "transparent"),
          strip.background = element_rect(colour = "transparent", fill = "transparent"),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
      else if(gridlines == "both") {
        theme(
          text = element_text(family = family, size = size_body, colour = pal_body, face = face_body),
          plot.title = element_text(family = family, size = size_title, colour = pal_title, face = face_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = family, size = size_subtitle, colour = pal_subtitle, face = face_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.5, margin = margin(b = size_body / 2)),
          legend.text = element_text(margin = margin(r = 10), hjust = 0),
          plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
          plot.title.position = "panel",
          plot.caption.position = "plot",
          panel.border = element_blank(),
          panel.spacing = unit(2.5, "lines"),
          panel.grid.major.x = element_line(colour = pal_gridlines, size = size_gridlines),
          panel.grid.major.y = element_line(colour = pal_gridlines, size = size_gridlines),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(colour = "transparent", fill = "transparent"),
          strip.background = element_rect(colour = "transparent", fill = "transparent"),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
      else if(gridlines == "none") {
        theme(
          text = element_text(family = family, size = size_body, colour = pal_body, face = face_body),
          plot.title = element_text(family = family, size = size_title, colour = pal_title, face = face_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          plot.subtitle = element_text(family = family, size = size_subtitle, colour = pal_subtitle, face = face_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
          axis.title.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 10)),
          axis.title.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, angle = 90, margin = margin(r = 10)),
          legend.title = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0, margin = margin(r = 20)),
          plot.caption = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.99, margin = margin(t = size_body)),
          axis.text.x = element_text(family = family, size = size_body, colour = pal_body, face = face_body, margin = margin(t = 2)),
          axis.text.y = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 1, margin = margin(r = 2)),
          strip.text = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.5, margin = margin(b = size_body / 2)),
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
          panel.background = element_rect(colour = "transparent", fill = "transparent"),
          strip.background = element_rect(colour = "transparent", fill = "transparent"),
          axis.line = element_line(colour = pal_axis, size = size_axis),
          axis.ticks = element_line(colour = pal_ticks, size = size_ticks),
          legend.margin = margin(t = 20, b = 20),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.height = unit(5, "mm"),
          legend.key.width = unit(5, "mm"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = "left",
          legend.box = NULL,
          complete = TRUE
        )
      }
    )
  }

#' @title Create a simple theme for a map. 
#'
#' @param family The font for all text to use. Defaults to "".
#' @param size_title The size of the title font. Defaults to 11.
#' @param size_subtitle The size of the subtitle font. Defaults to 10.
#' @param size_body The size of all font other than the title or subtitle. Defaults to 10.
#' @param face_title The face of the title font. Defaults to "bold".
#' @param face_subtitle The face of the subtitle font. Defaults to "plain". 
#' @param face_body The face of all font other than the title or subtitle. Defaults to "plain".
#' @param pal_title The colour palette for the title font. Defaults to "#000000".
#' @param pal_subtitle The colour palette for the subtitle font. Defaults to "#000000".
#' @param pal_body The colour palette for all font other than the title or subtitle. Defaults to "#323232".
#'
#' @return A ggplot theme.
#' @keywords internal
#' 
gg_theme_map <-
  function(family = "",
           size_title = 11,
           size_subtitle = 10,
           size_body = 10, 
           face_title = "bold",
           face_subtitle = "plain",
           face_body = "plain",
           pal_title = "#000000",
           pal_subtitle = "#000000",
           pal_body = "#323232"
           ) {
    list(
      theme(
        text = element_text(family = family, size = size_body, colour = pal_body, face = face_body),
        plot.title = element_text(family = family, size = size_title, colour = pal_title, face = face_title, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
        plot.subtitle = element_text(family = family, size = size_subtitle, colour = pal_subtitle, face = face_subtitle, hjust = 0.5, vjust = 1, margin = margin(b = size_body / 2)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.99, margin = margin(t = size_body)),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(family = family, size = size_body, colour = pal_body, face = face_body, hjust = 0.5, margin = margin(b = size_body / 2)),
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
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.margin = margin(t = 20, b = 20),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.justification = "left",
        legend.box = NULL,
        complete = TRUE
      )
    )
  }

#' @title Extra theme elements for ggplot2 graphs on mobile devices
#' @description Extra theme elements for ggplot2 graphs on mobile devices
#'
#' @return A ggplot theme.
#' @keywords internal
theme_mobile_extra <- function() {
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

#' @title Extra theme elements for ggplot2 maps on mobile devices
#' @description Extra theme elements for ggplot2 maps on mobile devices
#'
#' @return A ggplot theme.
#' @keywords internal
theme_mobile_extra_map <- function() {
  theme(
    plot.title.position = "plot",
    plot.margin = margin(t = 2, l = 2, b = 0, r = 10),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.justification = "left",
    legend.margin = margin(t = 0, b = 0, l = 0, r = 0)
  )
}