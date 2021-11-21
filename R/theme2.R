#' @title Create a simple clean theme. Defaults to no gridlines.
#'
#' @param family The font for all text to use. Defaults to "".
#' @param title_face The face of the title font. Defaults to "bold".
#' @param title_pal The colour palette for the title font. Defaults to "#000000".
#' @param title_size The size of the title font. Defaults to 11.
#' @param subtitle_face The face of the subtitle font. Defaults to "plain". 
#' @param subtitle_pal The colour palette for the subtitle font. Defaults to "#000000".
#' @param subtitle_size The size of the subtitle font. Defaults to 10.
#' @param body_face The face of all font other than the title or subtitle. Defaults to "plain".
#' @param body_pal The colour palette for all font other than the title or subtitle. Defaults to "#323232".
#' @param body_size The size of all font other than the title or subtitle. Defaults to 10.
#' @param axis_pal The colour palette for the axis. Defaults to "#323232".
#' @param axis_size The size of the axis. Defaults to 0.3.
#' @param ticks_pal The colour palette for the ticks. Defaults to "#323232".
#' @param ticks_size The size of the ticks. Defaults to 0.3.
#' @param grid_h_major_pal The colour palette for the horizontal major gridlines. Defaults to "#D3D3D3".   
#' @param grid_h_major_size The size of the horizontal major gridlines. Defaults to 0.
#' @param grid_v_major_pal The colour palette for the vertical major gridlines. Defaults to "#D3D3D3". 
#' @param grid_v_major_size The size of the vertical major gridlines. Defaults to 0.
#' @param grid_h_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_h_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#' @param grid_v_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_v_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#'
#' @return A ggplot theme.
#' @export
#' 
gg_theme <-
  function(family = "",
           title_face = "bold",
           title_pal = "#000000",
           title_size = 11,
           subtitle_face = "plain",
           subtitle_pal = "#000000",
           subtitle_size = 10,
           body_face = "plain",
           body_pal = "#323232",
           body_size = 10,
           axis_pal = "#323232",
           axis_size = 0.3,
           ticks_pal = "#323232",
           ticks_size = 0.3,
           grid_h_major_pal = "#D3D3D3",
           grid_h_major_size = 0,
           grid_v_major_pal = "#D3D3D3",
           grid_v_major_size = 0,
           grid_h_minor_pal = "#D3D3D3",
           grid_h_minor_size = 0,
           grid_v_minor_pal = "#D3D3D3",
           grid_v_minor_size = 0
           ) {
    list(
      theme(
        text = element_text(family = family, size = body_size, colour = body_pal, face = body_face),
        plot.title = element_text(family = family, size = title_size, colour = title_pal, face = title_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        plot.subtitle = element_text(family = family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        axis.title.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 10)),
        axis.title.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = margin(r = 10)),
        legend.title = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.99, margin = margin(t = body_size)),
        axis.text.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 2)),
        axis.text.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = margin(r = 2)),
        strip.text = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = margin(b = body_size / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major.x = element_line(colour = grid_v_major_pal, size = grid_v_major_size),
        panel.grid.major.y = element_line(colour = grid_h_major_pal, size = grid_h_major_size),
        panel.grid.minor.x = element_line(colour = grid_v_minor_pal, size = grid_v_minor_size),
        panel.grid.minor.y = element_line(colour = grid_h_minor_pal, size = grid_h_minor_size),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = axis_pal, size = axis_size),
        axis.ticks = element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = margin(t = 20, b = 20),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm"),
        legend.direction = "vertical",
        legend.box = NULL,
        complete = TRUE
      )
    )
  }

#' @title Create a simple clean theme. Defaults to having vertical major gridlines.
#'
#' @param family The font for all text to use. Defaults to "".
#' @param title_face The face of the title font. Defaults to "bold".
#' @param title_pal The colour palette for the title font. Defaults to "#000000".
#' @param title_size The size of the title font. Defaults to 11.
#' @param subtitle_face The face of the subtitle font. Defaults to "plain". 
#' @param subtitle_pal The colour palette for the subtitle font. Defaults to "#000000".
#' @param subtitle_size The size of the subtitle font. Defaults to 10.
#' @param body_face The face of all font other than the title or subtitle. Defaults to "plain".
#' @param body_pal The colour palette for all font other than the title or subtitle. Defaults to "#323232".
#' @param body_size The size of all font other than the title or subtitle. Defaults to 10.
#' @param axis_pal The colour palette for the axis. Defaults to "#323232".
#' @param axis_size The size of the axis. Defaults to 0.3.
#' @param ticks_pal The colour palette for the ticks. Defaults to "#323232".
#' @param ticks_size The size of the ticks. Defaults to 0.3.
#' @param grid_h_major_pal The colour palette for the horizontal major gridlines. Defaults to "#D3D3D3".   
#' @param grid_h_major_size The size of the horizontal major gridlines. Defaults to 0.
#' @param grid_v_major_pal The colour palette for the vertical major gridlines. Defaults to "#D3D3D3". 
#' @param grid_v_major_size The size of the vertical major gridlines. Defaults to 0.2.
#' @param grid_h_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_h_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#' @param grid_v_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_v_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#'
#' @return A ggplot theme.
#' @export
#' 
gg_theme_grid_v <-
  function(family = "",
           title_face = "bold",
           title_pal = "#000000",
           title_size = 11,
           subtitle_face = "plain",
           subtitle_pal = "#000000",
           subtitle_size = 10,
           body_face = "plain",
           body_pal = "#323232",
           body_size = 10,
           axis_pal = "#323232",
           axis_size = 0.3,
           ticks_pal = "#323232",
           ticks_size = 0.3,
           grid_h_major_pal = "#D3D3D3",
           grid_h_major_size = 0,
           grid_v_major_pal = "#D3D3D3",
           grid_v_major_size = 0.2,
           grid_h_minor_pal = "#D3D3D3",
           grid_h_minor_size = 0,
           grid_v_minor_pal = "#D3D3D3",
           grid_v_minor_size = 0
  ) {
    list(
      theme(
        text = element_text(family = family, size = body_size, colour = body_pal, face = body_face),
        plot.title = element_text(family = family, size = title_size, colour = title_pal, face = title_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        plot.subtitle = element_text(family = family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        axis.title.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 10)),
        axis.title.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = margin(r = 10)),
        legend.title = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.99, margin = margin(t = body_size)),
        axis.text.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 2)),
        axis.text.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = margin(r = 2)),
        strip.text = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = margin(b = body_size / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major.x = element_line(colour = grid_v_major_pal, size = grid_v_major_size),
        panel.grid.major.y = element_line(colour = grid_h_major_pal, size = grid_h_major_size),
        panel.grid.minor.x = element_line(colour = grid_v_minor_pal, size = grid_v_minor_size),
        panel.grid.minor.y = element_line(colour = grid_h_minor_pal, size = grid_h_minor_size),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = axis_pal, size = axis_size),
        axis.ticks = element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = margin(t = 20, b = 20),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm"),
        legend.direction = "vertical",
        legend.box = NULL,
        complete = TRUE
      )
    )
  }

#' @title Create a simple clean theme. Defaults to having horizontal major gridlines.
#'
#' @param family The font for all text to use. Defaults to "".
#' @param title_face The face of the title font. Defaults to "bold".
#' @param title_pal The colour palette for the title font. Defaults to "#000000".
#' @param title_size The size of the title font. Defaults to 11.
#' @param subtitle_face The face of the subtitle font. Defaults to "plain". 
#' @param subtitle_pal The colour palette for the subtitle font. Defaults to "#000000".
#' @param subtitle_size The size of the subtitle font. Defaults to 10.
#' @param body_face The face of all font other than the title or subtitle. Defaults to "plain".
#' @param body_pal The colour palette for all font other than the title or subtitle. Defaults to "#323232".
#' @param body_size The size of all font other than the title or subtitle. Defaults to 10.
#' @param axis_pal The colour palette for the axis. Defaults to "#323232".
#' @param axis_size The size of the axis. Defaults to 0.3.
#' @param ticks_pal The colour palette for the ticks. Defaults to "#323232".
#' @param ticks_size The size of the ticks. Defaults to 0.3.
#' @param grid_h_major_pal The colour palette for the horizontal major gridlines. Defaults to "#D3D3D3".   
#' @param grid_h_major_size The size of the horizontal major gridlines. Defaults to 0.2.
#' @param grid_v_major_pal The colour palette for the vertical major gridlines. Defaults to "#D3D3D3". 
#' @param grid_v_major_size The size of the vertical major gridlines. Defaults to 0.
#' @param grid_h_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_h_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#' @param grid_v_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_v_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#'
#' @return A ggplot theme.
#' @export
#' 
gg_theme_grid_h <-
  function(family = "",
           title_face = "bold",
           title_pal = "#000000",
           title_size = 11,
           subtitle_face = "plain",
           subtitle_pal = "#000000",
           subtitle_size = 10,
           body_face = "plain",
           body_pal = "#323232",
           body_size = 10,
           axis_pal = "#323232",
           axis_size = 0.3,
           ticks_pal = "#323232",
           ticks_size = 0.3,
           grid_h_major_pal = "#D3D3D3",
           grid_h_major_size = 0.2,
           grid_v_major_pal = "#D3D3D3",
           grid_v_major_size = 0,
           grid_h_minor_pal = "#D3D3D3",
           grid_h_minor_size = 0,
           grid_v_minor_pal = "#D3D3D3",
           grid_v_minor_size = 0
  ) {
    list(
      theme(
        text = element_text(family = family, size = body_size, colour = body_pal, face = body_face),
        plot.title = element_text(family = family, size = title_size, colour = title_pal, face = title_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        plot.subtitle = element_text(family = family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        axis.title.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 10)),
        axis.title.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = margin(r = 10)),
        legend.title = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.99, margin = margin(t = body_size)),
        axis.text.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 2)),
        axis.text.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = margin(r = 2)),
        strip.text = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = margin(b = body_size / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major.x = element_line(colour = grid_v_major_pal, size = grid_v_major_size),
        panel.grid.major.y = element_line(colour = grid_h_major_pal, size = grid_h_major_size),
        panel.grid.minor.x = element_line(colour = grid_v_minor_pal, size = grid_v_minor_size),
        panel.grid.minor.y = element_line(colour = grid_h_minor_pal, size = grid_h_minor_size),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = axis_pal, size = axis_size),
        axis.ticks = element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = margin(t = 20, b = 20),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm"),
        legend.direction = "vertical",
        legend.box = NULL,
        complete = TRUE
      )
    )
  }

#' @title Create a simple clean theme. Defaults to having horizontal and vertical major gridlines.
#'
#' @param family The font for all text to use. Defaults to "".
#' @param title_face The face of the title font. Defaults to "bold".
#' @param title_pal The colour palette for the title font. Defaults to "#000000".
#' @param title_size The size of the title font. Defaults to 11.
#' @param subtitle_face The face of the subtitle font. Defaults to "plain". 
#' @param subtitle_pal The colour palette for the subtitle font. Defaults to "#000000".
#' @param subtitle_size The size of the subtitle font. Defaults to 10.
#' @param body_face The face of all font other than the title or subtitle. Defaults to "plain".
#' @param body_pal The colour palette for all font other than the title or subtitle. Defaults to "#323232".
#' @param body_size The size of all font other than the title or subtitle. Defaults to 10.
#' @param axis_pal The colour palette for the axis. Defaults to "#323232".
#' @param axis_size The size of the axis. Defaults to 0.3.
#' @param ticks_pal The colour palette for the ticks. Defaults to "#323232".
#' @param ticks_size The size of the ticks. Defaults to 0.3.
#' @param grid_h_major_pal The colour palette for the horizontal major gridlines. Defaults to "#D3D3D3".   
#' @param grid_h_major_size The size of the horizontal major gridlines. Defaults to 0.2.
#' @param grid_v_major_pal The colour palette for the vertical major gridlines. Defaults to "#D3D3D3". 
#' @param grid_v_major_size The size of the vertical major gridlines. Defaults to 0.2.
#' @param grid_h_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_h_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#' @param grid_v_minor_pal The colour palette for the horizontal minor gridlines. Defaults to "#D3D3D3".
#' @param grid_v_minor_size The size of the horizontal minor gridlines. Defaults to 0.
#'
#' @return A ggplot theme.
#' @export
#' 
gg_theme_grid <-
  function(family = "",
           title_face = "bold",
           title_pal = "#000000",
           title_size = 11,
           subtitle_face = "plain",
           subtitle_pal = "#000000",
           subtitle_size = 10,
           body_face = "plain",
           body_pal = "#323232",
           body_size = 10,
           axis_pal = "#323232",
           axis_size = 0.3,
           ticks_pal = "#323232",
           ticks_size = 0.3,
           grid_h_major_pal = "#D3D3D3",
           grid_h_major_size = 0.2,
           grid_v_major_pal = "#D3D3D3",
           grid_v_major_size = 0.2,
           grid_h_minor_pal = "#D3D3D3",
           grid_h_minor_size = 0,
           grid_v_minor_pal = "#D3D3D3",
           grid_v_minor_size = 0
  ) {
    list(
      theme(
        text = element_text(family = family, size = body_size, colour = body_pal, face = body_face),
        plot.title = element_text(family = family, size = title_size, colour = title_pal, face = title_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        plot.subtitle = element_text(family = family, size = subtitle_size, colour = subtitle_pal, face = subtitle_face, hjust = 0.5, vjust = 1, margin = margin(b = body_size / 2)),
        axis.title.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 10)),
        axis.title.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, angle = 90, margin = margin(r = 10)),
        legend.title = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.99, margin = margin(t = body_size)),
        axis.text.x = element_text(family = family, size = body_size, colour = body_pal, face = body_face, margin = margin(t = 2)),
        axis.text.y = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 1, margin = margin(r = 2)),
        strip.text = element_text(family = family, size = body_size, colour = body_pal, face = body_face, hjust = 0.5, margin = margin(b = body_size / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major.x = element_line(colour = grid_v_major_pal, size = grid_v_major_size),
        panel.grid.major.y = element_line(colour = grid_h_major_pal, size = grid_h_major_size),
        panel.grid.minor.x = element_line(colour = grid_v_minor_pal, size = grid_v_minor_size),
        panel.grid.minor.y = element_line(colour = grid_h_minor_pal, size = grid_h_minor_size),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = axis_pal, size = axis_size),
        axis.ticks = element_line(colour = ticks_pal, size = ticks_size),
        legend.margin = margin(t = 20, b = 20),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(5, "mm"),
        legend.direction = "vertical",
        legend.box = NULL,
        complete = TRUE
      )
    )
  }

