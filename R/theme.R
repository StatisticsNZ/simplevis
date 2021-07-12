#' @title Theme for graphs with horizontal y gridlines.
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   theme_y_gridlines("Courier", 9, 7) +
#'   ggtitle("This is a title of a font family and size")
theme_y_gridlines <-
  function(font_family = "",
           font_size_title = 11,
           font_size_body = 10) {
    list(
      theme(
        text = element_text(family = font_family, size = font_size_body, colour = "#323232", face = "plain"),
        plot.title = element_text(size = font_size_title, face = "bold", colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        plot.subtitle = element_text(colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
        legend.title = element_text(hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(hjust = 0.99, margin = margin(t = font_size_body)),
        axis.text.x = element_text(size = font_size_body),
        axis.text.y = element_text(hjust = 1, size = font_size_body),
        strip.text = element_text(hjust = 0.5, margin = margin(b = font_size_body / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#D3D3D3", size = 0.2),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = "#323232", size = 0.3),
        axis.ticks = element_line(colour = "#323232", size = 0.3),
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

#' @title Theme for graphs with vertical x gridlines.
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   theme_x_gridlines("Courier", 9, 7) +
#'   ggtitle("This is a title of a font family and size")
theme_x_gridlines <-
  function(font_family = "",
           font_size_title = 11,
           font_size_body = 10) {
    list(
      theme(
        text = element_text(family = font_family, size = font_size_body, colour = "#323232", face = "plain"),
        plot.title = element_text(size = font_size_title, face = "bold", colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        plot.subtitle = element_text(colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
        legend.title = element_text(hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(hjust = 0.99, margin = margin(t = font_size_body)),
        axis.text.x = element_text(size = font_size_body),
        axis.text.y = element_text(hjust = 1, size = font_size_body),
        strip.text = element_text(hjust = 0.5, margin = margin(b = font_size_body / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = "#323232", size = 0.3),
        axis.ticks = element_line(colour = "#323232", size = 0.3),
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

#' @title Theme for graphs with x and y gridlines.
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   theme_xy_gridlines("Courier", 9, 7) +
#'   ggtitle("This is a title of a font family and size")
theme_xy_gridlines <-
  function(font_family = "",
           font_size_title = 11,
           font_size_body = 10) {
    list(
      theme(
        text = element_text(family = font_family, size = font_size_body, colour = "#323232", face = "plain"),
        plot.title = element_text(size = font_size_title, face = "bold", colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        plot.subtitle = element_text(colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
        legend.title = element_text(hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(hjust = 0.99, margin = margin(t = font_size_body)),
        axis.text.x = element_text(size = font_size_body),
        axis.text.y = element_text(hjust = 1, size = font_size_body),
        strip.text = element_text(hjust = 0.5, margin = margin(b = font_size_body / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#D3D3D3", size = 0.2),
        panel.grid.major.y = element_line(colour = "#D3D3D3", size = 0.2),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = "#323232", size = 0.3),
        axis.ticks = element_line(colour = "#323232", size = 0.3),
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

#' @title Theme for graphs with no gridlines.
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   theme_no_gridlines("Courier", 9, 7) +
#'   ggtitle("This is a title of a font family and size")
theme_no_gridlines <-
  function(font_family = "",
           font_size_title = 11,
           font_size_body = 10) {
    list(
      theme(
        text = element_text(family = font_family, size = font_size_body, colour = "#323232", face = "plain"),
        plot.title = element_text(size = font_size_title, face = "bold", colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        plot.subtitle = element_text(colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
        legend.title = element_text(hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(hjust = 0.99, margin = margin(t = font_size_body)),
        axis.text.x = element_text(size = font_size_body),
        axis.text.y = element_text(hjust = 1, size = font_size_body),
        strip.text = element_text(hjust = 0.5, margin = margin(b = font_size_body / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_line(colour = "#323232", size = 0.3),
        axis.ticks = element_line(colour = "#323232", size = 0.3),
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
#' @title Theme for maps.
#' @param font_family Font family to use. Defaults to "".
#' @param font_size_title Font size for the title text. Defaults to 11.
#' @param font_size_body Font size for all text other than the title. Defaults to 10.
#' @return A ggplot theme.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   theme_map("Courier", 9, 7) +
#'   ggtitle("This is a title of a font family and size")
theme_map <-
  function(font_family = "",
           font_size_title = 11,
           font_size_body = 10) {
    list(
      theme(
        text = element_text(family = font_family, size = font_size_body, colour = "#323232", face = "plain"),
        plot.title = element_text(size = font_size_title, face = "bold", colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        plot.subtitle = element_text(colour = "#000000", hjust = 0.5, vjust = 1, margin = margin(b = font_size_body / 2)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
        legend.title = element_text(hjust = 0, margin = margin(r = 20)),
        plot.caption = element_text(hjust = 0.99, margin = margin(t = font_size_body)),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(hjust = 0.5, margin = margin(b = font_size_body / 2)),
        legend.text = element_text(margin = margin(r = 10), hjust = 0),
        plot.margin = margin(t = 5, l = 5, b = 5, r = 20),
        plot.caption.position = "plot",
        panel.border = element_blank(),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_rect(colour = "transparent", fill = "transparent"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
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
    legend.justification = "left",
    legend.margin = margin(t = 0, b = 0, l = 0, r = 0)
  )
}