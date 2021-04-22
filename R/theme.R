#' @title Extra theme elements for ggplot2 graphs on mobile devices
#' @description Extra theme elements for ggplot2 graphs on mobile devices
#'
#' @return A ggplot theme.
#' @keywords internal
theme_mobile_graph <- function() {
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
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
theme_mobile_map <- function() {
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(t = 2, l = 2, b = 0, r = 10),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(t = 0, b = 0, l = 0, r = 0)
  ) 
}
