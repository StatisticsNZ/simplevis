#' @title Remove plotly buttons from the mode bar, other than the camera.
#' @description Remove plotly buttons from the mode bar, other than the camera and plotly logo.
#' @param plotly A plotly object. Required input.
#' @param logo TRUE or FALSE of whether to display the plotly logo. Defaults to FALSE.
#' @export
#' @examples
#' plot_data <- dplyr::sample_frac(ggplot2::diamonds, 0.05)
#' 
#' plot <- gg_point(data = plot_data, x_var = carat, y_var = price)
#' 
#' plotly::ggplotly(plot) %>% 
#'    plotly_camera()
plotly_camera <- function(plotly, logo = FALSE){
  plotly::config(plotly,
                 modeBarButtonsToRemove = list(
                   "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d","resetScale2d", "hoverClosestCartesian",
                   "hoverCompareCartesian", "sendDataToCloud", "toggleHover", "resetViews", "toggleSpikelines",
                   "resetViewMapbox", "toggleSpikelines", "resetViewMapbox", "lasso2d", "select2d"
                 ),
                 displaylogo = logo) %>% 
    plotly::style(hoverlabel = list(align = "left"))
}

#' Change colour legend elements order.
#'
#' @param plotly A plotly object. Required input.
#' @param rev TRUE or FALSE of whether to reverse the order of elements.
#' @param order A numeric vector specifying the order of elements. 
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' plot_data <- dplyr::sample_frac(ggplot2::diamonds, 0.05) 
#' 
#' plot <- gg_point_col(data = plot_data, x_var = carat, y_var = price, col_var = color)
#' 
#' plotly::ggplotly(plot)
#' 
#' plotly::ggplotly(plot) %>% 
#'   plotly_col_legend(rev = TRUE)
#' 
#' plotly::ggplotly(plot) %>% 
#'   plotly_col_legend(order = c(2, 1, 3:7))
plotly_col_legend <- function(plotly, rev = FALSE, order = NULL) {
  if(rev == TRUE & !is.null(order)) stop("Please either reverse or provide a specified order, but not both")
  
  if(rev == TRUE) {
    n_labels <- length(plotly$x$data)
    plotly$x$data[1:n_labels] <- plotly$x$data[n_labels:1]
  }
  else if(!is.null(order)) {
    n_labels <- length(plotly$x$data)
    plotly$x$data[1:n_labels] <- plotly$x$data[order]
  }
  return(plotly)
}  
