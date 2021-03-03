#' @title Remove plotly buttons from the mode bar, other than the camera.
#' @description Remove plotly buttons from the mode bar, other than the camera and plotly logo.
#' @param plotly A plotly object.
#' @param logo TRUE or FALSE of whether to display the plotly logo. Defaults to FALSE.
#' @export
#' @examples
#' plot_data <- dplyr::sample_frac(ggplot2::diamonds, 0.05)
#' 
#' plot <- ggplot_point(data = plot_data, x_var = carat, y_var = price)
#' 
#' plotly::ggplotly(plot, tooltip = "text") %>% 
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

#' @title Reverse plotly legend elements.
#' @description Reverse plotly legend elements.
#' @param plotly A plotly object.
#' @export
#' @examples
#' plot_data <- ggplot2::diamonds %>%
#'    dplyr::mutate(cut = stringr::str_to_sentence(cut)) %>%
#'    dplyr::group_by(cut, clarity) %>%
#'    dplyr::summarise(average_price = mean(price)) %>%
#'    dplyr::mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
#'    dplyr::ungroup()
#'    
#' plot <- ggplot_hbar_col(data = plot_data, 
#'                        x_var = average_price_thousands, 
#'                        y_var = cut, 
#'                        col_var = clarity, 
#'                        legend_ncol = 4,
#'                        title = "Average diamond price by cut and clarity", 
#'                        x_title = "Average price ($US thousands)", 
#'                        y_title = "Cut")
#' 
#' plotly::ggplotly(plot, tooltip = "text")
#' 
#' plotly::ggplotly(plot, tooltip = "text") %>% 
#'    plotly_legend_rev()
plotly_legend_rev <- function(plotly) {
  n_labels <- length(plotly$x$data)
  plotly$x$data[1:n_labels] <- plotly$x$data[n_labels:1]
  plotly
}  

#' @title Order plotly legend elements.
#' @description Order plotly legend elements.
#' @param plotly A plotly object.
#' @param numeric_order A vector specifying the numeric order of elements. Required input.
#' @export
#' @examples
#' plot_data <- ggplot2::diamonds %>%
#'    dplyr::mutate(cut = stringr::str_to_sentence(cut)) %>%
#'    dplyr::group_by(cut, clarity) %>%
#'    dplyr::summarise(average_price = mean(price)) %>%
#'    dplyr::mutate(average_price_thousands = round(average_price / 1000, 1)) %>%
#'    dplyr::ungroup()
#'    
#' plot <- ggplot_hbar_col(data = plot_data, 
#'                        x_var = average_price_thousands, 
#'                        y_var = cut, 
#'                        col_var = clarity, 
#'                        legend_ncol = 4,
#'                        title = "Average diamond price by cut and clarity", 
#'                        x_title = "Average price ($US thousands)", 
#'                        y_title = "Cut")
#' 
#' plotly::ggplotly(plot, tooltip = "text")
#' 
#' plotly::ggplotly(plot, tooltip = "text") %>% 
#'    plotly_legend_order(c(4, 1:3, 5:8))
plotly_legend_order <- function(plotly, numeric_order = NULL) {
  if(is.null(numeric_order)) stop("A numeric order vector must be provided")
  n_labels <- length(plotly$x$data)
  plotly$x$data[1:n_labels] <- plotly$x$data[numeric_order]
  plotly
}
