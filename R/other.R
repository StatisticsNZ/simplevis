# other

#' @title Numeric legend labels.
#' @description Pretty numeric legend labels.
#' @param bin_cuts A numeric vector of bin cuts from which to create a vector of legend labels.
#' @param legend_digits The number of digits to round the legend labels.
#' @return A vector of labels.
#' @export
numeric_legend_labels <- function(bin_cuts, legend_digits = 1) {
  labels <- vector("character", 0)
  bin_cuts_no <- length(bin_cuts)
  bin_cuts <-
    sprintf(paste0("%.", legend_digits, "f"),
            round(bin_cuts, legend_digits))

  if (bin_cuts_no == 2) {
    labels <- c("Feature")
  }
  else if (bin_cuts_no == 3) {
    labels <- c(paste0("<", bin_cuts[2]), paste0("\u2265", bin_cuts[2]))
  }
  else if (bin_cuts_no > 3) {
    for (i in 2:(length(bin_cuts) - 2)) {
      temp <- paste0(bin_cuts[i], "\u2013", bin_cuts[i + 1])
      labels <- c(labels, temp)
    }
    labels <-
      c(paste0("<", bin_cuts[2]),
        labels,
        paste0("\u2265", bin_cuts[length(bin_cuts) - 1]))
  }
}

#' @title A4 useable width.
#' @description The width of useable space within an a4 sheet.
#' @return A numeric value.
#' @export
a4_width_mm <- 170

#' @title A4 useable height.
#' @description The height of useable space within an a4 sheet.
#' @return A numeric value.
#' @export
a4_height_mm <- 257

#' @title Convert column names to sentence case.
#' @description A function to convert colnames to snakecase and then to sentence case to be used in functions for making hover values.
#' @param data The number of digits to round the legend labels.
#' @return A numeric value.
#' @export
sentence_spaced_colnames <- function(data) {
  data <- janitor::clean_names(data) 
  colnames(data) <-  stringr::str_replace_all(stringr::str_to_sentence(colnames(data)), "_", " ")
  return(data)
}

#' @title Remove plotly buttons from the mode bar, other than the camera and plotly logo.
#' @description Remove plotly buttons from the mode bar, other than the camera and plotly logo.
#' @param plotly A plotly object.
#' @param logo TRUE or FALSE of whether to display the plotly logo. Defaults to FALSE.
#' @export
#' @examples
#' plot_data <- dplyr::sample_frac(ggplot2::diamonds, 0.05)
#' 
#' plot <- ggplot_scatter(data = plot_data, x_var = carat, y_var = price)
#' 
#' plotly::ggplotly(plot, tooltip = "text") %>% 
#'    plotly_remove_buttons()
plotly_remove_buttons <- function(plotly, logo = FALSE){
  plotly::config(plotly,
    modeBarButtonsToRemove = list(
      "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d","resetScale2d", "hoverClosestCartesian",
      "hoverCompareCartesian", "sendDataToCloud", "toggleHover", "resetViews", "toggleSpikelines",
      "resetViewMapbox", "toggleSpikelines", "resetViewMapbox", "lasso2d", "select2d"
    ),
    displaylogo = logo
  )
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
#'    plotly_reverse_legend()
plotly_reverse_legend <- function(plotly) {
  n_labels <- length(plotly$x$data)
  plotly$x$data[1:n_labels] <- plotly$x$data[n_labels:1]
  plotly
}  

#' @title Order plotly legend elements.
#' @description Order plotly legend elements.
#' @param plotly A plotly object.
#' @param order_vector An order vector numerically specifying the order of elements. Required input.
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
#'    plotly_order_legend(c(4, 1:3, 5:8))
plotly_order_legend <- function(plotly, order_vector = NULL) {
  if(is.null(order_vector)) stop("An order vector must be provided")
  n_labels <- length(plotly$x$data)
  plotly$x$data[1:n_labels] <- plotly$x$data[order_vector]
  plotly
}

#' @title Colour palette for categorical variables.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
pal_snz <- c("#085c75", "#d2ac2f", "#ae4e51", "#35345d", "#76a93f", "#6f2e38", "#0d94a3", "#dd6829", "#1a6e5b")

#' @title Colour palette for categorical variables for points.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
pal_point_set1 <- c("#377EB8", "#A65628", "#F781BF", "#4DAF4A", "#FF7F00", "#984EA3", "#FFFF33", "#E41A1C", "#999999") #from Set1, 9col


