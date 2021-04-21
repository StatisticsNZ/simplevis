#' @title Convert column names to snake case and then to sentence case.
#' @description Convert colnames to snakecase and then to sentence case to present colnames visually.
#' @param data The number of digits to round the legend labels.
#' @return A numeric value.
#' @keywords internal
colnames_to_snake_to_sentence <- function(data) {
  
  data <- janitor::clean_names(data) 
  
  colnames(data) <-  stringr::str_replace_all(stringr::str_to_sentence(colnames(data)), "_", " ")
  
  return(data)
}

#' @title Numeric legend labels.
#' @description Pretty numeric legend labels.
#' @param cuts_vctr A numeric vector of bin cuts from which to create a vector of legend labels.
#' @param col_labels_dp The number of digits to round the legend labels.
#' @return A vector of labels.
#' @keywords internal
legend_labels_from_cuts <- function(cuts_vctr, col_labels_dp = 1) {
  
  labels <- vector("character", 0)
  cuts_vctr_no <- length(cuts_vctr)
  cuts_vctr <-
    sprintf(paste0("%.", col_labels_dp, "f"),
            round(cuts_vctr, col_labels_dp))
  
  if (cuts_vctr_no == 2) {
    labels <- c("Feature")
  }
  else if (cuts_vctr_no == 3) {
    labels <- c(paste0("<", cuts_vctr[2]), paste0("\u2265", cuts_vctr[2]))
  }
  else if (cuts_vctr_no > 3) {
    for (i in 2:(length(cuts_vctr) - 2)) {
      temp <- paste0(cuts_vctr[i], "\u2013", cuts_vctr[i + 1])
      labels <- c(labels, temp)
    }
    labels <-
      c(paste0("<", cuts_vctr[2]),
        labels,
        paste0("\u2265", cuts_vctr[length(cuts_vctr) - 1]))
  }
}

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

