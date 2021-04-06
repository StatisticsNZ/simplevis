# other

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

#' @title Add a quick tooltip text column to data.
#' @description Add a column of tooltip text which is automatically created based on column names and values. 
#' @param data A tibble or dataframe. Required input.
#' @param vars_vctr A vector of quoted variables to include in the tooltip. Defaults to NULL, which adds all variables in.
#' @param comma TRUE or FALSE of whether to convert numeric values to character values with comma seperators.
#' @return A vector of labels.
#' @export
#' @examples
#' library(dplyr)
#' 
#' plot_data <- slice_sample(ggplot2::diamonds, prop = 0.05) %>% 
#'   mutate_text(c("carat", "price"), comma = TRUE)
#' 
#' plot <- ggplot_point(data = plot_data, x_var = carat, y_var = price,
#'                        text_var = tip_text,
#'                        title = "Diamond price by carat",
#'                        x_title = "Carat",
#'                        y_title = "Price ($US thousands)")
#' 
#' plotly::ggplotly(plot, tooltip = "text")
mutate_text <- function(data, vars_vctr = NULL, comma = FALSE) {
  
  data <- data %>% ungroup()
  
  class <- class(data)[1]
  
  if(is.null(vars_vctr)) {
    if(class == "sf") vars_vctr <- colnames(data)[colnames(data) != "geometry"]
    else if(class != "sf") vars_vctr <- colnames(data)
  }
  
  tip_text <- vector("character", 0)
  
  if(comma == TRUE) {
    for (i in length(vars_vctr):1) {
      
      temp <- data %>% 
        dplyr::select(vars_vctr[i]) 
      
      if(class == "sf") temp <- temp %>% 
          sf::st_drop_geometry()
      
      temp <- paste0(
        stringr::str_to_sentence(stringr::str_replace_all(colnames(temp), "_", " ")),
        ": ", 
        format(dplyr::pull(temp, 1), big.mark = ","))
      
      tip_text <- paste(temp, tip_text, sep = "<br>")
    }
  }
  else if (comma == FALSE) {
    for (i in length(vars_vctr):1) {
      
      temp <- data %>% 
        dplyr::select(vars_vctr[i]) 
      
      if(class == "sf") temp <- temp %>% 
          sf::st_drop_geometry()
      
      temp <- paste0(
        stringr::str_to_sentence(stringr::str_replace_all(colnames(temp), "_", " ")),
        ": ", 
        dplyr::pull(temp, 1))
      
      tip_text <- paste(temp, tip_text, sep = "<br>")
    }
  }
  
  data <- data %>%
    dplyr::mutate(tip_text = stringr::str_replace_all(tip_text, "NA", "Not available"))
  
  return(data)
}

#' @title Numeric legend labels.
#' @description Pretty numeric legend labels.
#' @param cuts_vctr A numeric vector of bin cuts from which to create a vector of legend labels.
#' @param col_digits The number of digits to round the legend labels.
#' @return A vector of labels.
#' @keywords internal
legend_labels_from_cuts <- function(cuts_vctr, col_digits = 1) {
  
  labels <- vector("character", 0)
  cuts_vctr_no <- length(cuts_vctr)
  cuts_vctr <-
    sprintf(paste0("%.", col_digits, "f"),
            round(cuts_vctr, col_digits))
  
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

#' @title Convert column names to sentence case.
#' @description A function to convert colnames to snakecase and then to sentence case to be used in functions for making hover values.
#' @param data The number of digits to round the legend labels.
#' @return A numeric value.
#' @keywords internal
sentence_spaced_colnames <- function(data) {
  data <- janitor::clean_names(data) 
  colnames(data) <-  stringr::str_replace_all(stringr::str_to_sentence(colnames(data)), "_", " ")
  return(data)
}
