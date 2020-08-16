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
#' @param vars_vctr A vector of quoted variables to include in the tooltip. Required input.
#' @param comma TRUE or FALSE of whether to convert numeric values to character values with comma seperators.
#' @return A vector of labels.
#' @export
#' @examples
#' library(dplyr)
#' 
#' plot_data <- slice_sample(ggplot2::diamonds, prop = 0.05) %>% 
#'   add_tip(vars_vctr = c("carat", "price"), comma = TRUE)
#' 
#' plot <- ggplot_scatter(data = plot_data, x_var = carat, y_var = price,
#'                        tip_var = tip_text,
#'                        title = "Diamond price by carat",
#'                        x_title = "Carat",
#'                        y_title = "Price ($US thousands)")
#' 
#' plotly::ggplotly(plot, tooltip = "text")
add_tip <- function(data, vars_vctr, comma = FALSE) {
  
  data <- data %>%
    dplyr::ungroup() 
  
  tip_text <- vector("character", 0)
  
  if(comma == TRUE) {
    for (i in length(vars_vctr):1) {
      
      temp <- data %>% 
        dplyr::select(vars_vctr[i]) 
      
      temp <- paste0(
        stringr::str_to_sentence(stringr::str_replace_all(colnames(temp), "_", " ")),
        ": ", 
        format(pull(temp, 1), big.mark = ","))
      
      tip_text <- paste(temp, tip_text, sep = "<br>")
    }
  }
  else if (comma == FALSE) {
    for (i in length(vars_vctr):1) {
      
      temp <- data %>% 
        dplyr::select(vars_vctr[i]) 
      
      temp <- paste0(
        stringr::str_to_sentence(stringr::str_replace_all(colnames(temp), "_", " ")),
        ": ", 
        pull(temp, 1))
      
      tip_text <- paste(temp, tip_text, sep = "<br>")
    }
  }
  
  data <- data %>%
    dplyr::mutate(tip_text = stringr::str_replace_all(tip_text, "NA", "Not available"))
  
  return(data)
}

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
