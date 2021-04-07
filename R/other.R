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
#'                        text_var = text,
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
  
  text <- vector("character", 0)
  
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
      
      text <- paste(temp, text, sep = "<br>")
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
      
      text <- paste(temp, text, sep = "<br>")
    }
  }
  
  data <- data %>%
    dplyr::mutate(text = stringr::str_replace_all(text, "NA", "Not available"))
  
  return(data)
}

