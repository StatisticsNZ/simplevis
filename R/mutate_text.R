#' @title Add a quick tooltip text column to data.
#' @description Add a column of tooltip text which is automatically created based on column names and values. 
#' @param data A tibble or dataframe. Required input.
#' @param vars_vctr A vector of quoted variables to include in the tooltip. Defaults to NULL, which adds all variables in.
#' @param numeric_format A function to format all numeric variables within the tooltip text column. Defaults to non-scientific. Use function(x) x to leave as is.
#' 
#' @return A tibble or data frame with an additional column called text.
#' @export
#' @examples
#' library(simplevis)
#' library(dplyr)
#' 
#' plot_data <- slice_sample(ggplot2::diamonds, prop = 0.05) %>% 
#'   mutate_text(vars_vctr = c("carat", "price"))
#' 
#' plot <- gg_point(data = plot_data, 
#'                  x_var = carat, 
#'                  y_var = price, 
#'                  text_var = text, 
#'                  title = "Diamond price by carat", 
#'                  x_title = "Carat", 
#'                  y_title = "Price ($US thousands)")
#' 
#' plotly::ggplotly(plot, tooltip = "text")
#' 
mutate_text <- function(data, 
                        vars_vctr = NULL, 
                        numeric_format = function(x) prettyNum(x, big.mark = "", scientific = FALSE)) {
  
  data <- data %>% 
    dplyr::ungroup() 
  
  class <- class(data)[1]
  
  if (is.null(vars_vctr)) {
    if(class == "sf") {
      vars_vctr <- colnames(data)[colnames(data) != "geometry"]
    } else if(class != "sf") {
      vars_vctr <- colnames(data)
    }
  }
  
  temp <- data %>% 
    dplyr::select(vars_vctr) %>% 
    dplyr::mutate_if(.predicate = is.numeric, .funs = numeric_format)
  
  text <- vector("character", 0)
  
  for (i in length(vars_vctr):1) {
    
    temp2 <- temp %>% 
      dplyr::select(vars_vctr[i])  
    
    if(class == "sf") temp2 <- temp2 %>%
        sf::st_drop_geometry()
    
    temp2 <- paste0(
      snakecase::to_sentence_case(colnames(temp2)),
      ": ", 
      dplyr::pull(temp2, 1))
    
    text <- paste(temp2, text, sep = "<br>")
  }
  
  data <- data %>%
    dplyr::mutate(text = text)
  
  if(class == "sf") {
    data <- data %>%
      dplyr::relocate(text, .before = "geometry")
  }
  
  return(data)
}
