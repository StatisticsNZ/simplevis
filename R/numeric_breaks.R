#' Calculate the breaks for a y axis numeric vector.
#' 
#' @param var_vctr A numeric vector for the vertical scale from which to determine breaks from. 
#' @param balance Add balance to the vertical axis so that zero is in the centre of the vertical scale.
#' @param pretty_n The desired number of intervals on the vertical axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param trans A string specifying a transformation for the vertical axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param zero TRUE or FALSE of whether the minimum of the vertical scale is zero. Defaults to TRUE.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to NULL. 
#' @return A vector of breaks
#' @keywords internal
sv_numeric_breaks_v <- function(var_vctr, 
                                balance = FALSE, 
                                pretty_n = 5, 
                                trans = "identity", 
                                zero = TRUE) {
  
  min <- min(var_vctr, na.rm = TRUE)
  max <- max(var_vctr, na.rm = TRUE)
  min_max <- c(min, max)
  
  if (zero == TRUE) min_max <- c(0, min_max)
  if (balance == TRUE) min_max <- c(-min_max, min_max)
  
  breaks <- pretty(min_max, n = pretty_n)
  if(trans == "log10" | trans == "log") {
    if(breaks[1] == 0) breaks[1] <- 1
  }
  
  return(breaks)
}

#' Calculate the breaks for a horizontal axis numeric vector.
#' 
#' @param var_vctr A numeric vector for the horizontal scale from which to determine breaks from. 
#' @param balance Add balance to the y axis so that zero is in the centre of the horizontal scale.
#' @param pretty_n The desired number of intervals on the horizontal axis, as calculated by the pretty algorithm. Defaults to 6. 
#' @param trans A string specifying a transformation for the horizontal axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param zero TRUE or FALSE of whether the minimum of the horizontal scale is zero. Defaults to TRUE.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. 
#' @return A vector of breaks
#' @keywords internal
sv_numeric_breaks_h <- function(var_vctr, 
                                balance = FALSE, 
                                pretty_n = 6, 
                                trans = "identity", 
                                zero = TRUE,
                                mobile = FALSE) {
  
  min <- min(var_vctr, na.rm = TRUE)
  max <- max(var_vctr, na.rm = TRUE)
  min_max <- c(min, max)
  
  if (zero == TRUE) min_max <- c(0, min_max)
  if (balance == TRUE) min_max <- c(-min_max, min_max)
  
  breaks <- pretty(min_max, n = pretty_n)
  if(trans == "log10" | trans == "log") {
    if(breaks[1] == 0) breaks[1] <- 1 
  }
  
  if(mobile == TRUE) {
    breaks <- c(min(breaks), max(breaks))
    if (min(breaks) < 0 & max(breaks > 0)) breaks <- c(breaks[1], 0, breaks[2])
  }
  
  return(breaks)
}