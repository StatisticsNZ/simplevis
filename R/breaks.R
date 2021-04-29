#' Calculate the breaks for a y axis numeric vector.
#' 
#' @param y_var_vctr A numeric vector for the y scale from which to determine breaks from. 
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @return A vector of breaks
#' @export
y_numeric_breaks <- function(y_var_vctr, 
                                y_balance = FALSE, 
                                y_pretty_n = 5, 
                                y_trans = "identity", 
                                y_zero = TRUE) {
  if (y_balance == TRUE) {
    y_var_vctr <- abs(y_var_vctr)
    y_var_vctr <- c(-y_var_vctr, y_var_vctr)
  }
  if (y_zero == TRUE) {
      y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n)
      y_limits <- c(min(y_breaks), max(y_breaks))
      if(y_trans == "log10" | y_trans == "log") {
        y_breaks[1] <- 0.1 
        y_limits[1] <- 0.1
      }
  }
  else if (y_zero == FALSE) {
    y_breaks <- pretty(y_var_vctr, n = y_pretty_n)
    y_limits <- c(min(y_breaks), max(y_breaks))
    if(y_trans == "log10" | y_trans == "log") {
      if(y_breaks[1] == 0) y_breaks[1] <- 0.1 
      if(y_limits[1] == 0) y_limits[1] <- 0.1 
    }
  }

  return(y_breaks)
}

#' Calculate the breaks for a x axis numeric vector.
#' 
#' @param x_var_vctr A numeric vector for the x scale from which to determine breaks from. 
#' @param x_balance Add balance to the y axis so that zero is in the centre of the x scale.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 6. 
#' @param x_trans A string specifying a transformation for the x axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param mobile Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within an app with the mobileDetect function, then use mobile = input$mobile.
#' @return A vector of breaks
#' @export
x_numeric_breaks <- function(x_var_vctr, 
                                x_balance = FALSE, 
                                x_pretty_n = 6, 
                                x_trans = "identity", 
                                x_zero = TRUE,
                                mobile = FALSE) {
  if (x_balance == TRUE) {
    x_var_vctr <- abs(x_var_vctr)
    x_var_vctr <- c(-x_var_vctr, x_var_vctr)
  }
  if (x_zero == TRUE) {
    x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if(x_trans == "log10" | x_trans == "log") {
      x_breaks[1] <- 0.1 
      x_limits[1] <- 0.1
    }
  }
  else if (x_zero == FALSE) {
    x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
    x_limits <- c(min(x_breaks), max(x_breaks))
    if(x_trans == "log10" | x_trans == "log") {
      if(x_breaks[1] == 0) x_breaks[1] <- 0.1 
      if(x_limits[1] == 0) x_limits[1] <- 0.1 
    }
  }
  if(mobile == TRUE) {
    x_breaks <- c(min(x_breaks), max(x_breaks))
    if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
  }
  
  return(x_breaks)
}
