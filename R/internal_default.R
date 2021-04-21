#' Get default font_size_title
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_title <- function(isMobile = FALSE) {
  if (isMobile == FALSE) return(11)
  else if (isMobile == TRUE) return(15)
}

#' Get default font_size_body
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_body <- function(isMobile = FALSE) {
  if (isMobile == FALSE) font_size_body <- 10
  else if (isMobile == TRUE) font_size_body <- 14
}

#' Get default palette.
#' 
#' @param n_col The number of col_var levels or values.
#' @return A numeric value.
#' @keywords internal
sv_pal <- function(n_col) {
  
  if(n_col == 1) viridis::viridis(4)[2]
  else if(n_col == 2) viridis::viridis(4)[c(2, 3)]
  else if(n_col > 2) viridis::viridis(n_col)
}

#' Calculate the breaks for a numeric vector.
#' 
#' @param y_var_vctr A numeric vector for the y scale from which to determine breaks from. 
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @return A vector of breaks
#' @keywords internal 
sv_y_numeric_breaks <- function(y_var_vctr, 
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
    if(y_trans == "log10") y_breaks <- c(1, y_breaks[y_breaks > 1])
    y_limits <- c(min(y_breaks), max(y_breaks))
  }
  else if (y_zero == FALSE) {
    if(y_trans != "log10") y_breaks <- pretty(y_var_vctr, n = y_pretty_n)
    if(y_trans == "log10") {
      y_breaks <- pretty(c(0, y_var_vctr), n = y_pretty_n) 
      y_breaks <- c(1, y_breaks[y_breaks > 1])
    }
  }
  return(y_breaks)
}

