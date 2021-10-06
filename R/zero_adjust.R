#' Automatically adjust x_zero and x_zero_line if necessary
#' @param description Generate a list of x_zero and x_zero_line elements that are adjusted if necessary.
#'
#' @param x_var_vctr A vector of values for the x scale.
#' @param x_balance Add balance to the x axis so that zero is in the centre of the x scale. Only applicable where facet_scales equals "fixed" or "free_y".
#' @param x_zero TRUE or FALSE whether the minimum of the x scale is zero. Defaults to TRUE.
#' @param x_zero_line TRUE or FALSE whether to add a zero reference line to the x axis. Defaults to NULL, which is TRUE if there are positive and negative values in x_var. Otherwise it is FALSE.
#'
#' @return A list with first element x_zero (TRUE or FALSE) and second element x_zero_line (TRUE or FALSE)
#' @keywords internal
sv_x_zero_adjust <- function(x_var_vctr, x_balance, x_zero, x_zero_line) {
  
  x_above_and_below_zero <- ifelse(min(x_var_vctr, na.rm = TRUE) < 0 & max(x_var_vctr, na.rm = TRUE) > 0, TRUE, FALSE)
  
  if(x_above_and_below_zero == TRUE) x_zero <- FALSE
  
  if(is.null(x_zero_line)) {
    if(x_above_and_below_zero == TRUE | x_balance == TRUE) x_zero_line <- TRUE
    else(x_zero_line <- FALSE)
  }
  return(list(x_zero, x_zero_line))
}

#' Automatically adjust y_zero and y_zero_line if necessary
#' @param description Generate a list of y_zero and y_zero_line elements that are adjusted if necessary.
#'
#' @param y_var_vctr A vector of values for the y scale.
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale. Only applicable where facet_scales equals "fixed" or "free_y".
#' @param y_zero TRUE or FALSE whether the minimum of the y scale is zero. Defaults to TRUE.
#' @param y_zero_line TRUE or FALSE whether to add a zero reference line to the y axis. Defaults to NULL, which is TRUE if there are positive and negative values in y_var. Otherwise it is FALSE.
#'
#' @return A list with first element y_zero (TRUE or FALSE) and second element y_zero_line (TRUE or FALSE)
#' @keywords internal
sv_y_zero_adjust <- function(y_var_vctr, y_balance, y_zero, y_zero_line) {
  
  y_above_and_below_zero <- ifelse(min(y_var_vctr, na.rm = TRUE) < 0 & max(y_var_vctr, na.rm = TRUE) > 0, TRUE, FALSE)
  
  if(y_above_and_below_zero == TRUE) y_zero <- FALSE
  
  if(is.null(y_zero_line)) {
    if(y_above_and_below_zero == TRUE | y_balance == TRUE) y_zero_line <- TRUE
    else(y_zero_line <- FALSE)
  }
  return(list(y_zero, y_zero_line))
}
