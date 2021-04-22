#' @title Convert column names to snake case and then to sentence case.
#' @description Convert colnames to snakecase and then to sentence case to present colnames visually.
#' @param data The number of digits to round the legend labels.
#' @return A numeric value.
#' @keywords internal
sv_colnames_to_present <- function(data) {
  
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
sv_labels_from_cuts <- function(cuts_vctr, col_labels_dp = 1) {
  
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

#' Get default font_size_title
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_title <- function(isMobile) {
  if (isMobile == FALSE) return(11)
  else if (isMobile == TRUE) return(15)
}

#' Get default font_size_body
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_body <- function(isMobile) {
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

#' Calculate the breaks for a y axis numeric vector.
#' 
#' @param y_var_vctr A numeric vector for the y scale from which to determine breaks from. 
#' @param y_balance Add balance to the y axis so that zero is in the centre of the y scale.
#' @param y_pretty_n The desired number of intervals on the y axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param y_trans A string specifying a transformation for the y axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param y_zero TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.
#' @return A vector of breaks
#' @keywords internal 
sv_y_numeric_breaks <- function(y_var_vctr, 
                                y_balance, 
                                y_pretty_n, 
                                y_trans, 
                                y_zero) {
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
    y_breaks <- pretty(y_var_vctr, n = y_pretty_n)
  }
  return(y_breaks)
}

#' Calculate the breaks for a x axis numeric vector.
#' 
#' @param x_var_vctr A numeric vector for the x scale from which to determine breaks from. 
#' @param x_balance Add balance to the y axis so that zero is in the centre of the x scale.
#' @param x_pretty_n The desired number of intervals on the x axis, as calculated by the pretty algorithm. Defaults to 5. 
#' @param x_trans A string specifying a transformation for the x axis scale, such as "log10" or "sqrt". Defaults to "identity".
#' @param x_zero TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to TRUE.
#' @return A vector of breaks
#' @keywords internal 
sv_x_numeric_breaks <- function(x_var_vctr, 
                                x_balance, 
                                x_pretty_n, 
                                x_trans, 
                                x_zero,
                                isMobile) {
  if (x_balance == TRUE) {
    x_var_vctr <- abs(x_var_vctr)
    x_var_vctr <- c(-x_var_vctr, x_var_vctr)
  }
  if (x_zero == TRUE) {
    x_breaks <- pretty(c(0, x_var_vctr), n = x_pretty_n)
    if(x_trans == "log10") x_breaks <- c(1, x_breaks[x_breaks > 1])
    x_limits <- c(min(x_breaks), max(x_breaks))
  }
  else if (x_zero == FALSE) {
    x_breaks <- pretty(x_var_vctr, n = x_pretty_n)
  }
  if(isMobile == TRUE) {
    x_breaks <- c(min(x_breaks), max(x_breaks))
    if (min(x_breaks) < 0 & max(x_breaks > 0)) x_breaks <- c(x_breaks[1], 0, x_breaks[2])
  }
  
  return(x_breaks)
}


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

