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

#' @title Numeric legend labels.
#' @description Pretty numeric legend labels.
#' @param cuts_vctr A numeric vector of bin cuts from which to create a vector of legend labels.
#' @param labels_dp The number of digits to round the legend labels.
#' @return A vector of labels.
#' @keywords internal
sv_numeric_bin_labels <- function(cuts_vctr, labels_dp = 1) {
  
  labels <- vector("character", 0)
  cuts_vctr_no <- length(cuts_vctr)
  cuts_vctr <-
    sprintf(paste0("%.", labels_dp, "f"),
            round(cuts_vctr, labels_dp))
  
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


#' Identify the maximum decimal places in a numeric vector. 
#'
#' @param vctr A numeric vector. 
#'
#' @return a numeric value.
#' @keywords internal
sv_max_dp <- function(vctr) {
  if (length(vctr) == 0) return(numeric())
  vctr_nchr <- vctr %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  vctr_int <- floor(vctr) %>% abs() %>% nchar()
  vctr_nchr <- vctr_nchr - 1 - vctr_int
  vctr_nchr[vctr_nchr < 0] <- 0
  vctr_nchr <- max(vctr_nchr, na.rm = TRUE)
  return(vctr_nchr)
}

#' Get default font_size_title
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_title <- function(mobile) {
  if (mobile == FALSE) return(11)
  else if (mobile == TRUE) return(15)
}

#' Get default font_size_body
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_body <- function(mobile) {
  if (mobile == FALSE) font_size_body <- 10
  else if (mobile == TRUE) font_size_body <- 14
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


