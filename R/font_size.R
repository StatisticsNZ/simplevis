#' Get default font_size_title
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_title <- function(mobile) {
  return(11)
  # if (mobile == FALSE) return(11)
  # else if (mobile == TRUE) return(15)
}

#' Get default font_size_body
#' 
#' @return values for font sizes
#' @keywords internal
sv_font_size_body <- function(mobile) {
  # if (mobile == FALSE) return(10)
  # else if (mobile == TRUE) return(11)
  return(10) 
}
