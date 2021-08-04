#' D3 palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a non-numeric (or non-ordererd) variable.
#' 
#' @param col_n The number of colours (excluding an NA colour).
#' 
#' @return A character vector of hex codes.
#' @export
#' @examples 
#' scales::show_col(pal_d3_reorder(9)) 
pal_d3_reorder <- function(col_n) {
  
  c("#17BECFFF", "#BCBD22FF", "#8C564BFF", "#E377C2FF", "#2CA02CFF", "#1F77B4FF", "#FF7F0EFF", "#9467BDFF", "#D62728FF")[1:col_n]
}

#' Viridis palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a numeric (or ordererd) variable.
#' 
#' @param col_n The number of colours (excluding an NA colour).
#' 
#' @return A character vector of hex codes.
#' @export 
#' @examples 
#' scales::show_col(pal_viridis_reorder(9)) 
pal_viridis_reorder <- function(col_n) {
  
  if(col_n == 1) viridis::viridis(4)[2]
  else if(col_n == 2) viridis::viridis(4)[c(2, 3)]
  else if(col_n >= 3) viridis::viridis(col_n)
}

#' NA palette.
#' 
#' @description A function to retreive a hex code for a colour to use for NA values.
#' 
#' @param pal The hex code or name of the NA colour. Defaults to "#7F7F7FFF".
#'
#' @return A character vector.
#' @export 
#' @examples 
#' scales::show_col(pal_na()) 
pal_na <- function(pal = "#7F7F7FFF") {
  return(pal)  
}
