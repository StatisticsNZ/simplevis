#' D3 palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a non-numeric (or non-ordererd) variable.
#' 
#' @param n_col The number of colours (excluding an NA colour).
#' 
#' @return A vector of hex codes.
#' @export 
pal_d3_reorder <- function(n_col) {
  
  c("#17BECFFF", "#BCBD22FF", "#8C564BFF", "#E377C2FF", "#2CA02CFF", "#1F77B4FF", "#FF7F0EFF", "#9467BDFF", "#D62728FF")[1:n_col]
}

#' Viridis palette reordered.
#' 
#' @description A function to retreive a vector of hex codes for a numeric (or ordererd) variable.
#' 
#' @param n_col The number of colours (excluding an NA colour).
#' 
#' @return A vector of hex codes.
#' @export 
pal_viridis_reorder <- function(n_col) {
  
  if(n_col == 1) viridis::viridis(4)[2]
  else if(n_col == 2) viridis::viridis(4)[c(2, 3)]
  else if(n_col >= 3) viridis::viridis(n_col)
}
