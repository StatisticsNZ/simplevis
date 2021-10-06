#' Identify the maximum decimal places in a numeric vector. 
#'
#' @param vctr A numeric vector. 
#'
#' @return a numeric value.
#' @keywords internal
sv_max_digits <- function(vctr) {
  if (length(vctr) == 0) return(numeric())
  vctr_nchr <- vctr %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  vctr_int <- floor(vctr) %>% abs() %>% nchar()
  vctr_nchr <- vctr_nchr - 1 - vctr_int
  vctr_nchr[vctr_nchr < 0] <- 0
  vctr_nchr <- max(vctr_nchr, na.rm = TRUE)
  return(vctr_nchr)
}
