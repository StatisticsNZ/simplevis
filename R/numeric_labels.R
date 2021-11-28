#' #' Numeric labels
#' #'
#' #' @param labels 
#' #' @param label_digits 
#' #'
#' #' @return
#' #' @keywords internal
#' sv_numeric_labels <- function(labels = NULL, label_digits = NULL) {
#'     if (is.null(label_digits)) {
#'       labels <- scales::comma
#'     }
#'     else {
#'       labels <- scales::comma_format(accuracy = 10 ^ -label_digits)
#'     }
#'   }
