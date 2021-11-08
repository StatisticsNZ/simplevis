#' @title Convert numeric or interval cuts to simple and pretty labels.
#' @param cuts A vector or numeric or character interval cuts.
#' @param digits If cuts are numeric, the number of decimal places to round labels to.
#' @param right_closed If cuts are numeric, TRUE or FALSE of whether intervals are to be right-closed. Defaults to TRUE.
#' @return A vector of character labels.
#' @export 
#' @examples 
#' library(simplevis)
#' 
#' interval_labels(c(0, 0.1, 3, 4.1, 7, 100, Inf))
#' 
#' interval_labels(c("(0, 10]", "(10, 50]", "(50, 100]"))
#' 
#' interval_labels(c("[0, 10)", "[10, 50)", "[50, 100)"))
#' 
interval_labels <- function(cuts, digits = NULL, right_closed = TRUE) {
  
  if (is.numeric(cuts)) {
    if (is.null(digits)) {
      cuts <- scales::comma(cuts)  
    } else cuts <-   scales::comma(cuts, accuracy = 10 ^ -digits)

    labels <- vector("character", 0)
    
    cuts_no <- length(cuts)
    
    sign1 <- ifelse(right_closed == TRUE, "\u2264", "<")  
    sign2 <- ifelse(right_closed == TRUE, ">", "\u2265")  
    
    if (cuts_no == 2) {
      labels <- c("Feature")
    }
    else if (cuts_no == 3) {
      labels <- c(paste0(sign1, cuts[2]), paste0(sign2, cuts[2]))
    }
    else if (cuts_no > 3) {
      for (i in 2:(length(cuts) - 2)) {
        temp <- paste0(cuts[i], "\u2013", cuts[i + 1])
        labels <- c(labels, temp)
      }
      
      labels <- 
        c(paste0(sign1, cuts[2]),
          labels,
          paste0(sign2, cuts[length(cuts) - 1]))
    }
  } 
  else {
    right_closed <- ifelse(stringr::str_sub(cuts[1], -1L, -1L) == "]", TRUE, FALSE)
    
    labels <- stringr::str_replace_all(stringr::str_replace_all(cuts, ", ", "\u2013"), "\\[|\\]|\\)|\\(", "")
    
    sign1 <- ifelse(right_closed == TRUE, "\u2264", "<")  
    sign2 <- ifelse(right_closed == TRUE, ">", "\u2265")  
    
    labels[1] <- glue::glue("{sign1}{stringr::word(labels[2], sep = '\u2013')}")
    
    if (stringr::str_detect(tidyr::replace_na(labels[length(labels)], "NA"), "NA")) {    
      labels[length(labels) - 1] <- glue::glue("{sign2}{stringr::word(labels[length(labels) - 1], 1, sep = '\u2013')}")
    } else {
      labels[length(labels)] <- glue::glue("{sign2}{stringr::word(labels[length(labels)], 1, sep = '\u2013')}")
    }
  }
  
  return(labels)
}

#' @title Convert numeric bin cuts to simple and pretty labels.
#' @param cuts A vector of numeric cuts.
#' @param digits If cuts are numeric, the number of decimal places to round labels to.
#' @param right_closed If cuts are numeric, TRUE or FALSE of whether intervals are to be right-closed. Defaults to TRUE.
#' @return A vector of character labels.
#' @keywords internal
#' 
sv_interval_labels_num <- function(cuts, digits = NULL, right_closed = TRUE) {
  
  if (is.null(digits)) {
    cuts <- scales::comma(cuts)  
  } else cuts <-   scales::comma(cuts, accuracy = 10 ^ -digits)
    
  labels <- vector("character", 0)
  
  cuts_no <- length(cuts)
  
  sign1 <- ifelse(right_closed == TRUE, "\u2264", "<")  
  sign2 <- ifelse(right_closed == TRUE, ">", "\u2265")  
  
  if (cuts_no == 2) {
    labels <- c("Feature")
  }
  else if (cuts_no == 3) {
    labels <- c(paste0(sign1, cuts[2]), paste0(sign2, cuts[2]))
  }
  else if (cuts_no > 3) {
    for (i in 2:(length(cuts) - 2)) {
      temp <- paste0(cuts[i], "\u2013", cuts[i + 1])
      labels <- c(labels, temp)
    }
    
    labels <- 
      c(paste0(sign1, cuts[2]),
        labels,
        paste0(sign2, cuts[length(cuts) - 1]))
  }
  
  return(labels)
}

#' @title Convert bin cuts to simple and pretty labels.
#' @param cuts An vector of interval cuts.
#' @return A vector of character labels.
#' @keywords internal
sv_interval_labels_chr <- function(cuts) {
  
  right_closed <- ifelse(stringr::str_sub(cuts[1], -1L, -1L) == "]", TRUE, FALSE)
  
  labels <- stringr::str_replace_all(stringr::str_replace_all(cuts, ", ", "\u2013"), "\\[|\\]|\\)|\\(", "")
  
  sign1 <- ifelse(right_closed == TRUE, "\u2264", "<")  
  sign2 <- ifelse(right_closed == TRUE, ">", "\u2265")  
  
  labels[1] <- glue::glue("{sign1}{stringr::word(labels[2], sep = '\u2013')}")
  
  if (stringr::str_detect(tidyr::replace_na(labels[length(labels)], "NA"), "NA")) {    
    labels[length(labels) - 1] <- glue::glue("{sign2}{stringr::word(labels[length(labels) - 1], 1, sep = '\u2013')}")
  } else {
    labels[length(labels)] <- glue::glue("{sign2}{stringr::word(labels[length(labels)], 1, sep = '\u2013')}")
  }
  
  return(labels)
}