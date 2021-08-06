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

#' Calculate the max density in a gg_density() plot.
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max <- function(data, x_var, 
                           density_bw = "nrd0",
                           density_adjust = 1,
                           density_kernel = "gaussian",
                           density_n = 512,
                           density_trim = FALSE) {
  
  x_var <- rlang::enquo(x_var)
  
  vctr <- data %>% 
    dplyr::select(density_var = !!x_var) %>%
    dplyr::pull(.data$density_var)  
  
  suppressWarnings(max(
    stats::density(
      vctr,
      bw = density_bw,
      adjust = density_adjust,
      kernel = density_kernel,
      n = density_n,
      trim = density_trim,
      na.rm = TRUE
    )[[2]]
  ))
}

#' Calculate the max density in a gg_density_col(..., position == "identity") plot.
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_col <- function(data, x_var, col_var, 
                               density_bw = "nrd0",
                               density_adjust = 1,
                               density_kernel = "gaussian",
                               density_n = 512,
                               density_trim = FALSE) {
  
  x_var <- rlang::enquo(x_var)
  col_var <- rlang::enquo(col_var)
  
  data %>%
    dplyr::group_split(!!col_var) %>%
    purrr::map( ~ dplyr::select(., density_var = !!x_var)) %>%
    purrr::map( ~ dplyr::pull(., .data$density_var)) %>%
    purrr::map( ~ suppressWarnings(
      stats::density(
        .,
        bw = density_bw,
        adjust = density_adjust,
        kernel = density_kernel,
        n = density_n,
        trim = density_trim,
        na.rm = TRUE
      )[[2]]
    )) %>%
    purrr::map_dbl( ~ max(.)) %>%
    max()
}

#' Calculate the max density in a gg_density_facet() plot.
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param facet_var Unquoted categorical variable to facet by. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_facet <- function(data, x_var, facet_var, 
                                 density_bw = "nrd0",
                                 density_adjust = 1,
                                 density_kernel = "gaussian",
                                 density_n = 512,
                                 density_trim = FALSE) {
  
  x_var <- rlang::enquo(x_var)
  facet_var <- rlang::enquo(facet_var)
  
  data %>%
    dplyr::group_split(!!facet_var) %>%
    purrr::map( ~ dplyr::select(., density_var = !!x_var)) %>%
    purrr::map( ~ dplyr::pull(., .data$density_var)) %>%
    purrr::map( ~ suppressWarnings(
      stats::density(
        .,
        bw = density_bw,
        adjust = density_adjust,
        kernel = density_kernel,
        n = density_n,
        trim = density_trim,
        na.rm = TRUE
      )[[2]]
    )) %>%
    purrr::map_dbl( ~ max(.)) %>%
    max()
}

#' Calculate the max density in a gg_density_col_facet(..., position == "identity") plot.
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param facet_var Unquoted categorical variable to facet by. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_col_facet <- function(data, x_var, col_var, facet_var, 
                                     density_bw = "nrd0",
                                     density_adjust = 1,
                                     density_kernel = "gaussian",
                                     density_n = 512,
                                     density_trim = FALSE) {
  
  data %>% 
    dplyr::group_split( {{col_var}}, {{facet_var}} ) %>% 
    purrr::map(~dplyr::select(., density_var = {{x_var}} )) %>%
    purrr::map(~dplyr::pull(., .data$density_var)) %>% 
    purrr::map( ~ suppressWarnings(
      stats::density(
        .,
        bw = density_bw,
        adjust = density_adjust,
        kernel = density_kernel,
        n = density_n,
        trim = density_trim,
        na.rm = TRUE
      )[[2]]
    )) %>%
    purrr::map_dbl(~max(.)) %>% 
    max() 
}

#' Calculate the max density in a gg_density_col_facet(..., position == "identity") plot.

#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param facet_var Unquoted categorical variable to facet by. Required input.
#' @param density_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param density_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param density_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param density_n The n argument of the stats::density function. Defaults to 512.
#' @param density_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_col_facet <- function(data, x_var, col_var, facet_var, 
                                     density_bw = "nrd0",
                                     density_adjust = 1,
                                     density_kernel = "gaussian",
                                     density_n = 512,
                                     density_trim = FALSE) {
  
  x_var <- rlang::enquo(x_var)
  col_var <- rlang::enquo(col_var)
  facet_var <- rlang::enquo(facet_var)
  
  data %>%
    dplyr::rename(density_var = !!x_var) %>%
    dplyr::group_by(!!col_var, !!facet_var) %>%
    tidyr::nest() %>%
    dplyr::mutate(max_density = suppressWarnings(purrr::map_dbl(data, ~ max(
      stats::density(
        .x$density_var,
        bw = density_bw,
        adjust = density_adjust,
        kernel = density_kernel,
        n = density_n,
        trim = density_trim,
        na.rm = TRUE
      )[[2]]
    )))) %>%
    dplyr::group_by(!!facet_var) %>%
    dplyr::summarise(max_density = max(.data$max_density)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(max_density = max(.data$max_density)) %>% 
    dplyr::pull(.data$max_density)
}

#' Convert interval labels to a simpler format. 
#'
#' @param breaks A vector of interval breaks. 
#'
#' @return Labels for the legend.
#' @keywords internal
#' 
sv_interval_breaks_to_interval_labels <- function(breaks) {
  
  right_closed <- ifelse(stringr::str_sub(breaks[1], -1L, -1L) == "]", TRUE, FALSE)
  
  breaks <- stringr::str_replace_all(stringr::str_replace_all(breaks, ", ", "\u2013"), "\\[|\\]|\\)|\\(", "")
  
  sign1 <- ifelse(right_closed == TRUE, "\u2264", "<")  
  sign2 <- ifelse(right_closed == TRUE, ">", "\u2265")  
  
  breaks[1] <- glue::glue("{sign1}{stringr::word(breaks[2], sep = '\u2013')}")
  
  if (stringr::str_detect(tidyr::replace_na(breaks[length(breaks)], "NA"), "NA")) {    
    breaks[length(breaks) - 1] <- glue::glue("{sign2}{stringr::word(breaks[length(breaks) - 1], 1, sep = '\u2013')}")
  } else {
    breaks[length(breaks)] <- glue::glue("{sign2}{stringr::word(breaks[length(breaks)], 1, sep = '\u2013')}")
  }
  return(breaks)
}

#' @title Convert bin cuts to interval legend labels.
#' @param bin_cuts A numeric vector of bin cuts from which to create a vector of legend labels.
#' @param label_digits The number of decimal places to round labels to.
#' @param right_closed TRUE or FALSE of whether bins or quantiles are to be cut right-closed. Defaults to TRUE.
#' @return A vector of labels.
#' @export 
#' @examples 
#' bin_cuts_to_interval_labels(c(0, 0.1, 3, 4.1, 7, 100, Inf))
bin_cuts_to_interval_labels <- function(bin_cuts, label_digits = NULL, right_closed = TRUE) {
  
  if (is.null(label_digits)) label_digits <- sv_max_dp(bin_cuts) 
  
  labels <- vector("character", 0)
  
  bin_cuts_no <- length(bin_cuts)
  
  bin_cuts <- format(round(as.numeric(bin_cuts), label_digits), nsmall = label_digits, big.mark = ",", trim = TRUE) 
  
  sign1 <- ifelse(right_closed == TRUE, "\u2264", "<")  
  sign2 <- ifelse(right_closed == TRUE, ">", "\u2265")  
  
  if (bin_cuts_no == 2) {
    labels <- c("Feature")
  }
  else if (bin_cuts_no == 3) {
    labels <- c(paste0(sign1, bin_cuts[2]), paste0(sign2, bin_cuts[2]))
  }
  else if (bin_cuts_no > 3) {
    for (i in 2:(length(bin_cuts) - 2)) {
      temp <- paste0(bin_cuts[i], "\u2013", bin_cuts[i + 1])
      labels <- c(labels, temp)
    }
    
    labels <- 
      c(paste0(sign1, bin_cuts[2]),
        labels,
        paste0(sign2, bin_cuts[length(bin_cuts) - 1]))
  }
  return(labels)
}