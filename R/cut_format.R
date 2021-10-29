#' Convert Numeric to Factor, with custom formatting
#'
#' This is an enhanced version of [base::cut()] that allows a custom
#' formatting to be applied to the values.
#'
#' @inheritParams base::cut.default
#' @param breaks  `[numeric]`\cr
#' A vector of two or more unique cut points
#' @param ... Passed to `cut()`
#' @param format_fun `[function(x): character]`\cr
#'   A vectorized function that performs the desired formatting.  Default:
#'   [base::format()]
#' @param sep `[character(1)]`\cr
#'   The separator between lower and upper end of the interval. Default:
#'   `", "`
#' @param paren `[character(4)]`\cr
#'   Opening and closing parentheses in two variants. Default:
#'   \code{c("(", "[", ")", "]")}
#' @keywords internal
#' @importFrom utils head tail
#' @references kimisc package https://github.com/krlmlr/kimisc
cut_format <- function(x, breaks, include.lowest = FALSE, right = TRUE,
                       ordered_result = FALSE, ...,
                       format_fun = format, sep = ", ",
                       paren = c("(", "[", ")", "]")) {
  if (length(breaks) < 2L) {
    stop("Please specify breaks as a numeric vector of length >= 2",
         call. = FALSE)
  }
  
  if (right) {
    ob <- c(include.lowest, rep(FALSE, length(breaks) - 2L))
    cb <- rep(TRUE, length(breaks) - 1L)
  } else {
    ob <- rep(TRUE, length(breaks) - 1L)
    cb <- c(rep(FALSE, length(breaks) - 2L), include.lowest)
  }
  
  ob <- ifelse(ob, paren[[2L]], paren[[1L]])
  cb <- ifelse(cb, paren[[4L]], paren[[3L]])
  
  formatted_breaks <- format_fun(breaks)
  labels <- paste0(ob, head(formatted_breaks, -1L), sep, tail(formatted_breaks, -1L), cb)
  cut.default(x = x, breaks = breaks, labels = labels, include.lowest = include.lowest,
              right = right, ordered_result = ordered_result, ...)
}
