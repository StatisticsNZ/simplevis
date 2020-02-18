#' Package imports
#' 
#' @import ggplot2
#' @import janitor
#' @import leaflet
#' @import leafpop
#' @import rlang
#' @import dplyr
#' @importFrom stats quantile
#'
NULL


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
