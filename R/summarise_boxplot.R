#' Summarise boxplot stats in a dataset or tibble.
#'
#' @param data A tibble or dataframe. Required input. Group the dataset as appropriate prior.
#' @param var Unquoted variable from which to calculate boxplot stats. Required input.
#' @param names_vctr A vector of names for the boxplot stats.
#' @param ... Passed to \code{boxplot.stats}
#'
#' @return A tibble or dataframe. 
#' @export
#'
#' @examples
#' library(simplevis)
#' library(dplyr)
#' library(palmerpenguins)
#' 
#' penguins %>% 
#'   group_by(species) %>% 
#'   summarise_boxplot_stats(body_mass_g) 
#' 
#' penguins %>% 
#'   group_by(sex, species) %>% 
#'   summarise_boxplot_stats(body_mass_g, names_vctr = LETTERS[1:5]) 
#'   
summarise_boxplot_stats <- function(data, var, names_vctr = c('min', 'lower', 'middle', 'upper', 'max'), ...) {
  var <- rlang::enquo(var) 
  
  data %>% 
    dplyr::summarise(dplyr::across(!!var, ~ list(
      rlang::set_names(
        boxplot.stats(.x, ...)$stats,
        names_vctr
      )))) %>% 
    dplyr::ungroup() %>% 
    tidyr::unnest_wider(!!var)
}


#' Summarise outliers in a dataset or tibble.
#'
#' @param data A tibble or dataframe. Required input. Group the dataset as appropriate prior.
#' @param var Unquoted variable from which to calculate outliers. Required input.
#' @param ... Passed to \code{boxplot.stats}
#' 
#' @return A tibble or dataframe. 
#' @export
#'
#' @examples
#' library(simplevis)
#' library(dplyr)
#' library(palmerpenguins)
#' 
#' penguins %>% 
#'   group_by(species) %>% 
#'   summarise_boxplot_outliers(body_mass_g)
#' 
summarise_boxplot_outliers <- function(data, var, ...) {
  var <- rlang::enquo(var) 
  
  data %>% 
    dplyr::summarise(dplyr::across(!!var, ~boxplot.stats(.x, ...)$out)) %>% 
    dplyr::ungroup()
}
  

