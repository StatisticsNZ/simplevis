#' Summarise boxplot stats
#'
#' @param data A tibble or dataframe. Required input.
#' @param var A numeric variable. Required input.
#' @param ... Passed to \code{boxplot.stats}
#'
#' @return A tibble or dataframe.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(simplevis)
#' library(palmerpenguins)
#' 
#' penguins %>% 
#'   group_by(sex, species) %>% 
#'   summarise_boxplot_stats(body_mass_g) 
#' 
#' penguins %>% 
#'   group_by(sex, species) %>% 
#'   summarise_boxplot_stats(body_mass_g) %>% 
#'   gg_boxplot_col(sex, body_mass_g, species, stat = "identity")
#'   
summarise_boxplot_stats <- function(data, var, ...) {
  data %>% 
    dplyr::summarise(dplyr::across({{var}}, ~ list(
      rlang::set_names(
        boxplot.stats(.x, ...)$stats,
        c('min', 'lower', 'middle', 'upper', 'max')
      )))) 
}
