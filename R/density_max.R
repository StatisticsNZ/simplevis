#' Calculate the max density in a gg_density() plot.
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max <- function(data, x_var, 
                           model_bw = "nrd0",
                           model_adjust = 1,
                           model_kernel = "gaussian",
                           model_n = 512,
                           model_trim = FALSE) {
  
  x_var <- rlang::enquo(x_var)
  
  vctr <- data %>% 
    dplyr::select(density_var = !!x_var) %>%
    dplyr::pull(.data$density_var)  
  
  suppressWarnings(max(
    stats::density(
      vctr,
      bw = model_bw,
      adjust = model_adjust,
      kernel = model_kernel,
      n = model_n,
      trim = model_trim,
      na.rm = TRUE
    )[[2]]
  ))
}

#' Calculate the max density in a gg_density_col(..., position == "identity") plot.
#'
#' @param data A tibble or dataframe. Required input.
#' @param x_var Unquoted numeric variable to be on the x scale. Required input.
#' @param col_var Unquoted categorical variable to colour density areas. Required input.
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_col <- function(data, x_var, col_var, 
                               model_bw = "nrd0",
                               model_adjust = 1,
                               model_kernel = "gaussian",
                               model_n = 512,
                               model_trim = FALSE) {
  
  x_var <- rlang::enquo(x_var)
  col_var <- rlang::enquo(col_var)
  
  data %>%
    dplyr::group_split(!!col_var) %>%
    purrr::map( ~ dplyr::select(., density_var = !!x_var)) %>%
    purrr::map( ~ dplyr::pull(., .data$density_var)) %>%
    purrr::map( ~ suppressWarnings(
      stats::density(
        .,
        bw = model_bw,
        adjust = model_adjust,
        kernel = model_kernel,
        n = model_n,
        trim = model_trim,
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
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_facet <- function(data, x_var, facet_var, 
                                 model_bw = "nrd0",
                                 model_adjust = 1,
                                 model_kernel = "gaussian",
                                 model_n = 512,
                                 model_trim = FALSE) {
  
  x_var <- rlang::enquo(x_var)
  facet_var <- rlang::enquo(facet_var)
  
  data %>%
    dplyr::group_split(!!facet_var) %>%
    purrr::map( ~ dplyr::select(., density_var = !!x_var)) %>%
    purrr::map( ~ dplyr::pull(., .data$density_var)) %>%
    purrr::map( ~ suppressWarnings(
      stats::density(
        .,
        bw = model_bw,
        adjust = model_adjust,
        kernel = model_kernel,
        n = model_n,
        trim = model_trim,
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
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_col_facet <- function(data, x_var, col_var, facet_var, 
                                     model_bw = "nrd0",
                                     model_adjust = 1,
                                     model_kernel = "gaussian",
                                     model_n = 512,
                                     model_trim = FALSE) {
  
  data %>% 
    dplyr::group_split( {{col_var}}, {{facet_var}} ) %>% 
    purrr::map(~dplyr::select(., density_var = {{x_var}} )) %>%
    purrr::map(~dplyr::pull(., .data$density_var)) %>% 
    purrr::map( ~ suppressWarnings(
      stats::density(
        .,
        bw = model_bw,
        adjust = model_adjust,
        kernel = model_kernel,
        n = model_n,
        trim = model_trim,
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
#' @param model_bw The bw argument of the stats::density function. Defaults to "nrd0".
#' @param model_adjust The adjust argument of the stats::density function. Defaults to 1.
#' @param model_kernel The kernel argument of the stats::density function. Defaults to "gaussian".
#' @param model_n The n argument of the stats::density function. Defaults to 512.
#' @param model_trim The trim argument of the stats::density function. Defaults to FALSE.
#'
#' @keywords internal
sv_density_max_col_facet <- function(data, x_var, col_var, facet_var, 
                                     model_bw = "nrd0",
                                     model_adjust = 1,
                                     model_kernel = "gaussian",
                                     model_n = 512,
                                     model_trim = FALSE) {
  
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
        bw = model_bw,
        adjust = model_adjust,
        kernel = model_kernel,
        n = model_n,
        trim = model_trim,
        na.rm = TRUE
      )[[2]]
    )))) %>%
    dplyr::group_by(!!facet_var) %>%
    dplyr::summarise(max_density = max(.data$max_density)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(max_density = max(.data$max_density)) %>% 
    dplyr::pull(.data$max_density)
}