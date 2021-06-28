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
sv_max_density <- function(data, x_var, 
                               density_bw = "nrd0",
                               density_adjust = 1,
                               density_kernel = "gaussian",
                               density_n = 512,
                               density_trim = FALSE) {
  
  vctr <- data %>% 
    dplyr::select(density_var = {{x_var}} ) %>%
    dplyr::pull(.data$density_var)  
  
  max(stats::density(vctr, bw = density_bw, 
                 adjust = density_adjust, 
                 kernel = density_kernel, 
                 n = density_n, 
                 trim = density_trim,
                 na.rm = TRUE)[[2]])
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
sv_max_density_col_identity <- function(data, x_var, col_var, 
                            density_bw = "nrd0",
                            density_adjust = 1,
                            density_kernel = "gaussian",
                            density_n = 512,
                            density_trim = FALSE) {
  
  data %>% 
    dplyr::group_split( {{col_var}} ) %>% 
    purrr::map(~dplyr::select(., density_var = {{x_var}} )) %>%
    purrr::map(~dplyr::pull(., .data$density_var)) %>% 
    purrr::map(~stats::density(., 
                 bw = density_bw, 
                 adjust = density_adjust, 
                 kernel = density_kernel, 
                 n = density_n, 
                 trim = density_trim,
                 na.rm = TRUE)[[2]]) %>% 
    purrr::map_dbl(~max(.)) %>% 
    max()
}

#' Calculate the max density in a gg_density_col(..., position == "stack") plot.
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
sv_max_density_col_stack <- function(data, x_var, col_var, 
                                     density_bw = "nrd0",
                                     density_adjust = 1,
                                     density_kernel = "gaussian",
                                     density_n = 512,
                                     density_trim = FALSE) {
  
  data %>% 
    dplyr::group_split( {{col_var}} ) %>% 
    purrr::map(~dplyr::select(., density_var = {{x_var}} )) %>%
    purrr::map(~dplyr::pull(., .data$density_var)) %>% 
    purrr::map(~stats::density(., 
                 bw = density_bw, 
                 adjust = density_adjust, 
                 kernel = density_kernel, 
                 n = density_n, 
                 trim = density_trim,
                 na.rm = TRUE)[[2]]) %>% 
    purrr::reduce(`+`) %>% 
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
sv_max_density_facet <- function(data, x_var, facet_var, 
                               density_bw = "nrd0",
                               density_adjust = 1,
                               density_kernel = "gaussian",
                               density_n = 512,
                               density_trim = FALSE) {
  
  data %>% 
    dplyr::group_split( {{facet_var}} ) %>% 
    purrr::map(~dplyr::select(., density_var = {{x_var}} )) %>%
    purrr::map(~dplyr::pull(., .data$density_var)) %>% 
    purrr::map(~stats::density(., 
                 bw = density_bw, 
                 adjust = density_adjust, 
                 kernel = density_kernel, 
                 n = density_n, 
                 trim = density_trim,
                 na.rm = TRUE)[[2]]) %>% 
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
sv_max_density_col_facet_identity <- function(data, x_var, col_var, facet_var, 
                                 density_bw = "nrd0",
                                 density_adjust = 1,
                                 density_kernel = "gaussian",
                                 density_n = 512,
                                 density_trim = FALSE) {
  
  data %>% 
    dplyr::group_split( {{col_var}}, {{facet_var}} ) %>% 
    purrr::map(~dplyr::select(., density_var = {{x_var}} )) %>%
    purrr::map(~dplyr::pull(., .data$density_var)) %>% 
    purrr::map(~stats::density(., 
                 bw = density_bw, 
                 adjust = density_adjust, 
                 kernel = density_kernel, 
                 n = density_n, 
                 trim = density_trim,
                 na.rm = TRUE)[[2]]) %>% 
    purrr::map_dbl(~max(.)) %>% 
    max()
}

#' Calculate the max density in a gg_density_col_facet(..., position == "stack") plot.
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
sv_max_density_col_facet_stack <- function(data, x_var, col_var, facet_var, 
                                           density_bw = "nrd0",
                                           density_adjust = 1,
                                           density_kernel = "gaussian",
                                           density_n = 512,
                                           density_trim = FALSE) {
  
  data %>% 
    dplyr::group_split( {{col_var}}, {{facet_var}} ) %>% 
    purrr::map(~dplyr::select(., density_var = {{x_var}} )) %>%
    purrr::map(~dplyr::pull(., .data$density_var)) %>% 
    purrr::map(~stats::density(., 
                 bw = density_bw, 
                 adjust = density_adjust, 
                 kernel = density_kernel, 
                 n = density_n, 
                 trim = density_trim,
                 na.rm = TRUE)[[2]]) %>% 
    purrr::reduce(`+`) %>% 
    max()
}
