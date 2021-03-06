% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_point.R
\name{gg_point_col}
\alias{gg_point_col}
\title{Point ggplot that is coloured.}
\usage{
gg_point_col(data, x_var, y_var, col_var, text_var = NULL,
  position = "identity", alpha = 1, size_point = 1, pal = NULL,
  pal_rev = FALSE, x_balance = FALSE, x_expand = NULL,
  x_gridlines_minor = FALSE, x_labels = NULL, x_na = TRUE,
  x_pretty_n = 6, x_rev = FALSE, x_trans = "identity",
  x_zero = FALSE, x_zero_line = NULL, y_balance = FALSE,
  y_expand = NULL, y_gridlines_minor = FALSE, y_labels = waiver(),
  y_na = TRUE, y_pretty_n = 5, y_trans = "identity",
  y_zero = FALSE, y_zero_line = NULL, title = NULL,
  subtitle = NULL, x_title = NULL, y_title = NULL,
  col_title = NULL, caption = NULL, col_cuts = NULL,
  col_labels = NULL, col_labels_dp = NULL, col_legend_ncol = NULL,
  col_legend_nrow = NULL, col_method = NULL, col_na = TRUE,
  col_pretty_n = 6, font_family = "", font_size_title = NULL,
  font_size_body = NULL, title_wrap = 100, subtitle_wrap = 100,
  x_title_wrap = 50, y_title_wrap = 50, col_title_wrap = 25,
  caption_wrap = 80, mobile = FALSE)
}
\arguments{
\item{data}{An ungrouped summarised tibble or dataframe. Required input.}

\item{x_var}{Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). Required input.}

\item{y_var}{Unquoted numeric variable to be on the y scale. Required input.}

\item{col_var}{Unquoted variable for points to be coloured by. Required input.}

\item{text_var}{Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.}

\item{position}{Whether bars are positioned by "identity" or "jitter". Defaults to "identity".}

\item{alpha}{The opacity of points. Defaults to 1.}

\item{size_point}{Size of points. Defaults to 1.}

\item{pal}{Character vector of hex codes.}

\item{pal_rev}{Reverses the palette. Defaults to FALSE.}

\item{x_balance}{For a numeric x variable, add balance to the x scale so that zero is in the centre. Defaults to FALSE.}

\item{x_expand}{A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions.}

\item{x_gridlines_minor}{TRUE or FALSE of whether to add minor gridlines to the x scale. Defaults to FALSE.}

\item{x_labels}{A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.}

\item{x_na}{TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.}

\item{x_pretty_n}{For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 6.}

\item{x_rev}{For a categorical x variable, TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.}

\item{x_trans}{For a numeric x variable, a string specifying a transformation for the x scale, such as "log10" or "sqrt". Defaults to "identity".}

\item{x_zero}{For a numeric x variable, TRUE or FALSE of whether the minimum of the x scale is zero. Defaults to FALSE.}

\item{x_zero_line}{For a numeric x variable, TRUE or FALSE of whether to add a zero reference line to the x scale. Defaults to TRUE if there are positive and negative values in x_var. Otherwise defaults to FALSE.}

\item{y_balance}{For a numeric y variable, add balance to the y scale so that zero is in the centre of the y scale.}

\item{y_expand}{A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions.}

\item{y_gridlines_minor}{TRUE or FALSE of whether to add minor gridlines to the y scale. Defaults to FALSE.}

\item{y_labels}{A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.}

\item{y_na}{TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.}

\item{y_pretty_n}{For a numeric or date x variable, the desired number of intervals on the x scale, as calculated by the pretty algorithm. Defaults to 5.}

\item{y_trans}{For a numeric y variable, a string specifying a transformation for the y scale, such as "log10" or "sqrt". Defaults to "identity".}

\item{y_zero}{For a numeric y variable, TRUE or FALSE of whether the minimum of the y scale is zero. Defaults to TRUE.}

\item{y_zero_line}{For a numeric y variable, TRUE or FALSE whether to add a zero reference line to the y scale. Defaults to TRUE if there are positive and negative values in y_var. Otherwise defaults to FALSE.}

\item{title}{Title string. Defaults to NULL.}

\item{subtitle}{Subtitle string.}

\item{x_title}{X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.}

\item{y_title}{y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.}

\item{col_title}{Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.}

\item{caption}{Caption title string.}

\item{col_cuts}{A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles.}

\item{col_labels}{A vector of colour labels.}

\item{col_labels_dp}{For numeric colour variables and where col_labels equals NULL, the number of decimal places. Defaults to 1 for "quantile" col_method, and the lowest dp within the col_cuts vector for "bin".}

\item{col_legend_ncol}{The number of columns in the legend.}

\item{col_legend_nrow}{The number of rows in the legend.}

\item{col_method}{The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "bin".}

\item{col_na}{TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.}

\item{col_pretty_n}{For a numeric colour variable, the desired number of intervals on the colour scale, as calculated by the pretty algorithm. Defaults to 6.}

\item{font_family}{Font family to use. Defaults to "".}

\item{font_size_title}{Font size for the title text. Defaults to 11.}

\item{font_size_body}{Font size for all text other than the title. Defaults to 10.}

\item{title_wrap}{Number of characters to wrap the title to. Defaults to 100. Not applicable where mobile equals TRUE.}

\item{subtitle_wrap}{Number of characters to wrap the subtitle to. Defaults to 100. Not applicable where mobile equals TRUE.}

\item{x_title_wrap}{Number of characters to wrap the x title to. Defaults to 50.}

\item{y_title_wrap}{Number of characters to wrap the y title to. Defaults to 50.}

\item{col_title_wrap}{Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.}

\item{caption_wrap}{Number of characters to wrap the caption to. Defaults to 80. Not applicable where mobile equals TRUE.}

\item{mobile}{Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.}
}
\value{
A ggplot object.
}
\description{
Point ggplot that is coloured, but not facetted.
}
\examples{
library(simplevis)
library(palmerpenguins)

gg_point_col(penguins, 
             x_var = bill_length_mm, 
             y_var = body_mass_g, 
             col_var = species)

}
