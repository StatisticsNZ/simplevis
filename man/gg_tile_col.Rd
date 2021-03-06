% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_tile.R
\name{gg_tile_col}
\alias{gg_tile_col}
\title{Tile ggplot that is coloured.}
\usage{
gg_tile_col(data, x_var, y_var, col_var, label_var = NULL,
  text_var = NULL, pal = NULL, pal_rev = FALSE, pal_label = NULL,
  width = 1, alpha = 1, size_line = 0.5, size_label = 3.5,
  title = NULL, title_wrap = 100, subtitle = NULL,
  subtitle_wrap = 100, x_expand = NULL, x_labels = NULL,
  x_na = TRUE, x_rev = FALSE, x_title = NULL, x_title_wrap = 50,
  y_expand = NULL, y_labels = waiver(), y_na = TRUE, y_rev = FALSE,
  y_title = NULL, y_title_wrap = 50, col_cuts = NULL,
  col_labels = NULL, col_labels_dp = NULL, col_legend_ncol = NULL,
  col_legend_nrow = NULL, col_method = NULL, col_na = TRUE,
  col_pretty_n = 6, col_title = NULL, col_title_wrap = 25,
  caption = NULL, caption_wrap = 80, font_family = "",
  font_size_title = NULL, font_size_body = NULL, mobile = FALSE)
}
\arguments{
\item{data}{A tibble or dataframe. Required input.}

\item{x_var}{Unquoted variable to be on the x scale (i.e. character, factor, logical, numeric, date or datetime). If numeric, date or datetime, variable values are bins that are mutually exclusive and equidistant. Required input.}

\item{y_var}{Unquoted numeric variable to be on the y scale. Required input.}

\item{col_var}{Unquoted categorical variable to colour the tiles Required input.}

\item{label_var}{Unquoted variable to label the tiles.}

\item{text_var}{Unquoted variable to be used as a customised tooltip in combination with plotly::ggplotly(plot, tooltip = "text"). Defaults to NULL.}

\item{pal}{Character vector of hex codes.}

\item{pal_rev}{Reverses the palette. Defaults to FALSE.}

\item{pal_label}{Hex code for the label font colour.}

\item{width}{Width of tiles. Defaults to 1.}

\item{alpha}{The alpha of the fill. Defaults to 1.}

\item{size_line}{The size of the outlines of tiles.}

\item{size_label}{The size of the of labels. Defaults to 3.5.}

\item{title}{Title string. Defaults to NULL.}

\item{title_wrap}{Number of characters to wrap the title to. Defaults to 100.}

\item{subtitle}{Subtitle string.}

\item{subtitle_wrap}{Number of characters to wrap the subtitle to. Defaults to 100.}

\item{x_expand}{A vector of range expansion constants used to add padding to the x scale, as per the ggplot2 expand argument in ggplot2 scales functions.}

\item{x_labels}{A function or vector to modify x scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep x labels untransformed.}

\item{x_na}{TRUE or FALSE of whether to include x_var NA values. Defaults to TRUE.}

\item{x_rev}{TRUE or FALSE of whether the x variable variable is reversed. Defaults to FALSE.}

\item{x_title}{X scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.}

\item{x_title_wrap}{Number of characters to wrap the x title to. Defaults to 50.}

\item{y_expand}{A vector of range expansion constants used to add padding to the y scale, as per the ggplot2 expand argument in ggplot2 scales functions.}

\item{y_labels}{A function or vector to modify y scale labels, as per the ggplot2 labels argument in ggplot2 scales functions. If NULL, categorical variable labels are converted to sentence case. Use ggplot2::waiver() to keep y labels untransformed.}

\item{y_na}{TRUE or FALSE of whether to include y_var NA values. Defaults to TRUE.}

\item{y_rev}{TRUE or FALSE of whether the y variable variable is reversed. Defaults to FALSE.}

\item{y_title}{y scale title string. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.}

\item{y_title_wrap}{Number of characters to wrap the y title to. Defaults to 50.}

\item{col_cuts}{A vector of cuts to colour a numeric variable. If "bin" is selected, the first number in the vector should be either -Inf or 0, and the final number Inf. If "quantile" is selected, the first number in the vector should be 0 and the final number should be 1. Defaults to quartiles.}

\item{col_labels}{A vector of colour labels.}

\item{col_labels_dp}{For numeric colour variables and where col_labels equals NULL, the number of decimal places. Defaults to 1 for "quantile" col_method, and the lowest dp within the col_cuts vector for "bin".}

\item{col_legend_ncol}{The number of columns in the legend.}

\item{col_legend_nrow}{The number of rows in the legend.}

\item{col_method}{The method of colouring features, either "bin", "quantile" or "category." If numeric, defaults to "bin".}

\item{col_na}{TRUE or FALSE of whether to include col_var NA values. Defaults to TRUE.}

\item{col_pretty_n}{For a numeric colour variable, the desired number of intervals on the colour scale, as calculated by the pretty algorithm. Defaults to 6.}

\item{col_title}{Colour title string for the legend. Defaults to NULL, which converts to sentence case with spaces. Use "" if you would like no title.}

\item{col_title_wrap}{Number of characters to wrap the colour title to. Defaults to 25. Not applicable where mobile equals TRUE.}

\item{caption}{Caption title string.}

\item{caption_wrap}{Number of characters to wrap the caption to. Defaults to 80.}

\item{font_family}{Font family to use. Defaults to "".}

\item{font_size_title}{Font size for the title text. Defaults to 11.}

\item{font_size_body}{Font size for all text other than the title. Defaults to 10.}

\item{mobile}{Whether the plot is to be displayed on a mobile device. Defaults to FALSE. If within a shiny app with the mobileDetect function, then use mobile = input$isMobile.}
}
\value{
A ggplot object.
}
\description{
Tile ggplot that is coloured, but not facetted.
}
\examples{
library(simplevis)
library(dplyr)
library(palmerpenguins)

plot_data <- penguins \%>\% 
  group_by(species, sex) \%>\% 
  summarise(bill_length_mm = round(mean(bill_length_mm, na.rm = TRUE), 0)) \%>\% 
  mutate(label = glue::glue("{bill_length_mm} mm"))
  
gg_tile_col(plot_data, 
  x_var = sex, 
  y_var = species, 
  col_var = bill_length_mm, 
  label_var = label) 
            
}
