# other

#' @title Numeric legend labels.
#' @description Pretty numeric legend labels.
#' @param bin_cuts A numeric vector of bin cuts from which to create a vector of legend labels.
#' @param legend_digits The number of digits to round the legend labels.
#' @return A vector of labels.
#' @export
numeric_legend_labels <- function(bin_cuts, legend_digits = 1) {
  labels <- vector("character", 0)
  bin_cuts_no <- length(bin_cuts)
  bin_cuts <-
    sprintf(paste0("%.", legend_digits, "f"),
            round(bin_cuts, legend_digits))

  if (bin_cuts_no == 2) {
    labels <- c("Feature")
  }
  else if (bin_cuts_no == 3) {
    labels <- c(paste0("<", bin_cuts[2]), paste0("\u2265", bin_cuts[2]))
  }
  else if (bin_cuts_no > 3) {
    for (i in 2:(length(bin_cuts) - 2)) {
      temp <- paste0(bin_cuts[i], "\u2013", bin_cuts[i + 1])
      labels <- c(labels, temp)
    }
    labels <-
      c(paste0("<", bin_cuts[2]),
        labels,
        paste0("\u2265", bin_cuts[length(bin_cuts) - 1]))
  }
}

#' @title Signed square root ggplot scale transformation.
#' @description A signed square root ggplot scale transformation.
#' @return A ggplot scale transformation.
#' @export
signed_sqrt_trans <- function()
  scales::trans_new(
    name = "signed_sqrt",
    transform = function(x) {
      sign(x) * sqrt(abs(x))
    },
    inverse = function(x) {
      x ^ 2 * sign(x)
    }
  )

#' @title Colour palette for categorical variables.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_snz <- c("#085c75", "#d2ac2f", "#ae4e51", "#35345d", "#76a93f", "#6f2e38", "#0d94a3", "#dd6829", "#1a6e5b")

#' @title Colour palette for a categorical trend variable with 2 values.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_snz_trend2 <- c("#AE4E51", "#0D94A3")

#' @title Colour palette for a categorical trend variable with 3 values.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_snz_trend3 <- c("#0D94A3", "#C4C4C7", "#AE4E51")

#' @title Colour palette for a categorical trend variable with 5 values.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_snz_trend5 <- c("#35345D", "#0D94A3", "#C4C4C7", "#AE4E51", "#6F2E38")

#' @title Colour palette for 4 categories from good to bad
#' @description A colour palette used for depicting subcategories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_snz_nof4 <- c("#35345d", "#085c75", "#d2ac2f", "#6f2e38")

#' @title Colour palette for 5 categories from good to bad
#' @description A colour palette used for depicting subcategories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_snz_nof5 <- c("#35345d", "#085c75", "#76a93f", "#d2ac2f", "#6f2e38")

#' @title Colour palette for the NZTCS.
#' @description A colour palette used for depicting categories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_snz_nztcs_c <- c("Threatened" = "#6f2e38", "At risk" = "#ae4e51", "Data deficient" = "#c4c4c7", "Not threatened" = "#0d94a3")

#' @title Colour palette for the NZTCS.
#' @description A colour palette used for depicting subcategories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_snz_nztcs_s <- c(
  "Nationally critical" = "#6f2e38", "Nationally endangered" = "#813641", "Nationally vulnerable" = "#933d4a",
  "Declining" = "#ae4e51", "Recovering" = "#b75e61", "Relict" = "#bf7073", "Naturally uncommon" = "#c78284",
  "Data deficient" = "#c4c4c7", "Not threatened" = "#0d94a3"
)

#' @title  Colour palette for categorical variables.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_ea19 <- c("#172a45", "#00b2c3", "#c04124", "#005c75", "#a2c62b", "#702e01", "#ff590d", "#c4c4c7", "#007f39")

#' @title Colour palette for a categorical trend variable with 2 values.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_ea19_trend2 <- c("#00b2c3", "#c04124")

#' @title Colour palette for a categorical trend variable with 3 values.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_ea19_trend3 <- c("#00b2c3", "#c4c4c7", "#c04124")

#' @title Colour palette for a categorical trend variable with 5 values.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_ea19_trend5 <- c("#172a45", "#00b2c3", "#c4c4c7", "#c04124", "#702e01")

#' @title Colour palette for 4 categories from good to bad
#' @description A colour palette used for depicting subcategories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_ea19_nof4 <- c("#172a45", "#00b2c3", "#a2c62b", "#702e01")

#' @title Colour palette for 5 categories from good to bad
#' @description A colour palette used for depicting subcategories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_ea19_nof5 <- c("#172a45", "#00b2c3", "#a2c62b", "#ff590d", "#702e01")

#' @title Colour palette for the NZTCS.
#' @description A colour palette used for depicting categories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_ea19_nztcs_c <- c("Threatened" = "#702e01", "At risk" = "#c04124", "Data deficient" = "#c4c4c7", "Not threatened" = "#00b2c3")

#' @title Colour palette for categorical variables for points.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_point_set1 <- c("#377EB8", "#A65628", "#F781BF", "#4DAF4A", "#FF7F00", "#984EA3", "#FFFF33", "#E41A1C", "#999999") #from Set1, 9col

#' @title Colour palette for categorical variables for points.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_point_trend3 <- c("#4575B4", "#D3D3D3", "#D73027")

#' @title Colour palette for categorical variables for points.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_point_trend5 <- c("#4575B4", "#90C3DD", "#D3D3D3", "#F98E52", "#D73027")

#' @title A4 useable width.
#' @description The width of useable space within an a4 sheet.
#' @return A numeric value.
#' @export
a4_width_mm <- 170

#' @title A4 useable height.
#' @description The height of useable space within an a4 sheet.
#' @return A numeric value.
#' @export
a4_height_mm <- 257

#' @title Convert column names to sentence case.
#' @description A function to convert colnames to snakecase and then to sentence case to be used in functions for making hover values.
#' @param data The number of digits to round the legend labels.
#' @return A numeric value.
#' @export
sentence_spaced_colnames <- function(data) {
  data <- janitor::clean_names(data) 
  colnames(data) <-  stringr::str_replace_all(stringr::str_to_sentence(colnames(data)), "_", " ")
  return(data)
}

#' @title Remove ggplotly buttons from the mode bar, other than the camera and plotly logo.
#' @description Remove ggplotly buttons from the mode bar, other than the camera and plotly logo.
#' @param plotly A plotly object.
#' @export
remove_plotly_buttons <- function(plotly){
  plotly::config(plotly,
    modeBarButtonsToRemove = list(
      "zoom2d", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d","resetScale2d", "hoverClosestCartesian",
      "hoverCompareCartesian", "sendDataToCloud", "toggleHover", "resetViews", "toggleSpikelines",
      "resetViewMapbox", "toggleSpikelines", "resetViewMapbox", "lasso2d", "select2d"
    ),
    displaylogo = TRUE
  )
}


