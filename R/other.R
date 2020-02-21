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

# pal

#' @title Colour palette for categorical variables.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_snz <- c(
  "#085c75",
  # teal 1
  "#d2ac2f",
  # yellow 2
  "#ae4e51",
  # pinky brown 3
  "#35345d",
  # dark blue 4
  "#76a93f",
  # light green 5
  "#6f2e38",
  # chocolate brown 6
  "#0d94a3",
  # light blue 7
  "#dd6829",
  # orange 8
  "#1a6e5b" # dark green 9
)

#' @title Colour palette for categorical variables.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_set1 <-
  c(
    "#377EB8", #blue
    "#A65628", #brown
    "#F781BF", #pink
    "#4DAF4A", #green
    "#FF7F00", #orange
    "#984EA3", #purple
    "#FFFF33", #yellow
    "#E41A1C", #red
    "#999999" #grey
  ) #from Set1, 9col

#' @title Colour palette for categorical variables.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_trend3 <- c("#4575B4", "#D3D3D3", "#D73027")

#' @title Colour palette for categorical variables.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_trend5 <-
  c("#4575B4", "#90C3DD", "#D3D3D3", "#F98E52", "#D73027")

#' @title  Colour palette for categorical variables.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_ea19 <- c(
  "#172a45",
  # dark blue 1
  "#00b2c3",
  #light blue 2
  "#c04124",
  #red 3
  "#005c75",
  #teal 4
  "#a2c62b",
  #light green 5
  "#702e01",
  #brown 6
  "#ff590d",
  #orange 7
  "#c4c4c7",
  #grey 8
  "#007f39" #dark green 9
)

# a4_dim

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

#' @title A4 useable height.
#' @description The height of useable space within an a4 sheet.
#' @return A numeric value.
#' @export
a4_height_mm <- 257

#' @title Convert column names to sentence case
#' @description A function to convert colnames to snakecase and then to sentence case to be used in functions for making hover values.
#' @param data The number of digits to round the legend labels.

#' @return A numeric value.
#' @export
sentence_colnames <- function(data) {
  
  tmp <- janitor::clean_names(data) 
  
  colnames(tmp) <-  stringr::str_replace_all(stringr::str_to_sentence(colnames(tmp)), "_", " ")
  
  tmp
}


