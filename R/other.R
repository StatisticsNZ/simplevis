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

#' @title Colour palette for categorical variables.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_set1 <- c("#377EB8", "#A65628", "#F781BF", "#4DAF4A", "#FF7F00", "#984EA3", "#FFFF33", "#E41A1C", "#999999") #from Set1, 9col

#' @title Colour palette for categorical variables.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_snz_trend5 <- c("#35345D", "#0D94A3", "#D3D3D3", "#AE4E51", "#6F2E38")

#' @title Colour palette for categorical variables.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_snz_trend3 <- c("#0D94A3", "#C4C4C7", "#AE4E51")

#' @title Colour palette for categorical variables.
#' @description Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_trend3 <- c("#4575B4", "#C4C4C7", "#D73027")

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
pal_ea19 <- c("#172a45", "#00b2c3", "#c04124", "#005c75", "#a2c62b", "#702e01", "#ff590d", "#c4c4c7", "#007f39")

#' @title Colour palette for categorical variables.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_ea19_trend5 <- c("#172a45", "#00b2c3", "#c4c4c7", "#c04124", "#702e01")

#' @title Colour palette for categorical variables.
#' @description  Colour palette for categorical variables.
#' @return A vector of hex codes.
#' @export
pal_ea19_trend3 <- c("#00b2c3", "#c4c4c7", "#c04124")

#' @title pal_snz_nztcs_category
#' @description A colour palette used for depicting categories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_snz_nztcs_category <- c("Threatened" = pal_snz[6], "At risk" = pal_snz[3], "Not threatened" = pal_snz[7], "Data deficient" = "#c4c4c7")

#' @title pal_snz_nztcs_subcategory_change
#' @description A colour palette used for depicting changes in subcategories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_snz_nztcs_subcategory_change <- c("Actual decline" = pal_snz[3], "Actual improvement" = pal_snz[7])

#' @title pal_snz_nztcs_subcategory
#' @description A colour palette used for depicting subcategories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_snz_nztcs_subcategory <- c(
  "Nationally critical" = "#6f2e38", "Nationally endangered" = "#813641", "Nationally vulnerable" = "#933d4a",
  "Declining" = "#ae4e51", "Recovering" = "#b75e61", "Relict" = "#bf7073", "Naturally uncommon" = "#c78284",
  "Not threatened" = "#0d94a3", "Data deficient" = "#c4c4c7"
)

#' @title pal_snz_nztcs_category
#' @description A colour palette used for depicting categories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_ea19_nztcs_category <- c("Threatened" = pal_ea19[6], "At risk" = pal_ea19[3], "Not threatened" = pal_ea19[2], "Data deficient" = pal_ea19[8])

#' @title pal_ea19_nztcs_subcategory_change
#' @description A colour palette used for depicting categories in the NZ conservation threat status.
#' @return A vector of hex codes.
#' @export
pal_ea19_nztcs_subcategory_change <- c("Actual decline" = pal_ea19[3], "Actual improvement" = pal_ea19[2])

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