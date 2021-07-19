#' nice_median
#'
#' Prints a median with interquartile range.
#' @param x A numeric vector
#' @param digits The number of digits to which values will be rounded
#' @return A median with interquartile range in brackets
#'
#' @examples
#' data(mtcars)
#' nice_median(mtcars$cyl)

nice_median <- function(x, digits = 1){
  med <- format(round(median(x, na.rm = TRUE), digits = digits), big.mark = ",")
  lqtile <- format(round(quantile(x, 0.25, na.rm = TRUE), digits = digits), big.mark = ",")
  uqtile <- format(round(quantile(x, 0.75, na.rm = TRUE), digits = digits), big.mark = ",")
  z <- paste0(med, " (IQR:", lqtile, "-", uqtile, ")")
  return(z)
}
