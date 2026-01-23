#' nice_risk_difference
#'
#' calculates a risk difference and confidence interval with a 95% confidence interval
#'
#' @param n1 Numerator 1
#' @param d1 Denominator 1
#' @param n2 Numerator 2
#' @param d2 Denominator 2
#'
#' @examples
#' # table 16.2 Kirkwood and Sterne
#' nice_risk_difference(n1 = 20, d1 = 240, n2 = 80, d2 = 220)
#' @export

nice_risk_difference <- function(n1, d1, n2, d2){
  rd <- (n1/d1) - (n2/d2)
  p1 <- (n1/d1)
  p2 <- (n2/d2)
  se_rd <- sqrt( ((p2*(1-p2))/d2) + ((p1*(1-p1))/d1) )
  lci <- rd - (1.96 * se_rd)
  uci <- rd + (1.96 * se_rd)
  return(list(rd = rd, se_rd = se_rd, lci = lci, uci = uci))
}

#' nice_risk_difference_d
#'
#' A dplyr version of [nice_risk_difference()]
#' The first row in the dataframe will always be used as the reference level,
#' so sort your dataframe appropriately.
#'
#' @examples
#'
#' kas_16_2 <- data.frame(num = c(20, 80), denom = c(240, 220))
#' kas_16_2 <- nice_risk_difference_d(kas_16_2, numerator = num, denominator = denom)
#' kas_16_2

nice_risk_difference_d <- function(dfrm, numerator, denominator){
  n1 <- dfrm %>% dplyr::filter(dplyr::row_number() == 1) %>% dplyr::pull({{ numerator }})
  d1 <- dfrm %>% dplyr::filter(dplyr::row_number() == 1) %>% dplyr::pull({{ denominator }})
  p1 <- n1/d1

  dfrm <- dfrm %>%
    dplyr::mutate(
      rd = (n1 / d1) - ({{numerator}} / {{denominator}}),
      p2 = ({{numerator}} / {{denominator}}),
      se_rd = sqrt( ((p2*(1-p2))/{{denominator}}) + ((p1*(1-p1))/d1) ),
      lci = rd - (1.96 * se_rd),
      uci = rd + (1.96 * se_rd)
           ) %>%
    dplyr::select(-p2)
  return(dfrm)
}

var_summary <- function(data, var) {
  data %>%
    dplyr::summarise(n = dplyr::n(), min = min({{ var }}), max = max({{ var }}))
}
mtcars %>%
  dplyr::group_by(cyl) %>%
  var_summary(mpg)
