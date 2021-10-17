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
