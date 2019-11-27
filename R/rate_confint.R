#' Confidence interval for a rate calculation
#'
#' Calculates a 95 percent confidence interval for a rate using the Rothman/Greenland method described in \url{http://www.openepi.com/PDFDocs/PersonTime1Doc.pdf}.
#' @param numerator Count of events
#' @param denominator Count of person-years
#' @return A data frame with three columns giving the rate, lower confidence interval and upper confidence interval.
#' @examples
#' dat <- data.frame(n = 1, denom = 10)
#' rate_confint(dat$n, dat$denom)
#' dat <- data.frame(n = 5, denom = 25) # replicates output from openepi just fine.
#' rate_confint(dat$n, dat$denom)
#' @export

# Normal approximation
# rate_confint <- function(numerator, denominator){
#   rate <- numerator/denominator
#   lci <- rate - (
#     1.96 * sqrt(numerator/(denominator^2))
#   )
#   uci <- rate + (
#     1.96 * sqrt(numerator/(denominator^2))
#   )
#   return(as.data.frame(cbind(rate, lci, uci)))
# }

rate_confint <- function(numerator, denominator){
  if(!is.numeric(numerator) | !is.numeric(denominator)){
    stop("Numerator and denominator must be numeric")
  }
  if(numerator < 0 | denominator < 0){
    stop("Numerator and denominator must be positive")
  }
  rate <- numerator/denominator
  lci <- exp(
    log(rate) - 1.96 * (1/sqrt(numerator))
  )
  uci <- exp(
    log(rate) + 1.96 * (1/sqrt(numerator))
  )
  return(as.data.frame(cbind(rate, lci, uci)))
}

#' Confidence interval for a rate difference
#'
#' Calculates a 95 percent confidence interval for a rate difference.
#' Uses Kirkwood and Sterne's formula for a rate difference, p241 of 2nd Ed.
#' I think this could be re-written to work better so that it takes a data.frame as an input and compares against a given baseline.
#'
#' @param numerator1 The numerator for the first level for comparison (e.g. the unexposed group)
#' @param denominator1 The denominator for the first level for comparison
#' @param numerator2 The numerator for the second level for comparison (e.g. the exposed group)
#' @param denominator2 The denominator for the second level for comparison
#' @return A dataframe with columns for rate difference, upper and lower confidence intervals
#' @examples
#' rate_difference(33, 355, 24, 518) # matches output from p241 K&S, 2nd Ed
#' @export

rate_difference <- function(numerator1, denominator1, numerator2, denominator2){
  r1 <- numerator1/denominator1
  r2 <- numerator2/denominator2
  rd <- r1 - r2
  se <- sqrt((numerator1 / denominator1^2) +
               (numerator2 / denominator2^2))
  lci <- rd - (1.96*se)
  uci <- rd + (1.96*se)
  return(as.data.frame(cbind(rd, lci, uci)))
}
