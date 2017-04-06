#' Confidence interval for a rate calculation
#'
#' Calculates a 95%CI confidence interval for a rate using the Rothman/Greenland method described in \url{http://www.openepi.com/PDFDocs/PersonTime1Doc.pdf}.
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
  rate <- numerator/denominator
  lci <- exp(
    log(rate) - 1.96 * (1/sqrt(numerator))
  )
  uci <- exp(
    log(rate) + 1.96 * (1/sqrt(numerator))
  )
  return(as.data.frame(cbind(rate, lci, uci)))
}
