#' calculate sample size
#'
#' Returns the total number of observations required in a case control study.
#' Uses the formulae on page 420 of Kirkwood and Sterne, 2nd Ed.
#'
#' It should be noted that there is an error on p422 that is corrected here:
#' https://higheredbcs.wiley.com/legacy/college/kirkwood/0865428719/updates/updates.htm
#' The error is in the last line of step 3, where the print states 2.2769/0.17^2 = 179.4
#' The correction is 2.2769^2/0.17^2 = 179.4
#'
#' @param pi_0 Proportion of controls exposed (0-1)
#' @param or Minimum odds ratio to be detected
#' @param power The power required (0-100\%)
#' @param alpha The alpha to be detected (0-100\%)
#' @param ratio Ratio of controls to cases
#'
#' @export
#' @examples
#' # Example 35.4, page 419 and 422 of Kirkwood and Sterne, 2nd Ed
#' # This gives a slightly lower result than Kirkwood and Sterne due to rounding.
#'
#' nice_sampsi_cc(pi_0 = 0.4, or = 2, power = 90, alpha = 5)

nice_sampsi_cc <- function(pi_0, or, power, alpha, ratio = 1){

  pi_1 <- (pi_0*or)/(1+pi_0*(or-1))

  u <- qnorm(power / 100)
  v <- qnorm(1-(alpha/100) / 2)

  pi_bar <- (pi_0 + pi_1)/2
  if(ratio != 1){
    n <- (( u*sqrt(pi_0*(1-pi_0) + pi_1*(1-pi_1)) + v*sqrt(2*pi_bar*(1-pi_bar)) )^2)/((pi_1-pi_0)^2)
    f = (ratio + 1) / (2*ratio)
    n <- ceiling(ceiling(n)*f) + ceiling(ceiling(n)*f*ratio)
  }else{
    n <- (( u*sqrt(pi_0*(1-pi_0) + pi_1*(1-pi_1)) + v*sqrt(2*pi_bar*(1-pi_bar)) )^2)/((pi_1-pi_0)^2)
    n <- ceiling(n) * 2
  }
  return(n)
}
