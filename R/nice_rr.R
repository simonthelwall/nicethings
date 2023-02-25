#' nice_rr
#'
#' Calculate risk ratios from line-list data
#'
#' @param n1 numerator for the reference group
#' @param d1 denominator for the reference group
#' @param n2 numerator for the comparator group
#' @param d2 denominator for the comparator group
#'
#' @examples
#' data(smokers)
#' lc_agg <- smokers %>%
#'   dplyr::group_by(smoker) %>%
#'   dplyr::summarise(denominator = dplyr::n(), numerator = sum(lung_cancer)) %>%
#'   tidyr::pivot_wider(names_from = smoker, values_from = c(denominator, numerator))
#' lc_agg
#' nice_rr(n1 = lc_agg$numerator_0, d1 = lc_agg$denominator_0,
#'     n2 = lc_agg$numerator_1, d2 = lc_agg$denominator_1)
#' @export

nice_rr <- function(n1, d1, n2, d2){

  rr    <- ( (n2/d2) / (n1/d1) )
  se_log_rr <-  sqrt( abs((1/d2) - (1/n2) + (1/d1) - (1/n1)) )
  lci   <- rr / exp(1.96 * se_log_rr)
  uci   <- rr * exp(1.96 * se_log_rr)

  z <- cbind.data.frame(rr, se_log_rr, lci, uci)
  return(z)
}
