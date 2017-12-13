#' Difference in means
#'
#' Calculates a difference in means between two groups
#'
#' @param data A data frame
#' @param var A numeric variable containing the data for the calculation of the means
#' @param by A grouping variable with only two values
#' @return A numeric value for the difference in means
#'
#' @examples
#' data(sleep)
#' sleep$group <- as.numeric(sleep$group)
#' nice_diff_means(data = sleep, var = "extra", by = "group")
#' @export

nice_diff_means <- function(data, var, by){ # , ci = TRUE
  # check for two levels of `by`, then stop if not 2
  assertthat::assert_that(length(unique(data[[by]])) == 2,
                          msg = "There can only be two elements in the grouping variable")
  assertthat::assert_that(is.numeric(data[[var]]) == TRUE ,
                           msg = "var must be numeric")

  by_1 <- unique(data[[by]])[1]
  by_2 <- unique(data[[by]])[2]

  mean_1 <- mean(data[[var]][ data[[by]] == by_1 ], na.rm = TRUE)
  mean_2 <- mean(data[[var]][ data[[by]] == by_2 ], na.rm = TRUE)

  diff_in_means <- mean_1 - mean_2
  # need nice rounding
  return(diff_in_means)
  # add ci too. # need se function to do this
}

#' Standard error for difference in means
#'
#' Calculates the standard error for the difference in means between two groups
#'
#' @param data A data frame
#' @param var A numeric variable containing the data for the calculation of the means
#' @param by A grouping variable with only two values
#' @return A numeric value for the difference in means
#'
#' @examples
#' data(sleep)
#' sleep$group <- as.numeric(sleep$group)
#' se_diff_means(data = sleep, var = "extra", by = "group")
#'
#' # confirming against Kirkwood and Sterne
#' dat <- data.frame(weight = c(3.18, 2.74, 2.90, 3.27, 3.65, 3.42, 3.23, 2.86,
#'                                3.6, 3.65, 3.69, 3.53, 2.38, 2.34, 3.99, 3.89,
#'                                3.60, 3.73, 3.31, 3.7, 4.08, 3.61, 3.83, 3.41,
#'                                4.13, 3.36, 3.54, 3.51, 2.71),
#'                     group = c(rep(1, 14), rep(0, 15)))
#' se_diff_means(data = dat, var = "weight", by = "group")
#' @export

se_diff_means <- function(data, var, by){
  # sqrt(sd_mean1 + sd_mean2)

  # check for two levels of `by`, then stop if not 2
  assertthat::assert_that(length(unique(data[[by]])) == 2,
                          msg = "There can only be two elements in the grouping variable")
  assertthat::assert_that(is.numeric(data[[var]]) == TRUE ,
                          msg = "var must be numeric")

  by_1 <- unique(data[[by]])[1]
  by_2 <- unique(data[[by]])[2]

  sd_1 <- stats::sd(data[[var]][ data[[by]] == by_1 ], na.rm = TRUE)
  sd_2 <- stats::sd(data[[var]][ data[[by]] == by_2 ], na.rm = TRUE)

  step1_1 <- (sd_1^2)/length(data[[by]][data[[by]] == by_1])
  step1_2 <- (sd_2^2)/length(data[[by]][data[[by]] == by_2])

  se_diff <- sqrt(( step1_1 + step1_2 ))
  return(se_diff)
}

#' Confidence interval for difference in means
#'
#' Calulates a 95% CI for the difference between two means.
#' Uses an approximation to degrees of freedom of n observations - 2.
#' Thus, may give broader confidence intervals than built in \code{t.test()} function
#'
#' @param data A data frame
#' @param var A numeric variable containing the data for the calculation of the means
#' @param by A grouping variable with only two values
#' @return A numeric value for the difference in means
#'
#' @examples
#' data(sleep)
#' sleep$group <- as.numeric(sleep$group)
#' ci_diff_means(data = sleep, var = "extra", by = "group")
#' @export

ci_diff_means <- function(data, var, by){

  # check for two levels of `by`, then stop if not 2
  assertthat::assert_that(length(unique(data[[by]])) == 2,
                          msg = "There can only be two elements in the grouping variable")
  assertthat::assert_that(is.numeric(data[[var]]) == TRUE ,
                          msg = "var must be numeric")

  # proof that quantile function is what I want:
  # qnorm(0.025) == 1.96 (critical value for 5% two-tailed normal distribution)
  # also qt(0.025, 27) matches t' given p 65
  # qt is quantile function of t distribution
  # requires p = vector of probabilities
  #     df = degrees of freedom
  se <- se_diff_means(data, var, by)
  estimate <- nice_diff_means(data, var, by)
  t_crit <- abs(stats::qt(0.025, df = length(data[[by]]) - 2 ))
  lci <- estimate - (t_crit * se)
  uci <- estimate + (t_crit * se)
  z <- paste0("(", sprintf(lci, fmt = "%0.2f"), "-", sprintf(uci, fmt = "%0.2f"), ")")
  return(z)
}
