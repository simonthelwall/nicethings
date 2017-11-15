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
  # stopifnot(length(unique(data[[by]])) == 2,
  #           error = stop("There can only be two elements in the grouping variable"))
  # stopifnot(is.numeric(data[[var]]) == TRUE,
  #           error = stop(paste0(data, "$", var, " must be numeric")))

  by_1 <- unique(data[[by]])[1]
  by_2 <- unique(data[[by]])[2]

  mean_1 <- mean(data[[var]][ data[[by]] == by_1 ], na.rm = TRUE)
  mean_2 <- mean(data[[var]][ data[[by]] == by_2 ], na.rm = TRUE)

  diff_in_means <- mean_1 - mean_2
  # need nice rounding
  return(diff_in_means)
  # add ci too.
}
