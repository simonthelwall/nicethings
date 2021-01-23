#' Neil Pearce's multicollinearity
#'
#' Detect multicollinearity using Neal Pearce's change in root mean square error
#'
#' This function assess multicollinearity in a regression model using the
#' strategy described in Greenland, Daniel and Pearce, 2016.
#' It translates a procedure written in Excel by my friend Adrian Root.
#'
#' The function will take a regression model and a vector of covariates, and drop them
#' one-by-one.
#' The relative change in root mean square error (`rel_rmse`) is returned in a
#' dataframe, along with the regression co-efficients for the 'main' exposure of
#' interest.
#'
#' A value of less than one indicates that the root mean square error is larger
#' in the reduced model than the full model, indicating that confounding by the
#' dropped covariate is greater than multicollinearity.
#' Conversely, a value less than one indicates the possibility of
#' multicollinearity.
#'
#' Since the function repeatedly re-fits a regression model, as many times as
#' there are covariates there are to test, it is likely that the function will
#' take a long time to run when large datasets are being used.
#'
#' @param model A regression model
#' @param main_effect A string variable giving the main exposure of interest
#' @param covariates A vector of strings giving the covariates to assess for multicollinearity as a
#'
#' @return A dataframe
#'
#' @export
#'
#' @examples
#'
#' data(mtcars)
#'
#' mt_m <- lm(mpg ~ ., data = mtcars) # fit regression model with all covariates
#' mt_vars <- names(mtcars) # create vector of covariates to examine for collinearity
#' mt_vars <- mt_vars[!mt_vars %in% c("mpg", "wt")] # drop the outcome and main effect
#'
#' np_multicol(model = mt_m, main_effect = "wt", covariates = mt_vars)

np_multicol <- function(model, main_effect, covariates){
  assertthat::assert_that(is.character(main_effect),
                          msg = "main_effect must be a character string")
  # assertthat::assert_that(is.list(covariates),
  #                         msg = "covariates must be a list")

  z <- purrr::map_df(.x = covariates, .f = model_reduction_function,
              model = model, main_effect = main_effect)
  return(z)
}

model_reduction_function <- function(model, main_effect, single_covariate){
  beta_ful <- stats::coef(model)[main_effect]
  se_ful <- stats::coef(summary(model))[, "Std. Error"][main_effect]
  m2 <- stats::update(model, glue::glue(". ~ . -{single_covariate}"))
  beta_red <- stats::coef(m2)[main_effect]
  se_red <- stats::coef(summary(m2))[, "Std. Error"][main_effect]

  # =SQRT((C2-C$2)^2 + C3^2)
  rmse_ful <- sqrt((beta_ful - beta_ful)^2 + se_ful^2)
  rmse_red <- sqrt((beta_red - beta_ful)^2 + se_red^2)

  rel_rmse <- rmse_red / rmse_ful
  z <- data.frame("main_effect" = main_effect, "dropped_var" = single_covariate,
                  "beta_full" = beta_ful, "beta_reduced" = beta_red,
                  "std_err_full" = se_ful, "std_err_red" = se_red,
                  "rel_rmse" = rel_rmse, row.names = NULL,
                  stringsAsFactors = FALSE)
  return(z)
}
