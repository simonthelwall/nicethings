#' Formats estimates and 95\% confidence intervals for nice printing.
#'
#' Rounds estimates to 1 decimal place and copies similarly formatted confidence intervals inside brackets.
#' @param estimate An estimate such as a rate ratio
#' @param lci The lower confidence interval
#' @param uci The upper confidence interval
#' @return A string in format d.d (95\% CI: d.d-d.d)
#' @examples
#' nice_estimate(100.111, 90.0, 110.000002)
#' nice_estimate(0.9, 0.8001, 0.95)
#' @export

nice_estimate <- function(estimate, lci, uci){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  estimate <- stringr::str_trim(sprintf("%7.1f", estimate))
  lci <- stringr::str_trim(sprintf("%7.1f", lci))
  uci <- stringr::str_trim(sprintf("%7.1f", uci))
  z <- paste0(estimate, " (95% CI:", lci, "-", uci, ")" )
  return(z)
}

#' Format variable names into standard format.
#'
#' Turns column names of data frames into standardised format.
#' Dots are replaced with underscores, trailing underscores are removed and everything is made lower case.
#'
#' @param x A data frame
#' @examples
#' data(mtcars)
#' names(mtcars) <- toupper(names(mtcars))
#' names(mtcars) <- nice_names(mtcars)
#' @export

nice_names <- function(x){
  names(x) <- gsub("\\.", "\\_", tolower(names(x)))
  names(x) <- gsub("\\_{2,}", "\\_", names(x)) # where multiple underscores occur, are replaced by one.
  names(x) <- gsub("\\s", "\\_", names(x)) # get rid of white space
  names(x) <- gsub("\\_$", "", names(x)) # remove trailing underscores.
}

#' Nicely formats percentages for printing
#'
#' Takes a denominator and a numerator, calculates a percentage and formats for printing in a markdown doc.
#' @param numerator The numerator
#' @param denominator The denominator
#' @return A string giving the percentage to one decimal place
#' @examples
#' nice_perc(5, 10)
#' @export
nice_perc <- function(numerator, denominator){
  pc <- (numerator/denominator) * 100
  pc <- sprintf("%.1f", pc)
  pc <- ifelse(pc == "0.0", "<0.1", pc)
  return(pc)
}

#' Formats p values for nice printing
#'
#' Takes a p value and formats it to three decimal places for printing.
#' @param x A p-value
#' @return A string where the p-value is formatted to three decimal places or "<0.001" where appropriate
#' @examples
#' nice_pval(0.045)
#' nice_pval(1e-05)
#' @export
nice_pval <- function(x){
  z <- ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))
  return(z)
}

#' Formats regression estimates and confidence intervals for nice printing.
#'
#' Takes estimates, lower and upper confidence intervals and formats them for printing in markdown
#' @param estimate An estimate
#' @param lci A lower confidence interval
#' @param uci An upper confidence interval
#' @return A string formatted with "d.dd (dd-dd)"
#' @examples
#' nice_estimate(3.546548, 1.215116, 17.60546)
#' @return
nice_estimate <- function(estimate, lci, uci){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  estimate <- stringr::str_trim(sprintf("%7.2f", estimate))
  lci <- stringr::str_trim(sprintf("%7.2f", lci))
  uci <- stringr::str_trim(sprintf("%7.2f", uci))
  z <- paste0(estimate, " (", lci, "-", uci, ")" )
  return(z)
}
