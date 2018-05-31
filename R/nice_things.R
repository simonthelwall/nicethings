#' Formats estimates and 95\% confidence intervals for nice printing.
#'
#' Rounds estimates to 1 decimal place and copies similarly formatted confidence intervals inside brackets.
#' If you want a \strong{really} good inline output from a regression model, see Benjamin Nutter's \code{\link[pixiedust]{dust_inline}} from his package \href{https://github.com/nutterb/pixiedust}{pixiedust}
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

#' Allows user to write examples of inline R code.
#'
#' Yihui gets the credit for this one.
#' \url{http://stackoverflow.com/questions/20409172/how-to-display-verbatim-inline-r-code-with-backticks-using-rmarkdown}
#' Writing text to give examples of inline r code is tricky.
#' This function should achieve that.
#' @param x A string of R code for printing as code, inline
#' @return A formatted string
#' @examples
#' nice_inline_r("round(3.77155454, 3)")
#' @export
nice_inline_r <- function(x){
  sprintf('``` `r %s` ```', x)
}

#' Formats dates to gov.uk requirements
#'
#' gov.uk dates are supposed to be formatted DD Month YYYY
#' This function takes a date and reformats it accordingly
#' @param x A date object
#' @return A string
#' @examples
#' nice_govuk_date(Sys.Date())
#' @export

nice_govuk_date <- function(x){
  assertthat::assert_that(assertthat::is.date(x))
  format(x, "%d %B %Y")
}

#' Easily calculate a percentage change
#'
#' Intended for inline printing, allows easy calculation of percentage changes
#'
#' @param start The value at the start
#' @param end The value at the end
#' @return A percentage change from start to end
#' @examples
#' nice_pc_change(5, 10)
#' @export

nice_pc_change <- function(start, end){
  assertthat::assert_that(is.numeric(start))
  assertthat::assert_that(is.numeric(end))
  z <- ((end - start) / end) * 100
  return(z)
}

#' nice_rversionstring
#'
#' Print a nice R version string without the date at the end
#'
#' @return A character string giving the R version
#' @examples nice_rversionstring()
#' @export

nice_rversionstring <- function(){
  z <- sub("\\s\\(.+", "", R.version.string)
  return(z)
}

#' yaml_today
#'
#' A truly lazy way to insert today's date into an Rmarkdown document.
#' To use, write date: nicethings::yaml_today() in the YAML header of an
#' Rmarkdown document.
#' Pinched from https://stackoverflow.com/a/25389694
#' @examples
#' \dontrun{
#'  # An example yaml header would go
#'  date: '`r nicethings::yaml_today()`'
#' }
#' @export

yaml_today <- function(){
  return(format(Sys.Date(), "%d %B %Y"))
}

