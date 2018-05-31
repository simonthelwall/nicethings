#' nice_month_factor
#'
#' One of the problems with the Gregorian calendar is that months are not alphabetical.
#' In R the default sorting of strings is in alphabetical order.
#' For tables and graphs of data by month this may mean repeated creation of month variables, which is tedious.
#'
#' @param x A month string, will take either abbreviated or full length month strings
#' @param abbreviated_out Should the output be abbreviated? default = FALSE
#' @param year_type string, either calendar or financial (year starts April)
#' @return A factor with months in order as the occur in the year.
#' @examples
#' nice_month_string(x = "Dec", abbreviated_out = FALSE, year_type = "calendar")
#' @export

nice_month_string <- function(x, abbreviated_out = FALSE, year_type = "calendar"){
  assertthat::assert_that(is.character(x), msg = "x must be a character string")
  assertthat::assert_that(is.character(year_type) == TRUE,
                          msg = "year_type must be a character string")
  year_type <- tolower(year_type)

  abbreviated_in <- ifelse(max(nchar(x)) > 3, FALSE, TRUE)

  month_levels <- ifelse(abbreviated_out == TRUE & year_type == "calendar",
                         c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                           "Aug", "Sep", "Oct", "Nov", "Dec", NA),
                         ifelse(abbreviated_out == FALSE & year_type == "calendar",
                                c("January", "February", "March", "April", "May",
                                  "June", "July", "August", "September",
                                  "October", "November", "December", NA),
                                ifelse(abbreviated_out == TRUE & year_type == "financial",
                                       c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", NA),
                                       ifelse(abbreviated_out == FALSE & year_type == "financial",
                                              c("January", "February", "March", "April", "May",
                                              "June", "July", "August", "September",
                                              "October", "November", "December",
                                              NA)))))
  x <- ifelse(abbreviated_in == TRUE & abbreviated_out == FALSE,
              dplyr::case_when(
                x == "Jan" ~ "January",
                x == "Feb" ~ "February",
                x == "Mar" ~ "March",
                x == "Apr" ~ "April",
                x == "May" ~ "May",
                x == "Jun" ~ "June",
                x == "Jul" ~ "July",
                x == "Aug" ~ "August",
                x == "Sep" ~ "September",
                x == "Oct" ~ "October",
                x == "Nov" ~ "November",
                x == "Dec" ~ "December",
                TRUE ~ NA_character_
              ))

  z <- ifelse(abbreviated_out == TRUE & abbreviated_in == TRUE,
              factor(x, levels = month_levels),
              ifelse(abbreviated_in == FALSE & abbreviated_out == TRUE,
                     factor(substr(x,1,3), levels = month_levels),
                     ifelse(abbreviated_in == TRUE & abbreviated_out == FALSE |
                            abbreviated_in == TRUE & abbreviated_out == TRUE,
                            factor(x, levels = month_levels), NA_character_
                            )))
  return(z)
}
