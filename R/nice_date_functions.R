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

   if(abbreviated_out == TRUE & year_type == "calendar"){
     month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                       "Aug", "Sep", "Oct", "Nov", "Dec", NA)
   }else if(abbreviated_out == FALSE & year_type == "calendar"){
     month_levels <- c("January", "February", "March", "April", "May",
                      "June", "July", "August", "September",
                      "October", "November", "December", NA)
   }else if(abbreviated_out == TRUE & year_type == "financial"){
     month_levels <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", NA)
   }else if(abbreviated_out == FALSE & year_type == "financial"){
     month_levels <- c( "April", "May",
                       "June", "July", "August", "September",
                       "October", "November", "December",
                       "January", "February", "March",
                       NA)
   }

    # month_levels <- ifelse(abbreviated_out == TRUE & year_type == "calendar",
    #                      c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
    #                        "Aug", "Sep", "Oct", "Nov", "Dec", NA),
    #                      ifelse(abbreviated_out == FALSE & year_type == "calendar",
    #                             c("January", "February", "March", "April", "May",
    #                               "June", "July", "August", "September",
    #                               "October", "November", "December", NA),
    #                             ifelse(abbreviated_out == TRUE & year_type == "financial",
    #                                    c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", NA),
    #                                    ifelse(abbreviated_out == FALSE & year_type == "financial",
    #                                           c("January", "February", "March", "April", "May",
    #                                           "June", "July", "August", "September",
    #                                           "October", "November", "December",
    #                                           NA)))))

  if(abbreviated_in == TRUE & abbreviated_out == FALSE){
    x <- dplyr::case_when(
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
    )
  }
  # x <- ifelse(abbreviated_in == TRUE & abbreviated_out == FALSE,
  #             dplyr::case_when(
  #               x == "Jan" ~ "January",
  #               x == "Feb" ~ "February",
  #               x == "Mar" ~ "March",
  #               x == "Apr" ~ "April",
  #               x == "May" ~ "May",
  #               x == "Jun" ~ "June",
  #               x == "Jul" ~ "July",
  #               x == "Aug" ~ "August",
  #               x == "Sep" ~ "September",
  #               x == "Oct" ~ "October",
  #               x == "Nov" ~ "November",
  #               x == "Dec" ~ "December",
  #               TRUE ~ NA_character_
  #             ), x)

  # x <- ifelse(abbreviated_in == FALSE & abbreviated_out == TRUE,
  #             substr(x,1,3), x)
  if(abbreviated_in == FALSE & abbreviated_out == TRUE){
    x <- substr(x,1,3)
  }

  z <- factor(x, levels = month_levels)

  # z <- ifelse(abbreviated_out == TRUE & abbreviated_in == TRUE,
  #             factor(x, levels = month_levels),
  #             ifelse(abbreviated_in == FALSE & abbreviated_out == TRUE,
  #                    factor(substr(x,1,3), levels = month_levels),
  #                    ifelse(abbreviated_in == TRUE & abbreviated_out == FALSE |
  #                           abbreviated_in == TRUE & abbreviated_out == TRUE,
  #                           factor(x, levels = month_levels), NA_character_
  #                           )))
  return(z)
}

#' Nicely print a year in a given format
#'
#' @param x A year variable
#' @param year_format The current format of x, one of "fyear6", "fyear4", "cyear2"
#' @return A string formatted year
#' @examples
#' x <- 201516
#' nice_year(x, "fyear6")
#' nice_year(97, "cyear2")
#' nice_year(12, "cyear2")
#' nice_year("0708", "fyear4") # this fails - is it supposed to?
#' nice_year("07/08", "fyear4")
#' @export



nice_year <- function(x, year_format){
  x <- gsub("[:alpha:] | [:punct:] | [:space:]" , "", x)
  x <-gsub("/", "", x)

  year_format_list <- c("fyear6", "fyear4", "cyear2")
  stopifnot(year_format %in% year_format_list)

  if(length(x > 1 & length(year_format) == 1)){
    year_format <- rep(year_format, length(x))
  }

  z <- ifelse(year_format == "fyear6",
              paste0(substr(x, 1,4), "/", substr(x, 5, 6)),
              ifelse(year_format == "fyear4",
                     ifelse(substr(x, 1, 2) < 90,
                            paste0(20, substr(x, 1,2), "/", substr(x, 3, 4)),
                            paste0(19, substr(x, 1,2), "/", substr(x, 3, 4))),
                     ifelse(year_format == "cyear2",
                            ifelse(x > 90, paste0("19", x), paste0("20", x)),
                            NA)
              )
  )
  return(z)
}
