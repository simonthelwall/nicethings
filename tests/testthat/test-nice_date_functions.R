context("Test nice_date_functions")

test_that("assertions work", {
  expect_error(nice_month_string(x = 7, abbreviated_out = FALSE, year_type = "calendar"))
  expect_error(nice_month_string(x = "Dec", abbreviated_out = FALSE, year_type = 7))
})

test_that("nice_month_string returns a factor", {
  expect_true(is.factor(nice_month_string(x = "Dec", abbreviated_out = FALSE,
                                          year_type = "calendar")))
})

test_that("nice_month_string is vectorised", {
  input <- c("Dec", "Jan")
  expect_equal(
    nice_month_string(x = input,
                      abbreviated_out = FALSE, year_type = "calendar"),
    factor(c("December", "January"), levels = c("January", "February", "March", "April",
                                    "May", "June", "July", "August",
                                    "September", "October", "November",
                                    "December" ))
    )

  library(dplyr)
  test_dat <- data.frame(input = c("Dec", "Jan"), stringsAsFactors = FALSE)
  test_out <- data.frame(input = c("Dec", "Jan"),
                         output = factor(c("December", "January"),
                                         levels = c("January", "February", "March", "April",
                                                    "May", "June", "July", "August",
                                                    "September", "October", "November",
                                                    "December" )),
                         stringsAsFactors = FALSE)
  expect_equal(
    test_dat <- test_dat %>%
      mutate(output = nice_month_string(x = input,
                                        abbreviated_out = FALSE, year_type = "calendar")),
    test_out
    )
})

test_that("Input and output permutations work", {
  expect_equal(nice_month_string(x = "Dec", abbreviated_out = TRUE,
                                 year_type = "calendar"),
               factor("Dec", levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                        "Nov", "Dec")))
  expect_equal(nice_month_string(x = "Dec", abbreviated_out = TRUE,
                                 year_type = "financial"),
               factor("Dec", levels = c("Apr", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                        "Nov", "Dec", "Jan", "Feb", "Mar")))
  expect_equal(nice_month_string(x = "December", abbreviated_out = TRUE,
                                 year_type = "calendar"),
               factor("Dec", levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                        "Nov", "Dec")))
  expect_equal(nice_month_string(x = "December", abbreviated_out = TRUE,
                                 year_type = "financial"),
               factor("Dec", levels = c("Apr", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                        "Nov", "Dec", "Jan", "Feb", "Mar")))


  # abbreviated out false
  expect_equal(nice_month_string(x = "Dec", abbreviated_out = FALSE,
                                 year_type = "calendar"),
               factor("December", levels = c("January", "February", "March", "April", "May",
                                        "June", "July", "August", "September",
                                        "October", "November", "December")))
  expect_equal(nice_month_string(x = "Dec", abbreviated_out = FALSE,
                                 year_type = "financial"),
               factor("December", levels = c( "April", "May",
                                        "June", "July", "August", "September",
                                        "October", "November", "December",
                                        "January", "February", "March")))
  expect_equal(nice_month_string(x = "December", abbreviated_out = FALSE,
                                 year_type = "calendar"),
               factor("December", levels = c("January", "February", "March", "April", "May",
                                        "June", "July", "August", "September",
                                        "October", "November", "December")))
  expect_equal(nice_month_string(x = "December", abbreviated_out = FALSE,
                                 year_type = "financial"),
               factor("December",
                      levels = c("April", "May",
                                 "June", "July", "August", "September",
                                 "October", "November", "December",
                                 "January", "February", "March")))

})

context("Test nice_year")

test_that("stopifnot works for nice_year", {
  expect_error(nice_year(97, "aardvark"))
})

test_that("nice_year output is of expected length", {
  # fyear6
  expect_equal(nice_year(201516, "fyear6"), "2015/16")
  # fyear4
  expect_equal(nice_year("07/08", "fyear4"), "2007/08")
  expect_equal(nice_year("0708", "fyear4"), "2007/08")
  expect_equal(nice_year("9798", "fyear4"), "1997/98")
  # cyear2
  expect_equal(nice_year(97, "cyear2"), "1997")
  expect_equal(nice_year("02", "cyear2"), "2002")
})
