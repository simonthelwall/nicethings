context("clear_dataframes")

dat <- data.frame(x = 1, y = 2)
dat2 <- dat
vec <- c(1:3)
lis <- list(a = 1, b = "one")
fun <- function(x){x+1}

test_that("clear_dataframes drops data frames", {
  clear_dataframes()
  expect_false(exists(dat), "FALSE")
})

test_that("clear_dataframes does not drop other objects", {
  clear_dataframes()
  expect_true(exists(vec), "FALSE")
  expect_true(exists(lis), "FALSE")
  expect_true(exists(fun), "FALSE")
})
