context("Difference in means")

test_that("Function stops if length(unique(by)) != 2", {
  dat <- data.frame(x = c(1:5), y = rep(1, 5))
  expect_error(nice_diff_means(data = dat, var = "x", by = "y"))
  dat <- data.frame(x = c(1:6), y = c(1,1,2,2,3,3))
  expect_error(nice_diff_means(data = dat, var = "x", by = "y"))
})

test_that("Function stops if var is not numeric", {
  dat <- data.frame(x = as.character(c(1:6)), y = c(1,1,1,2,2,2))
  expect_error(nice_diff_means(data = dat, var = "x", by = "y"))
})

test_that("Function works on numeric data, with two groups", {
  dat <- data.frame(x = rep(3, 6), y = c(1,1,1,2,2,2))
  expect_equal(nice_diff_means(data = dat, var = "x", by = "y"), 0)
})
