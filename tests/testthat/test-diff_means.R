context("Difference in means")

test_that("nice_diff_means stops if length(unique(by)) != 2", {
  dat <- data.frame(x = c(1:5), y = rep(1, 5))
  expect_error(nice_diff_means(data = dat, var = "x", by = "y"))
  dat <- data.frame(x = c(1:6), y = c(1,1,2,2,3,3))
  expect_error(nice_diff_means(data = dat, var = "x", by = "y"))
})

test_that("nice_diff_means stops if var is not numeric", {
  dat <- data.frame(x = as.character(c(1:6)), y = c(1,1,1,2,2,2))
  expect_error(nice_diff_means(data = dat, var = "x", by = "y"))
})

test_that("nice_diff_means works on numeric data, with two groups", {
  dat <- data.frame(x = rep(3, 6), y = c(1,1,1,2,2,2))
  expect_equal(nice_diff_means(data = dat, var = "x", by = "y"), 0)
})

test_that("nice_diff_means returns something close to the built in t.test function", {
  data(sleep)
  sleep$group <- as.numeric(sleep$group)
  expect_equal(abs(nice_diff_means(data = sleep, var = "extra", by = "group")),
               abs(t.test(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2])$estimate[[1]] -
                     t.test(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2])$estimate[[2]] )
  )
})

# se_diff_means ####

test_that("se_diff_means stops if length(unique(by)) != 2", {
  dat <- data.frame(x = c(1:5), y = rep(1, 5))
  expect_error(se_diff_means(data = dat, var = "x", by = "y"))
  dat <- data.frame(x = c(1:6), y = c(1,1,2,2,3,3))
  expect_error(se_diff_means(data = dat, var = "x", by = "y"))
})

test_that("se_diff_means stops if var is not numeric", {
  dat <- data.frame(x = as.character(c(1:6)), y = c(1,1,1,2,2,2))
  expect_error(se_diff_means(data = dat, var = "x", by = "y"))
})

test_that("se_diff_means works on numeric data, with two groups", {
  dat <- data.frame(x = rep(3, 6), y = c(1,1,1,2,2,2))
  expect_equal(se_diff_means(data = dat, var = "x", by = "y"), 0)
})

# ci_diff_means ####

test_that("ci_diff_means returns result equivalent to Kirkwood and Sterne", {
  dat <- data.frame(weight = c(3.18, 2.74, 2.90, 3.27, 3.65, 3.42, 3.23, 2.86,
                               3.6, 3.65, 3.69, 3.53, 2.38, 2.34, 3.99, 3.89,
                               3.60, 3.73, 3.31, 3.7, 4.08, 3.61, 3.83, 3.41,
                               4.13, 3.36, 3.54, 3.51, 2.71),
                    group = c(rep(1, 14), rep(0, 15)))
  expect_equal(ci_diff_means(data = dat, var = "weight", by = "group"),
               "(-0.77--0.14)")
})

# Could do this within 10% by substr and as.numeric.
# for a later date.
# test_that("ci_diff_means returns something similar to built in t.test", {
#   data(sleep)
#   expect_equal(ci_diff_means(data = sleep, var = "extra", by = "group"),
#                "(-3.37-0.21)")
# })
