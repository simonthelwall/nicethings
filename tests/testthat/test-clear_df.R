context("clearing objects from global environment")

dat <<- data.frame(x = 1, y = 2)
dat2 <<- dat
vec <<- c(1:3)
lis <<- list(a = 1, b = "one")
fun <<- function(x){x+1}
my_mat <- matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                 dimnames = NULL)

# tests failing due to "argument to 'which' is not logical", which doesn't occur otherwise
# Suspect this is because it which is looking in global environment and testthat may run in separate environment
# using <<- to assign to global env appears to resolve that.
test_that("clear_dataframes drops data frames", {
  clear_dataframes()
  expect_false(exists("dat"))
})

test_that("clear_dataframes does not drop other objects", {
  expect_true(exists("vec"), "TRUE")
  expect_true(exists("lis"), "TRUE")
  expect_true(exists("fun"), "TRUE")
  expect_true(exists("my_mat"), "TRUE")
})

# clear up the environment
rm(list = ls())
dat <<- data.frame(x = 1, y = 2)
dat2 <<- dat
vec <<- c(1:3)
lis <<- list(a = 1, b = "one")
fun <<- function(x){x+1}
my_mat <- matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                 dimnames = NULL)

test_that("clear_vectors drops data frames", {
  clear_vectors()
  expect_false(exists("vec"))
})

test_that("clear_dataframes does not drop other objects", {
  expect_true(exists("dat"), "TRUE")
  expect_true(exists("dat2"), "TRUE")
  expect_true(exists("lis"), "TRUE")
  expect_true(exists("fun"), "TRUE")
  expect_true(exists("my_mat"), "TRUE")
})
