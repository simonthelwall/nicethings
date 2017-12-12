context("clear_dataframes")

dat <<- data.frame(x = 1, y = 2)
dat2 <<- dat
vec <<- c(1:3)
lis <<- list(a = 1, b = "one")
fun <<- function(x){x+1}

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
})
