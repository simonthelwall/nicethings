context("Test utilities functions")

test_that("nice_load works as expected", {
  x <- 1
  y <- 2
  save(x,y, file = paste0(tempdir(), "/temp.RData"))
  rm(x, y)
  nice_load(file = paste0(tempdir(), "/temp.RData"), "y")
  expect_true(exists("y"))
  nice_load(file = paste0(tempdir(), "/temp.RData"), "y", rename = "z")
  expect_true(exists("z"))
  expect_equal(z, 2)
  expect_equal(y, 2)
})
