context("test nice_things.R")

# nice_estimate
test_that("nice_estimate returns expected values", {
  expect_equal(nice_estimate(0.9, 0.8001, 0.95) , "0.9 (95% CI:0.8-0.9)")
})

# nice_perc
test_that("nice_perc returns expected values", {
  expect_equal(nice_perc(5,10), "50.0")
  expect_equal(nice_perc(0.0000001,10), "<0.1")
})

# nice_pval
test_that("nice_pval returns expected values", {
  expect_equal(nice_pval(0.04916503), "0.049")
  expect_equal(nice_pval(0.00001943), "< 0.001")
})

# nice_inline_r
test_that("nice_inline_r returns expected value", {
  expect_equal(nice_inline_r("round(3.77155454, 3)"),
               "``` `r round(3.77155454, 3)` ```")
})

# nice_govuk_date
test_that("nice_gov_uk returns expected value", {
  expect_equal(nice_govuk_date(as.Date("01-01-1979", "%d-%m-%Y")),
               "01 January 1979")
  expect_error(nice_govuk_date("01-01-1979"))
})

# nice_pc_change
test_that("nice_pc_change returns expected value", {
  expect_equal(nice_pc_change(start = 5, end = 10), 50)
  expect_error(nice_pc_change(start = "5", end = 10))
  expect_error(nice_pc_change(start = 5, end = "10"))
})

# nice_rversionstring
test_that("nice_rversionstring returns expected value", {
  expect_true(is.character(nice_rversionstring()))
  expect_equal(substr(nice_rversionstring(), 1, 10), "R version ")
})
