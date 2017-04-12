context("Rate calculations")

# test_that("Lower confidence interval for a rate cannot be less than zero"{
#   expect_false(rate_confint(-1, 10000)$lci < 0)
# })

test_that("Incorrect inputs are caught", {
  expect_error(rate_confint(5, "10"), "Numerator and denominator must be numeric")
  expect_error(rate_confint(-1, 10), "Numerator and denominator must be positive")
  expect_error(rate_confint(-1, 10), "Numerator and denominator must be positive")
})
