context("Rate calculations")

# test_that("Lower confidence interval for a rate cannot be less than zero"{
#   expect_false(rate_confint(-1, 10000)$lci < 0)
# })

test_that("Incorrect inputs are caught", {
  expect_error(rate_confint(5, "10"), "Numerator and denominator must be numeric")
  expect_error(rate_confint(-1, 10), "Numerator and denominator must be positive")
  expect_error(rate_confint(1, -10), "Numerator and denominator must be positive")
})

test_that("rate_confint returns expected outputs", {
  expect_true(is.data.frame(rate_confint(5, 25)))
  expect_equal(rate_confint(5, 25)$rate, 0.2, tolerance = .002)
  expect_equal(rate_confint(5, 25)$lci, 0.0832, tolerance = .002)
  expect_equal(rate_confint(5, 25)$uci, 0.4805, tolerance = .002)
})

test_that("Rate difference output returns expected value from Kirkwood and Sterne, p241",{
  expect_equal(rate_difference(33, 355, 24, 518)$rd, 0.0466257)
  expect_equal(rate_difference(33, 355, 24, 518)$lci, 0.009889584)
  expect_equal(rate_difference(33, 355, 24, 518)$uci, 0.08336182)
})

