test_that("answers are as expected", {
  expect_equal(nice_risk_difference(n1 = 20, d1 = 240, n2 = 80, d2 = 220)$rd,
               -0.280303, tolerance=1e-6)
  expect_equal(nice_risk_difference(n1 = 20, d1 = 240, n2 = 80, d2 = 220)$se_rd,
               0.03701524, tolerance=1e-6)
  expect_equal(nice_risk_difference(n1 = 20, d1 = 240, n2 = 80, d2 = 220)$lci,
               -0.3528529, tolerance=1e-6)
  expect_equal(nice_risk_difference(n1 = 20, d1 = 240, n2 = 80, d2 = 220)$uci,
               -0.2077532, tolerance=1e-6)
})
