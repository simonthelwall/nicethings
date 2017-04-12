context("nice_names")

dat <- data.frame(var.1 = 1, VAR2 = 2, `var 3` = 3, var_4_ = 4,
                  var_5__ = 5, `var6 ` = 6)

test_that("nice names removes what it should", {
  names(dat) <- nice_names(dat)
  expect_false(grepl("\\.", names(dat)[1]))
  expect_false(grepl("[A-Z]", names(dat)[2]))
  # expect_false(grepl("\\s", names(dat)[3]))
  expect_false(grepl("\\_$", names(dat)[4]))
  expect_false(grepl("\\_\\_", names(dat)[5]))
  # not sure how to test 6 since set up hasn't worked
})
