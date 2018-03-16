context("nice_names")

dat <- data.frame(var.1 = 1, VAR2 = 2, `var 3` = 3, var_4_ = 4,
                  var_5__ = 5, `var6 ` = 6, `var-7` = 7, `var,8` = 8,
                  `var&9` = 9, `var(10` = 10, `var)11` = 11,
                  check.names = FALSE)

test_that("nice names removes what it should", {
  names(dat) <- nice_names(dat)
  expect_false(grepl("\\.", names(dat)[1]))
  expect_false(grepl("[A-Z]", names(dat)[2]))
  # expect_false(grepl("\\s", names(dat)[3]))
  expect_false(grepl("\\_$", names(dat)[4]))
  expect_false(grepl("\\_\\_", names(dat)[5]))
  expect_false(grepl(" ", names(dat)[6]))
  expect_false(grepl("-", names(dat)[7]))
  expect_false(grepl(",", names(dat)[8]))
  expect_false(grepl("\\&", names(dat)[9]))
  expect_false(grepl("\\(", names(dat)[10]))
  expect_false(grepl("\\)", names(dat)[11]))
})



test_that("nice_names_pipe works in a pipe", {

  dat <- data.frame(var.1 = 1, VAR2 = 2, `var 3` = 3, var_4_ = 4,
                    var_5__ = 5, `var6 ` = 6, `var-7` = 7, `var,8` = 8,
                    `var&9` = 9, `var(10` = 10, `var)11` = 11,
                    var12 = 12,
                    check.names = FALSE)
  names(dat)[12] <- paste(stringi::stri_unescape_unicode("\\u00ef"), "var12",
                          sep = "_")

  `%>%` <- magrittr::`%>%`
  dat <- dat %>%
    pipe_nice_names()
  expect_false(grepl("\\.", names(dat)[1]))
  expect_false(grepl("[A-Z]", names(dat)[2]))
  # expect_false(grepl("\\s", names(dat)[3]))
  expect_false(grepl("\\_$", names(dat)[4]))
  expect_false(grepl("\\_\\_", names(dat)[5]))
  expect_false(grepl(" ", names(dat)[6]))
  expect_false(grepl("-", names(dat)[7]))
  expect_false(grepl(",", names(dat)[8]))
  expect_false(grepl("\\&", names(dat)[9]))
  expect_false(grepl("\\(", names(dat)[10]))
  expect_false(grepl("\\)", names(dat)[11]))
  expect_equal(names(dat)[12], "var12")
})

# test_that("pipeable_nice_names works on known mrsa pir bug", {
#   temp_dat <- structure(list(`a ‘timeline’ for patient movement` = character(0)),
#                         .Names = c("a ‘timeline’ for patient movement"),
#                         row.names = integer(0),
#                         class = c("tbl_df", "tbl", "data.frame"))
#   temp_dat %>% pipe_nice_names()
#   pipeable_nice_names(names(temp_dat))
# })
