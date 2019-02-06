context("Test nowt function")

test_that("nowt works in dplyr", {
    library(dplyr)
    library(ggplot2)
    data(mtcars)
    expect_equal(
      mtcars %>%
        group_by(cyl) %>%
        summarise(mean(mpg)) %>%
        # rename(mean_mpg = mean(mpg)) %>%
        nowt(),
      mtcars %>%
        group_by(cyl) %>%
        summarise(mean(mpg))
    )
})

test_that("nowt works in ggplot", {
  library(dplyr)
  library(ggplot2)
  data(mtcars)
  expect_equal(
    ggplot(mtcars, aes(x = disp, y = mpg)) +
      geom_point() +
      # scale_x_continuous("Displacement") +
      nowt(),
    ggplot(mtcars, aes(x = disp, y = mpg)) +
      geom_point() +
      nowt()
  )
})
