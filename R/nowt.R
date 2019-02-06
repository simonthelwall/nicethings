#' nowt
#'
#' Add a final link to a dplyr or ggplot chain to make testing chains nice.
#'
#' Credit for the function goes to Duncan Garmonsway and Hadley Wickham
#'
#' Garmonsway (2019, Jan. 31).
#' Duncan Garmonsway: Add nowt() to your tidy pipelines.
#' Retrieved from https://nacnudus.github.io/duncangarmonsway/posts/2019-01-31-add-nowt-to-your-tidy-pipelines/
#'
#' @author Duncan Garmonsway
#' @param x A dplyr or ggplot2 object
#'
#' @return a dplyr result or ggplot object that is the same as it went into the function.
#'
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' data(mtcars)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean(mpg)) %>%
#'   # rename(mean_mpg = mean(mpg)) %>%
#'   nowt()
#'
#' p <- ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   geom_point() +
#'   # scale_x_continuous("Displacement") +
#'   nowt()
#' p
#'
#' @export

nowt <- function(x = NULL) x
