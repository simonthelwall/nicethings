#' removes data frames from global environment
#'
#' For those occaisions when you want to clear all your data frames out of memory without also removing any functions you may have loaded in.
#' Adapted from \url{http://stackoverflow.com/questions/19684819/get-list-of-available-data-frames}
#'
#' @examples
#' dat <- as.data.frame(x = rnorm(3), y = rnorm(3))
#' clear_dataframes()
#' ls()
#' @export

clear_dataframes <- function(){
  rm(list = names(which(unlist(eapply(.GlobalEnv, is.data.frame)))),
     envir = .GlobalEnv)
}
