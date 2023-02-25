#' removes data frames from global environment
#'
#' For those occasions when you want to clear all your data frames out of memory without also removing any functions you may have loaded in.
#' Adapted from \url{http://stackoverflow.com/questions/19684819/get-list-of-available-data-frames}
#'
#' @examples
#' dat <- as.data.frame(x = rnorm(3), y = rnorm(3))
#' ls()
#' clear_dataframes()
#' ls()
#' @export

clear_dataframes <- function(){
  rm(list = names(which(unlist(eapply(.GlobalEnv, is.data.frame)))),
     envir = .GlobalEnv)
}

#' removes vectors from global environment
#'
#' For those occasions when you want to clear up your global environment and get rid
#' of all the vectors.
#'
#' @examples
#' vec <- c(1:3)
#' ls()
#' clear_vectors()
#' ls()
#' @export

clear_vectors <- function(){
  objs_vec <- names(which(unlist(eapply(.GlobalEnv, is.vector))))
  objs_list <- names(which(unlist(eapply(.GlobalEnv, is.list))))
  rm(list = (objs_vec[!(objs_vec %in% objs_list)]),
     envir = .GlobalEnv)
}
