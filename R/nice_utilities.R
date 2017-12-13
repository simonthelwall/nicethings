#' Format variable names into standard format.
#'
#' Turns column names of data frames into standardised format.
#' Dots are replaced with underscores, trailing underscores are removed and everything is made lower case.
#' Punctuation and spaces are also replaced by underscores and double underscores are removed last
#'
#' @param x A data frame
#' @examples
#' data(mtcars)
#' names(mtcars) <- toupper(names(mtcars))
#' names(mtcars) <- nice_names(mtcars)
#' @export

nice_names <- function(x){
  names(x) <- gsub(pattern = "[[:punct:]]", "\\_", tolower(names(x)))
  names(x) <- gsub("\\.", "\\_", tolower(names(x)))
  names(x) <- gsub("\\s", "\\_", names(x)) # get rid of white space
  names(x) <- gsub("\\_$", "", names(x)) # remove trailing underscores.
  names(x) <- gsub("-", "\\_", names(x)) # remove hyphens
  names(x) <- gsub(" ", "\\_", names(x)) # remove spaces
  names(x) <- gsub("\\_{2,}", "\\_", names(x)) # where multiple underscores occur, are replaced by one.
}

#' A function to load a single object from an R data file
#'
#' \code{load()} loads all objects in the data file which is fine for most of the uses.
#' However, one may wish to load only a single object, if, for example, one has conflicting object names in the same environment.
#' This function also allows the user to rename an object within the data frame.
#'
#' @example
#' x <- 1
#' y <- 2
#' save(x,y, file = paste0(tempdir(), "/temp.RData"))
#' rm(x, y)
#' nice_load(file = paste0(tempdir(), "/temp.RData"), "y")
nice_load <- function(file, object, rename = NULL){
  load(file)

  # drop everything that is not the desired object and is not rename
  rm(list = ls()[!(ls() %in% c(object, rename) )])


  if(!is.null(rename)){
    # create object in local env that has name matching value for object, with new name same as rename
    assign(ls()[ls() == eval(object)], rename)
    return(get(eval(quote(rename))))
  }
  else{
    # return what is left, that is not rename
    return(get(ls()))
  }
}
