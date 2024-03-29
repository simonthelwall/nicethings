#' Format column names into standard format.
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

#' Format variable names into standard format in a pipe.
#'
#' Turns column names of data frames into standardised format.
#' Dots are replaced with underscores, trailing underscores are removed and everything is made lower case.
#' Punctuation and spaces are also replaced by underscores and double underscores are removed last
#'
#' @param x A data frame
#' @return A data frame with nicely formatted names
#' @examples
#' \dontrun{
#' data(mtcars)
#' names(mtcars) <- toupper(names(mtcars))
#' mtcars %>% pipe_nice_names()
#' }
#' @export

pipe_nice_names <- function(x){
  assertthat::assert_that(is.data.frame(x))
  x <- x %>% dplyr::rename_with(.cols = dplyr::everything(),
                           .fn = pipeable_nice_names)
  return(x)
}

pipeable_nice_names <- function(x){
  x <- tolower(x)
  x <- gsub(pattern = "[[:punct:]]", "\\_", x)
  x <- gsub("\\.", "\\_", x)
  x <- gsub("\\s+", "\\_", x) # get rid of white space
  x <- gsub("-", "\\_", x) # remove hyphens
  x <- gsub(" ", "\\_", x) # remove spaces
  x <- gsub("\\/", "\\_", x) # remove forward slashes
  x <- gsub("\\&", "\\_", x) # remove ampersands
  x <- gsub("\\(", "\\_", x) # remove round bracket left
  x <- gsub("\\)", "\\_", x) # remove round bracket right
  x <- gsub("\\u2018", "\\_", x) # remove weird quote left
  x <- gsub("\\u2019", "\\_", x) # remove weird quote right
  # I have no idea why the following line requires a single backslash, where as the above lines cope with double
  x <- gsub("\u00ef", "\\_", x) # remove LATIN SMALL LETTER I WITH DIAERESIS
  x <- gsub("\\_{2,}", "\\_", x) # where multiple underscores occur
  x <- gsub("\\_$", "", x) # remove trailing underscores.
  x <- gsub("^\\_+", "", x) # remove any leading underscores
  return(x)
}

#' A function to load a single object from an R data file
#'
#' \code{load()} loads all objects in the data file which is fine for most of the uses.
#' However, one may wish to load only a single object, if, for example, one has conflicting object names in the same environment.
#' This function also allows the user to rename an object within the data frame.
#' Unfortunately, it is not possible to selectively load objects so this function loads every thing in the data file, and then drops what is not required.
#' Cribbed from https://stackoverflow.com/questions/8700619/get-specific-object-from-rdata-file
#'
#' @param file An .RData file
#' @param object An object known to be saved within the RData file
#' @param rename String, a new name for the object to take
#'
#' @examples
#' x <- 1
#' y <- 2
#' save(x,y, file = paste0(tempdir(), "/temp.RData"))
#' rm(x, y)
#' nice_load(file = paste0(tempdir(), "/temp.RData"), "y")
#' nice_load(file = paste0(tempdir(), "/temp.RData"), "y", rename = "z")
#' @export

nice_load <- function(file, object, rename = NULL){

  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # assertthat::assert_that(is.character(file), "file must be a string")
  # assertthat::assert_that(is.character(object), "object must be a string")
  # assertthat::assert_that((is.character(rename) | is.null(rename)), "rename must be a string or NULL")

  file_string <- stringr::str_replace(file, "^.*/", "")
  file_string <- stringr::str_replace(file, "\\.RData", "")

  # get data frame into local environment
  e = local({load(file); environment()})

  # make lazy-load database
  tools:::makeLazyLoadDB(e, file_string)
  lazyLoad(file_string)

  # load object
  get(object)

  if(!is.null(rename) ){
    # create object in local env that has name matching value for object, with new name same as rename
    assign(eval(rename), get(object), envir = .GlobalEnv)
    # assign(ls()[ls() == eval(object)], rename)
    rm(e)
    # return(get(eval(quote(rename))))
  }
  else{
    rm(e)
    assign(eval(object), get(object), envir = .GlobalEnv)
  }
}
