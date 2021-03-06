% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nice_date_functions.R
\name{nice_year}
\alias{nice_year}
\title{Nicely print a year in a given format}
\usage{
nice_year(x, year_format)
}
\arguments{
\item{x}{A year variable}

\item{year_format}{The current format of x, one of "fyear6", "fyear4", "cyear2"}
}
\value{
A string formatted year
}
\description{
Nicely print a year in a given format
}
\examples{
x <- 201516
nice_year(x, "fyear6")
nice_year(97, "cyear2")
nice_year(12, "cyear2")
nice_year("0708", "fyear4") # this fails - is it supposed to?
nice_year("07/08", "fyear4")
}
