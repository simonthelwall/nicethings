% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nice_things.R
\name{up_or_down}
\alias{up_or_down}
\title{Calculate whether there has been either an increase, decrease or no change
between two values}
\usage{
up_or_down(from, to)
}
\arguments{
\item{from}{The stating value}

\item{to}{The final value}
}
\value{
Text string, one of "decrease", "increase" or "no change"
}
\description{
Calculate whether there has been either an increase, decrease or no change
between two values
}
\examples{
up_or_down(100, 200)
up_or_down(200, 100)
up_or_down(100, 100)
}
