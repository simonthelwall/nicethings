% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nice_sampsi.R
\name{nice_sampsi_cc}
\alias{nice_sampsi_cc}
\title{calculate sample size}
\usage{
nice_sampsi_cc(pi_0, or, power, alpha, ratio = 1)
}
\arguments{
\item{pi_0}{Proportion of controls exposed (0-1)}

\item{or}{Minimum odds ratio to be detected}

\item{power}{The power required (0-100\%)}

\item{alpha}{The alpha to be detected (0-100\%)}

\item{ratio}{Ratio of controls to cases}
}
\description{
Returns the total number of observations required in a case control study.
Uses the formulae on page 420 of Kirkwood and Sterne, 2nd Ed.
}
\details{
It should be noted that there is an error on p422 that is corrected here:
https://higheredbcs.wiley.com/legacy/college/kirkwood/0865428719/updates/updates.htm
The error is in the last line of step 3, where the print states 2.2769/0.17^2 = 179.4
The correction is 2.2769^2/0.17^2 = 179.4
}
\examples{
# Example 35.4, page 419 and 422 of Kirkwood and Sterne, 2nd Ed
# This gives a slightly lower result than Kirkwood and Sterne due to rounding.

nice_sampsi_cc(pi_0 = 0.4, or = 2, power = 90, alpha = 5)
}
