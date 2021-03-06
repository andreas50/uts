% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_methods.R
\name{Math.uts}
\alias{Math.uts}
\title{Math Group Methods for uts}
\usage{
\method{Math}{uts}(x, ...)
}
\arguments{
\item{x}{a \code{"uts"} object.}

\item{\dots}{further arguments passed to or from methods.}
}
\description{
Apply the \code{\link{Math}} methods in base \R{} to the observation values of \code{"uts"} objects.
}
\examples{
# Take the base-2 logarithm of the observation values and return the corresponding "uts"
log(ex_uts(), base=2)

# Calculate the cumulative sum of the observation values and return the corresponding "uts"
cumsum(ex_uts())
}
\seealso{
\code{\link{groupGeneric}}
}
