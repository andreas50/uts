% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/which.R
\name{which}
\alias{which}
\alias{which.default}
\title{Generic which function}
\usage{
which(x, ...)

\method{which}{default}(x, ...)
}
\arguments{
\item{x}{an \R object.}

\item{\dots}{further arguments passed to or from methods.}
}
\description{
The function is needed, because \code{\link[base:which]{which}} of base \R is not generic.
}
\section{Methods (by class)}{
\itemize{
\item \code{default}: simply calls the default implementation of base \R
}}

\note{
As recommended in Section 7.1 ("Adding new generics") of "Writing R Extensions", the implementation of \code{\link{which.default}} has been made a wrapper around \code{\link[base:which]{base::which}}.
}
\keyword{internal}
