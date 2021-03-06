#' Internal Functions
#'
#' The internal functions listed below might be of interest to developers seeking to extend the package functionality.
#' 
#' \code{uts} methods that exist primarily because they also work for \R's other time series classes:
#' \itemize{
#'   \item \code{\link{is.uts}}
#'   \item \code{\link{is.uts_virtual}}
#'   \item \code{\link{mean.uts}}
#'   \item \code{\link{median.uts}}
#'   \item \code{\link{sd.uts}}
#'   \item \code{\link{str.uts}}
#'   \item \code{\link{summary.uts}}
#' }
#' 
#' Helper functions:
#' \itemize{
#'   \item \code{\link{num_leq_sorted}}
#'   \item \code{\link{num_leq_sorted_R}}
#'   \item \code{\link{num_less_sorted}}
#'   \item \code{\link{Ops_uts}}
#'   \item \code{\link{sorted_union}}
#'   \item \code{\link{sorted_union_R}}
#'   \item \code{\link{split_segments}}
#' }
#' 
#' Functions in base \R that are not generic, but really ought to be:
#' \itemize{
#'   \item \code{\link{which}}
#'   \item \code{\link{which.default}}
#'   \item \code{\link{which.max}}
#'   \item \code{\link{which.max.default}}
#'   \item \code{\link{which.min}}
#'   \item \code{\link{which.min.default}}
#' }
#' 
#' Methods that are not applicable to unevenly spaced time series. These are provided so that methods intended for \code{\link{ts}} objects in base \R are not accidentally applied to \code{"uts"} objects:
#' \itemize{
#'   \item \code{\link{as.ts.uts}}
#'   \item \code{\link{cycle.uts}}
#'   \item \code{\link{frequency.uts}}
#' }
#' 
#' @name uts-internal
NULL
