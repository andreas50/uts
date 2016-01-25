#' Internal Functions
#'
#' The internal functions listed below might be of interest to developers seeking to extend the package functionality.
#' 
#' \code{uts} methods:
#' \itemize{
#'   \item \code{\link[=mean.uts]{mean}}
#'   \item \code{\link[=median.uts]{median}}
#'   \item \code{\link[=sd.uts]{sd}}
#'   \item \code{\link[=str.uts]{str}}
#'   \item \code{\link[=summary.uts]{summary}}
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
#' Methods that exists solely ensure that methods intended for \code{\link{ts}} objects in base \R are not accidentally applied to \code{"uts"} objects:
#' \itemize{
#'   \item \code{\link[=cycle.uts]{cycle}}
#'   \item \code{\link[=frequency.uts]{frequency}}
#' }
#' 
#' @name uts-internal
NULL
