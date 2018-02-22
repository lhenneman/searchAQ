#' adjust year in imported SEARCH data
#'
#' \code{foo} takes as input a date and changes the year to the correct one. Not to be called individually,
#' called by \code{read_SEARCH}
#'
#' @param x vector of strptime dates
#' @param year
#' @return Dates in consistent format between two types of SEARCH inputs



foo <- function( x,
                 year = 1968){
  m <- year( x) %% 100
  year( x) <- ifelse( m > year %% 100,
                      1900 + m,
                      2000 + m)
  return( x)
}
