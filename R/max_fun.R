#' identify maximum 8hr ozone average
#'
#' \code{max_fun} takes a vector of eight hour averages and returns which one is a max.
#' The function is called by \code{metrics_worker}
#'
#' @param z vector of eight hour averages
#' @return This function returns an integer for which is the maximum


max_fun <- function(z){
  h.vec <- rep( NA, 24)
  for ( h in 1:24 ){
    h.vec[h] <- mean( z[h:( h + 7)])
  }
  h.max <- which.max( h.vec)
  if ( length( h.max) == 0)
    h.max <- NA
  return( h.max)
}
