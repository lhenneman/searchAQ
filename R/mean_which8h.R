#' get the mda8
#'
#' \code{mean_which8h} takes a vector of eight hour averages and a which one is the maximym and resturns the mean. This
#' function is called by metrics_worker
#'
#' @param Z vector of eight hour averages
#' @param Z.which which Z is the maximum
#' @return This function returns the eight hour average daily max


mean_which8h <- function( Z,
                          Z.which){
  Z.8h <- rep( NA, length( Z.which))
  for ( d in 2:length( Z.which)){
    if ( is.na( Z.which[d]) == T) next
    h.which <- ( Z.which[d]):( Z.which[d] + 7)
    Z.8h[d] <- mean( Z[h.which, d])
  }
  return( Z.8h)
}

