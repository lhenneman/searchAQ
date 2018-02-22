#choose which maximum for md8a ozone
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
