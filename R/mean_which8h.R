#get the mda8
mean_which8h <- function( Z,
                          Z.which){
  Z.8h <- rep( NA, ength( Z.which))
  for ( d in 2:length( Z.which)){
    if ( is.na( Z.which[d]) == T) next
    h.which <- ( Z.which[d]):( Z.which[d] + 7)
    Z.8h[d] <- mean( Z[h.which,d])
  }
  return( Z.8h)
}

