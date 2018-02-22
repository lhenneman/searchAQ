foo <- function( x,
                 year = 1968){
  m <- year( x) %% 100
  year( x) <- ifelse( m > year %% 100,
                      1900 + m,
                      2000 + m)
  return( x)
}
