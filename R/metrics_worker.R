#==================================================================================================
#
#		Function to calculate daily metrics from hourly inputs
#
#		start.hour defines which hour data starts on, default is zero
#			values denote how many hours before midnight data starts
#
#			metrics:
#				mda8 - mean daily 8 hr average
#				afternoon - afternoon average ()
#				mean
#				sum
#				midday
#				morning
#				max
#				2p
#==================================================================================================


#define function with inputs of hourly measurements and metric of interest
#output is a daily vector
metrics_worker <- function(x,
                           metric,
                           start.hour = 0){
  try( if( metric %ni% c( 'mda8',
                          'afternoon',
                          'mean',
                          'sum',
                          'midday',
                          'morning',
                          'max',
                          '2p'))
    stop('choose a real metric'))

  #trim data from hours before/after 0
  if ( start.hour != 0){
    length.x <- length( x)
    x <- x[-c( 1:start.hour)]
    x[( length( x) + 1):length.x] <- NA
  }

  #Z defines which hour is associated with mda8h
  length.x <- length( x)
  nday <- length.x / 24
  try( if( nday < 3) stop( "too few days"))

  ## Mean takes the 24-hr average of each day
  if (metric == 'mda8') {## MDA8 of each day
    #z's much be from hour 17 the day before
    Z <- matrix( NA, 31, nday);
    for ( d in 1:nday){
      h.mark <- ( d - 1) * 24
      Z[,d] <- as.double( x[( h.mark + 1):( h.mark + 24 + 7)])
    }
    Z.which <- unlist( apply( Z, 2, max_fun))

    Z.8h <- mean.which8h( Z, Z.which)
    out <- unlist( Z.8h)

  } else {
    Z.mean <- rep( NA, nday)
    for ( d in 1:nday){
      if ( metric == 'afternoon')    {
        h <- ((d - 1) * 24 + 12): (d * 24 - 4)
        na.crit <- 5;
        if ( length( which( is.na( x[h])==T)) >= na.crit) next
        Z.mean[d] <- mean( x[h], na.rm = T)
      } else if (metric =='mean')  {
        h <- ((d - 1) * 24 + 1) : (d * 24)
        na.crit <- 12
        if ( length( which( is.na( x[h]) == T)) >= na.crit) next
        Z.mean[d] <- mean( x[h], na.rm = T)
      } else if (metric =='sum')   {
        h = ((d - 1) * 24 + 1) : (d * 24)
        na.crit <- 1
        if ( length( which( is.na( x[h]) == T)) >= na.crit) next
        Z.mean[d] <- sum( x[h], na.rm = T)
      } else if ( metric == 'midday'){
        h <- ((d - 1) * 24 + 12): (d * 24 - 8)
        na.crit <- 3
        if ( length( which( is.na( x[h])==T)) >= na.crit) next
        Z.mean[d] <- mean( x[h],na.rm=T)
      } else if (metric == 'morning'){
        h <- ((d - 1) * 24 + 8) : (d * 24 - 13)
        na.crit <- 3
        if ( length( which( is.na( x[h]) == T)) >= na.crit) next
        Z.mean[d] <- mean( x[h], na.rm = T)
      } else if (metric == 'max'){
        h <- ((d - 1) * 24 + 1) : (d * 24)
        na.crit <- 24
        if ( length( which( is.na( x[h]) == T)) >= na.crit) next
        Z.mean[d] <- max( x[h], na.rm=T)
      } else if (metric =='2p'){
        h <- ((d - 1) * 24 + 15) : (d * 24 - 8)
        na.crit <- 1
        if ( length( which( is.na( x[h]) == T)) >= na.crit) next
        Z.mean[d] <- mean( x[h], na.rm = T)
      }
    }
    out <- unlist( Z.mean)
  }
  retun( out)
}

