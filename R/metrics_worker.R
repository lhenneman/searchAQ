#' calculate daily metrics from hourly inputs
#'
#' \code{dailymetrics_SEARCH_gm} takes as input a list of hourly gas, meteorology (met), pm2.5 (pm)
#' and ion species concentrations (e.g., from the \code{read_SEARCH} function) and outputs a list of
#' daily gas, met, pm, and ion species. Multiple daily metrics are taken for each species
#'
#' @param metric string, one of:
#' #'\enumerate{
#'   \item mda8 - mean daily 8 hr average
#'   \item afternoon - afternoon average
#'   \item sum
#'   \item midday
#'   \item morning
#'   \item max
#'   \item 2p
#' }
#' @param start.hour defines which hour data starts on, default is zero
#			values denote how many hours before midnight data starts
#' @return This function returns a vector of daily metrics

metrics_worker <- function(x,
                           metric,
                           datehour,
                           species = NULL){
  try( if( metric %ni% c( 'mda8',
                          'afternoon',
                          'mean',
                          'sum',
                          'midday',
                          'morning',
                          'max',
                          '2p'))
    stop('choose a real metric'))

  start.hour <- hour( min( datehour))

  #trim data from hours before/after 0
  if ( start.hour != 0){
    length.x <- length( x)
    x <- x[-c( 1:start.hour)]
    datehour <- datehour[-c( 1:start.hour)]
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

    Z.8h <- mean_which8h( Z, Z.which)
    out1 <- unlist( Z.8h)

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
    out1 <- unlist( Z.mean)
  }

  date.unique <- unique(as.Date( datehour, tz = ''))
  name <- paste( species, metric, sep = '_')
  out <- data.frame( date = date.unique,
                     metric = out1)
  names(out) <- c('date', name)
  return( out)
}

