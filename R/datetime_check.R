#Make sure each date has 24 hours
datetime_check <- function( dataf,
                            hourday = 'hour'){
  if ( hourday == 'hour') form = '%F %T' else form = '%F'

  dataf <- dataf[order( dataf$date),]
  if ( length( which( is.na( dataf$date) == T)) > 0 )
    dataf <- dataf[-which( is.na( dataf$date) == T),]


  datetime.all <- seq( dataf$date[1], dataf$date[length( dataf$date)],
                       by = hourday)
  datetime.ni  <- datetime.all[as.numeric( datetime.all) %ni% as.numeric( dataf$date)]
  empty <- 	data.frame( matrix( NA,
                                ncol = dim( dataf)[2],
                                nrow = length( datetime.ni)))
  colnames( empty) <- colnames( dataf)
  empty$date <- datetime.ni

  if ( length( datetime.ni) > 0){
    out <- rbind( dataf, empty)
  } else out <- dataf

  out <- out[order( out$date),]
  return( out)
}
