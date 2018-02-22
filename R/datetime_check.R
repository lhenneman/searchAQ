#' Make sure each date has 24 hours
#'
#' \code{datetime_check} takes as input a dataframe and fills missing hours with NA. This function
#' is called by \code{read_search}
#'
#' @param dataf dataframe with date object in POSIXct format
#' @param hourday string of either c('hour', 'day') describing whether input is in hours or days
#' @return This function returns a data.frame with missing hours or days filled with NA


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
