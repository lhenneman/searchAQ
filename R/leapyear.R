################################################################################################
#
#
#				Function to remove the leap days from a signal
#
#				year1 = starting year
#				fill = TRUE/FALSE to fill NA values with mean of that day of year
#				met = TRUE/FALSE meteorological or not
#				log = TRUE/FALSE use the log of the signal or not
#
#				Lucas Henneman
#				December 2012
#
#
#
#################################################################################################

leapyear <- function(signal,
                     year1,
                     log,
                     fill = TRUE,
                     start.date = c()){


  C <- signal
  N <- floor( length( C)/365) #number of years of data
  Y <- year1 #starting year

  if ( length( C) >= ( 365 * N + ceiling( N/4)))
    N = N + 1

  #if start.date exists, use it for the leapyear signal
  if ( length( start.date) == 1){
    start.date.use = as.Date( start.date)
  } else if(length( start.date) == 0){
    start.date.use = as.Date( paste( year1, '01-01', sep = '-'))
  } else stop("start.date is not appropriate format (YYYY-MM-DD)")

  date.vec <- seq.Date( start.date.use,
                        by = '1 day',
                        length.out = length( signal))
  which.leapdays <- grep( '-02-29', date.vec)

  C <- C[-which.leapdays]

  #replace NA's with predicted mean values (using avg of that day of year)
  if( fill == TRUE){
    Cfix <- C
    for ( n in 0:(N-1)){
      for ( i in 1:365){
        C[( n * (365)+i)] <- ifelse( is.na( Cfix[(n * (365) + i)]) == TRUE,
                                     mean( Cfix[(0:(N-1)) * 365 + i],
                                           na.rm = TRUE),
                                     Cfix[(n * (365) + i)])
      }
    }
  }

  if (log == TRUE){C = log(C)}

  return( C)
}
