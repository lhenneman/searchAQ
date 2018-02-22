#' detrend SEARCH data
#'
#' \code{detrend} Script to detrend met data (input is daily values e.g.from MetVeraging.R)
#				with KZ filter, then average output over 365 days. Uses the KZ(365,3) and KZ(15,5) filters
#'
#' @param signal vector of daily obsevations
#' @param year1 starting year
#' @param log TRUE/FALSE use the log of the signal or not
#' @param raw TRUE/FALSE returns raw signal, minus leap days (and filling missing days)
#					-both fns return same value for this
#' @param fill TRUE/FALSE fill missing values?
#' @param is.leap.year use FALSE if \code{signal} contains leap days
#' @param start.date in date format, first day of the signal. Leave as default for start dates on 1 January
#' @return This function returns a list of daily detrended components

detrend <- function(signal,
                        year1,
                        log,
                        raw = FALSE,
                        fill = TRUE,
                        is.leap.year = TRUE,
                        start.date = c()){

  if (is.leap.year == TRUE){
    C <- leapyear( signal = signal,
                   year1 = year1,
                   log = log,
                   fill = fill,
                   start.date = start.date)
  } else {C <- signal}

  if (raw == TRUE){return(C)} else { ##raw gives back the log of the original time series, minus the leap days and with averaged days

    Clt <- kz( C, 365, k = 3)
    Cprime <- C - Clt
    CS <- kz( Cprime, 15, k = 5)

    N <- floor( length( C)/365) #number of years of data
    CSme = c()


    #create S (seasonal) trend using second KZ filter.
    for (d in 1:365){
      ref <- (0:(N - 1)) * 365 + d
      CSme[d] <- mean( CS[ref], na.rm = TRUE)
    }
    #account for signals that do not start on january 1st
    CSme.shift <- c()
    if ( length( start.date) == 0)
      start.date <- as.Date( USNewYearsDay( as.numeric( year1)))
    if ( start.date != as.Date( USNewYearsDay( as.numeric( year1)))){
      shift <- as.numeric( as.Date( paste( year1, '12-31', sep = '-')) - start.date)
      CSme.shift[1:(365 - shift)] <- CSme[(shift + 1):length( CSme)]
      CSme.shift[(365 - shift + 1):365] <- CSme[1:shift]
      CSme.complete = append(CSme[1:( shift + 1)],
                             rep( CSme.shift, times = N))
    } else {
      CSme.shift <- CSme
      CSme.complete <- rep(CSme.shift,times=N)
    }

    CSmen <- CSme.complete + Clt
    Cdelt <- C - CSmen
    out <- list(delt = Cdelt,
                S = CSme.shift,
                SLT = CSmen,
                LT = Clt,
                Scomplete = CSme.complete)

    return(out)
  }
}
