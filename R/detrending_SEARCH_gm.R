#' detrend into longterm, seasonal, short-term meteorology, weekly, and white noise
#'
#' \code{detrending_SEARCH_gm} detrending function shortens time series to complete years
#		to limit skewing on ends of KZ-filtered time series
#'
#' @param n name of column in x.df for detrending
#' @param x.df dataframe of daily pollutant observation metrics and dates
#' @param met dataframe of daily meteorology observation metrics and dates
#' @param rf dataframe of daily rainfall
#' @return This function returns a list of dates, observed values, STM, seasonal, PS, Detrended, and fit R^2

detrending_SEARCH_gm <- function(n,
                                 x.df,
                                 met,
                                 rf){

  #change list lengths so that they match
  if ( dim( x.df)[1] != dim( met)[1]){
    start.met <- met$date[1]
    start.gas <- x.df$date[1]
    end.met <- met$date[dim( met)[1]]
    end.gas <- x.df$date[dim( x.df)[1]]

    if ( start.met != start.gas){
      if ( start.met < start.gas){
        met <- met[met$date >= start.gas,]
      } else {
        x.df <- x.df[x.df$date >= start.met,]
      }
    }
    if ( end.met != end.gas){
      if ( end.met > end.gas){
        met <- met[met$date <= end.gas,]
      } else {
        x.df <- x.df[x.df$date <= end.met,]
      }
    }
  }


  #select correct part of data frame (correct pollutant)
  x = x.df[,n]

  #define terms
  dates <- met$date
  start.date.site <- dates[1];
  end.date.site <- dates[length( dates)]
  start.year <- format( start.date.site, '%Y')
  end.year <- format( end.date.site, '%Y')

  #remove dates at beginning and end of time series outside full years
  newyears.start <- as.Date( USNewYearsDay( as.numeric( as.numeric( start.year))))
  newyears.end <- as.Date( DENewYearsEve( as.numeric( as.numeric( end.year))))
  if (newyears.start != start.date.site){
    nextnewyears.start <- as.POSIXlt( newyears.start)
    nextnewyears.start$year <- nextnewyears.start$year + 1
    nextnewyears.start <- as.Date( nextnewyears.start)
    shortdate.ref <- which( dates %ni% seq.Date(nextnewyears.start, end.date.site, '1 day'))

    #shorten terms
    x.use <- x[-shortdate.ref]
    met.use <- met[-shortdate.ref,]

    #redefine start year
    start.year <- as.numeric( start.year) + 1
    start.date.site <- as.Date( nextnewyears.start)
    dates <- seq.Date( nextnewyears.start, end.date.site, '1 day')

    #redo rainfall rainfall
    rf.ref <- which(rf[,1] %in% dates);
    rf.use <- rf[rf.ref,2]
    rf_fac <- ifelse(rf.use > 0, 1, 0)
  } else {
    x.use <- x
    met.use <- met

    #add on rainfall
    rf.ref <- which( rf[, 1] %in% dates);
    rf.use <- rf[rf.ref, 2]
    rf_fac <- ifelse( rf.use > 0, 1, 0)
  }

  #define more time terms
  years <- start.year:end.year
  dates.nold <- leapyear( dates,
                          year1 = start.year,
                          log = F,
                          fill = F,
                          start.date = start.date.site)

  #run MKZ detrending function for pollutant and meteorology (for all but STM)
  x.mkz <- detrend( x.use,
                    year1 = start.year,
                    log = T,
                    raw = F,
                    fill = F,
                    start.date = start.date.site)
  met.mkz <- apply( met.use[,-which( colnames( met.use) == 'date')],
                    2,
                    detrend,
                    year1 = start.year,
                    log = F,
                    raw = F,
                    fill = F,
                    start.date = start.date.site)
  met.mkz.delt <- sapply( met.mkz,
                          FUN = function(x) x[['delt']])
  rf_fac <- leapyear( rf_fac,
                      year1 = start.year,
                      log = F,
                      fill = F)

  met.mkz.delt <- cbind( met.mkz.delt, rf_fac)

  #remove leapdays from met
  met.ly <- data.frame( apply( met.use[,-which( colnames( met.use) == 'date')],
                               2,
                               leapyear,
                               year1 = start.year,
                               log = F,
                               fill = F,
                               start.date = start.date.site))

  #=====# Define inputs to regressions #=====#
  #holidays
  holidays.dates <- c( as.Date( USMLKingsBirthday( years)),
                       as.Date( USPresidentsDay( years)),
                       as.Date( USMemorialDay( years)),
                       as.Date( USLaborDay( years)),
                       as.Date( USColumbusDay( years)),
                       as.Date( USThanksgivingDay(years)),
                       as.Date( USThanksgivingDay( years)) + 1)
  holidays1.dates<- c( rep( as.Date( USIndependenceDay( years)), 3) + rep( c(-1:1), each = length( years)),
                       rep( as.Date( USNewYearsDay( years)), 3) + rep( c(-1:1), each = length( years)),
                       rep( as.Date( USChristmasDay( years)), 3) + rep(c(-1:1), each = length( years)))
  holidays <-  ifelse( dates.nold %in% holidays.dates  == T, 1, 0)
  holidays1 <- ifelse( dates.nold %in% holidays1.dates == T, 1, 0)

  #time variables
  w <- 2 * pi / 365 #(period for cyclical variables)
  days <- 1:length( holidays)
  daysq <- days ^ 2
  coswt <- cos( w * days)
  sinwt <- sin( w * days)
  tcoswt <- days * cos( w * days)
  tsinwt <- days * sin( w * days)

  #weekdays, months, and season
  weekdays.name <- dayOfWeek( as.timeDate( dates.nold))
  weekdays.numr <- revalue( weekdays.name,
                            c( Sun = 1, Mon = 2, Tue = 3, Wed = 4, Thu = 5, Fri = 6, Sat = 7))
  month <- format( dates.nold,'%b')
  season <- getSeason( dates.nold)

  #interaction variables - weekdays and months with temp
  weekdays_tempM <- model.matrix( ~ weekdays.name - 1) * met.ly$temp_max
  colnames(weekdays_tempM) <- revalue( colnames( weekdays_tempM),c( weekdays.nameMon = 'Mon_tempM',
                                                                    weekdays.nameTue = 'Tue_tempM',
                                                                    weekdays.nameWed = 'Wed_tempM',
                                                                    weekdays.nameThu = 'Thu_tempM',
                                                                    weekdays.nameFri = 'Fri_tempM',
                                                                    weekdays.nameSat = 'Sat_tempM',
                                                                    weekdays.nameSun = 'Sun_tempM'))
  month_tempM <- model.matrix( ~ month - 1) * met.ly$temp_max
  colnames(month_tempM) <- revalue( colnames( month_tempM),c(monthApr = 'Apr_tempM',
                                                             monthAug = 'Aug_tempM',
                                                             monthDec = 'Dec_tempM',
                                                             monthFeb = 'Feb_tempM',
                                                             monthJan = 'Jan_tempM',
                                                             monthJul = 'Jul_tempM',
                                                             monthJun = 'Jun_tempM',
                                                             monthMar = 'Mar_tempM',
                                                             monthMay = 'May_tempM',
                                                             monthNov = 'Nov_tempM',
                                                             monthOct = 'Oct_tempM',
                                                             monthSep = 'Sep_tempM'))

  #lag variables, leapyear them (remove leap days)
  #lag on original data (without leap days removed)
  lag.fn <- function( x, lagk) lag( zoo( x), lagk, na.pad = T)
  met.lag1 <- apply( met.mkz.delt,
                     2,
                     lag.fn,
                     -1)
  met.lag2 <- apply( met.mkz.delt,
                     2,
                     lag.fn,
                     -2)
  colnames( met.lag1) <- paste( colnames( met.mkz.delt), 'p1', sep='_')
  colnames( met.lag2) <- paste( colnames( met.mkz.delt), 'p2', sep='_')

  #square, cube met variables
  met.mkz.delt.cu <- met.mkz.delt ^ 2
  met.mkz.delt.sq <- met.mkz.delt ^ 3
  colnames( met.mkz.delt.cu) <- paste( colnames( met.mkz.delt), 'sq', sep = '_')
  colnames( met.mkz.delt.sq) <- paste( colnames( met.mkz.delt), 'cu', sep = '_')

  #temperature and delta interaction variables
  temp_deltTM <- met.ly$temp_max * met.mkz$temp_max$delt
  temp_deltRH <- met.ly$temp_max * met.mkz$rh_midday$delt

  #=====# create data frame and run regression #=====#
  weekdays.name.mat <- model.matrix( ~ weekdays.name - 1)
  months.name.mat <- model.matrix( ~ month - 1)
  covars <- data.frame(met.mkz.delt, 		met.mkz.delt.cu,		met.mkz.delt.sq,
                       temp_deltTM,		met.lag1, 			met.lag2,			weekdays_tempM,
                       temp_deltRH,		weekdays.name.mat,	months.name.mat,
                       holidays, 			holidays1,			days)
  covars.rmnm <- na.omit( covars)
  covars.keep <- c( 'temp_midday', 'temp_midday_sq', 'temp_midday_cu', 'temp_deltTM', 'temp_deltRH',
                    'ws_midday', 'rh_midday', 'rf_fac', 'temp_midday_p1', 'ws_midday_p1', 'rh_midday_p1', 'rf_fac_p1',
                    'temp_midday_p2', 'ws_midday_p2', 'rh_midday_p2', 'rf_fac_p2', colnames( weekdays_tempM),
                    colnames( weekdays.name.mat), colnames( months.name.mat), 'holidays', 'holidays1')
  covars.min  <- c( colnames( weekdays_tempM), colnames( weekdays.name.mat), colnames( months.name.mat))
  #RF, RFp1, RFp2
  #run regression, use AIC to select variables (keep minimum model)
  STM_x <- lm( x.mkz$delt[covars.rmnm$days] ~. , covars.rmnm[,covars.keep], na.action = 'na.exclude')
  STM_min <- formula( x.mkz$delt[covars.rmnm$days] ~., covars.rmnm[,covars.min])
  STM_xs <- step( STM_x, direct = c('both'), k = 3.84, trace = 0)

  #estimate weekday-holiday (WH) fit to remove it
  covars.stm <- c( "(Intercept)",'temp_midday','temp_midday_sq','temp_midday_cu','temp_deltTM','temp_deltRH',
                   'ws_midday','rh_midday','rf_fac','temp_midday_p1','ws_midday_p1','rh_midday_p1','temp_midday_p2','ws_midday_p2',
                   'rh_midday_p2','rf_fac_p1','rf_fac_p2')
  covars.wh <- names( coef(STM_xs))[names( coef( STM_xs)) %ni% covars.stm]
  WH_xvec <- rep( NA, length( holidays))
  WH_xvec[covars.rmnm$days] <- c( subfitter( coef( STM_xs)[names( coef( STM_xs)) %in% covars.wh], covars.rmnm[,covars.wh]))

  #define STM (model fit with WH subtracted)
  STM_xvec <- rep( NA, length( holidays))
  STM_xvec[covars.rmnm$days] <- predict( STM_xs) - WH_xvec[covars.rmnm$days]

  #=====# create detrended timeseries #=====#
  x.raw <- detrend( x.use,
                    year1 = start.year,
                    log = F,
                    raw = T,
                    fill = F,
                    start.date = start.date.site)
  x.S <- detrend( x.use,
                  year1 = start.year,
                  log = F,
                  raw = F,
                  fill = F,
                  start.date = start.date.site)$Scomplete
  x.ln <- detrend( x.use,
                   year1 = start.year,
                   log = T,
                   raw = T,
                   fill = F,
                   start.date = start.date.site)
  x.det <- exp( x.ln - STM_xvec + mean( STM_xvec, na.rm=T))
  x.fit <- exp( x.mkz$SLT + STM_xvec + WH_xvec)

  #calculate statistics
  corr.x.fit <- cor( x.raw, x.fit, use = 'complete.obs') ^ 2

  #calcuate PS*
  ps <- x.S + x.raw - x.det

  #=====# create object to save #=====#
  out = list(data = data.frame( date = dates.nold,
                                obs = x.raw,
                                STM = STM_xvec,
                                S = x.mkz$Scomplete,
                                PS = ps,
                                Det = x.det),
             r.sq = corr.x.fit)

  return( out)

}
