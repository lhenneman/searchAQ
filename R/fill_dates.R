#fill missing dates with NA
fill_dates <- function( df,
                        station,
                        datevec){
  #subset data frame
  STA.df <- subset( df,
                    STATION_NAME == station)

  #three references - dates included and excluded in desired and new time series
  STA.dateref <- which( STA.df$DATE %in% datevec == T)
  NA.dateref <- which( datevec %ni% STA.df$DATE == T)
  fill.dateref <- which( datevec %in% STA.df$DATE == T)

  out <- c()
  out[NA.dateref] <- NA
  out[fill.dateref] <- STA.df$PRCP[STA.dateref]
  return( out)
}
