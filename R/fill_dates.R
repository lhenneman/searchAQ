#' fill missing dates with NA
#'
#' \code{fill_dates} takes a dataframe and station name, then returns a dataframe
#' with missing days filled with NA. Called in the \code{input_NCDC_CDO_rf} function
#'
#' @param df data.frame with at least the columns STATION_NAME and DATE
#' @param station station abbreviation. Likely one of c('GFP', 'PNS', 'BHM', 'ATL')
#' @return This function returns a filled dataset

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
