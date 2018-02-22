#==================================================#
#read input rainfall data
#==================================================#

input_NCDC_CDO_rf <- function( file){
  #import data file
  rf.data <- fread( file,
                    na.strings = c('-9999','unknown'),
                    data.table = F)

  #determine dates
  rf.data$DATE <- as.Date( as.character( rf.data$DATE), format = '%Y%m%d')
  rf.date <- sort( unique( rf.data$DATE))

  #list stations, rename to shorter names
  station.names <- c( 'GULFPORT BILOXI AIRPORT MS US' = 'GFP',
                     'PENSACOLA REGIONAL AIRPORT FL US' = 'PNS',
                     'BIRMINGHAM AIRPORT AL US' = 'BHM',
                     'ATLANTA HARTSFIELD INTERNATIONAL AIRPORT GA US' = 'ATL')
  rf.data$STATION_NAME <- revalue( rf.data$STATION_NAME, station.names)
  rf.stations <- unique( rf.data$STATION_NAME)

  #fill missing dates with NA
  rf.df <- data.frame(	date = rf.date,
                       GFP  = fill_dates(rf.data, 'GFP', rf.date),
                       PNS  = fill_dates(rf.data, 'PNS', rf.date),
                       BHM  = fill_dates(rf.data, 'BHM', rf.date),
                       ATL  = fill_dates(rf.data, 'ATL', rf.date))

  #combine into list, export
  stations.names.short <- c('GFP','PNS','BHM','ATL')
  listify <- function(n,df) list( date = df$date,
                                  Precip = df[,n])
  out <- lapply( stations.names.short,
                 listify ,
                 rf.df)
  names(out) <- stations.names.short
  return( out)
}
