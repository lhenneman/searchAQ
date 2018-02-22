#' read SEARCH data in native formats
#'
#' \code{dailymetrics_SEARCH_gm} takes as input a list of hourly gas, meteorology (met), pm2.5 (pm)
#' and ion species concentrations (e.g., from the \code{read_SEARCH} function) and outputs a list of
#' daily gas, met, pm, and ion species. Multiple daily metrics are taken for each species
#'
#' @param input list of hourly gas, meteorology (met), pm2.5 (pm)
#' and ion species concentrations (e.g., from the \code{read_SEARCH} function)
#' @return This function returns a list of daily gas, met, pm, and ion species

dailymetrics_SEARCH_gm <- function(input){
  gas.datehour <- input[['gas']]$date
  ion.datehour <- input[['ion']]$date
  met.datehour <- input[['met']]$date

  #create data frame of daily metrics for each species/met variable
  Gas.df <- join_all( list( metrics_worker( input[['gas']]$o3, 'mda8', datehour = gas.datehour, species = 'o3'),
                            metrics_worker( input[['gas']]$o3, 'mean', datehour = gas.datehour, species = 'o3'),
                            metrics_worker( input[['gas']]$o3, 'max', datehour = gas.datehour, species = 'o3'),
                            metrics_worker( input[['gas']]$o3, '2p', datehour = gas.datehour, species = 'o3'),
                            metrics_worker( input[['gas']]$co, 'afternoon', datehour = gas.datehour, species = 'co'),
                            metrics_worker( input[['gas']]$co, 'max', datehour = gas.datehour, species = 'co'),
                            metrics_worker( input[['gas']]$co, '2p', datehour = gas.datehour, species = 'co'),
                            metrics_worker( input[['gas']]$so2,'max', datehour = gas.datehour, species = 'so2'),
                            metrics_worker( input[['gas']]$so2,'mean', datehour = gas.datehour, species = 'so2'),
                            metrics_worker( input[['gas']]$so2,'2p', datehour = gas.datehour, species = 'so2'),
                            metrics_worker( input[['gas']]$nox,'afternoon', datehour = gas.datehour, species = 'nox'),
                            metrics_worker( input[['gas']]$nox,'max', datehour = gas.datehour, species = 'nox'),
                            metrics_worker( input[['gas']]$nox,'2p', datehour = gas.datehour, species = 'nox'),
                            metrics_worker( input[['gas']]$no, 'max', datehour = gas.datehour, species = 'no'),
                            metrics_worker( input[['gas']]$no, '2p', datehour = gas.datehour, species = 'no'),
                            metrics_worker( input[['gas']]$no, 'afternoon', datehour = gas.datehour, species = 'no'),
                            metrics_worker( input[['gas']]$no2,'max', datehour = gas.datehour, species = 'no2'),
                            metrics_worker( input[['gas']]$no2,'2p', datehour = gas.datehour, species = 'no2'),
                            metrics_worker( input[['gas']]$no2,'afternoon', datehour = gas.datehour, species = 'no2'),
                            metrics_worker( input[['gas']]$noy,'max', datehour = gas.datehour, species = 'noy'),
                            metrics_worker( input[['gas']]$noy,'2p', datehour = gas.datehour, species = 'noy'),
                            metrics_worker( input[['gas']]$noy,'afternoon', datehour = gas.datehour, species = 'noy'),
                            metrics_worker( input[['gas']]$hno3,'afternoon', datehour = gas.datehour, species = 'hno3'),
                            metrics_worker( input[['gas']]$hno3,'2p', datehour = gas.datehour, species = 'hno3'),
                            metrics_worker( input[['gas']]$hno3,'max', datehour = gas.datehour, species = 'hno3')),
                      by = 'date')

  PM.df  <-  data.frame( date	= unique(as.Date( input[['pm']]$date, tz = '')),
                         pm25  = input[['pm']]$pm25)

  Ion.df <- join_all( list( metrics_worker( input[['ion']]$so4, 'mean', datehour = ion.datehour, species = 'so4'),
                            metrics_worker( input[['ion']]$no3, 'mean', datehour = ion.datehour, species = 'no3'),
                            metrics_worker( input[['ion']]$nh4, 'mean', datehour = ion.datehour, species = 'nh4'),
                            metrics_worker( input[['ion']]$so4, '2p', datehour = ion.datehour, species = 'so4'),
                            metrics_worker( input[['ion']]$no3, '2p', datehour = ion.datehour, species = 'no3'),
                            metrics_worker( input[['ion']]$nh4, '2p', datehour = ion.datehour, species = 'nh4')),
                      by = 'date')

  Met.df <- join_all( list( rh_md = metrics_worker( input[['met']]$rh, 'midday', datehour = met.datehour, species = 'rh'),
                            sr_s  = metrics_worker( input[['met']]$sr, 'sum', datehour = met.datehour, species = 'sr'),
                            sr_M  = metrics_worker( input[['met']]$sr, 'max', datehour = met.datehour, species = 'sr'),
                            ws_md = metrics_worker( input[['met']]$ws, 'midday', datehour = met.datehour, species = 'ws'),
                            ws_morn=metrics_worker( input[['met']]$ws, 'morning', datehour = met.datehour, species = 'ws'),
                            temp_md=metrics_worker( input[['met']]$temp, 'midday', datehour = met.datehour, species = 'temp'),
                            temp_M= metrics_worker( input[['met']]$temp, 'max', datehour = met.datehour, species = 'temp')),
                      by = 'date')


  out = list( gas = Gas.df,
              met = Met.df,
              pm = PM.df,
              ion = Ion.df)
}
