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

  #create data frame of daily metrics for each species/met variable
  Gas.df <- data.frame( date   = unique( as.Date( input[['gas']]$date, tz = '')),
                        o3_md8a= metrics_worker( input[['gas']]$o3, 'mda8'),
                        o3_m   = metrics_worker( input[['gas']]$o3, 'mean'),
                        o3_M   = metrics_worker( input[['gas']]$o3, 'max'),
                        o3_2p  = metrics_worker( input[['gas']]$o3, '2p'),
                        co_aft = metrics_worker( input[['gas']]$co, 'afternoon'),
                        co_M   = metrics_worker( input[['gas']]$co, 'max'),
                        co_2p  = metrics_worker( input[['gas']]$co, '2p'),
                        so2_M  = metrics_worker( input[['gas']]$so2,'max'),
                        so2_m  = metrics_worker( input[['gas']]$so2,'mean'),
                        so2_2p = metrics_worker( input[['gas']]$so2,'2p'),
                        nox_aft= metrics_worker( input[['gas']]$nox,'afternoon'),
                        nox_M  = metrics_worker( input[['gas']]$nox,'max'),
                        nox_2p = metrics_worker( input[['gas']]$nox,'2p'),
                        no_M   = metrics_worker( input[['gas']]$no, 'max'),
                        no_2p  = metrics_worker( input[['gas']]$no, '2p'),
                        no_aft = metrics_worker( input[['gas']]$no, 'afternoon'),
                        no2_M  = metrics_worker( input[['gas']]$no2,'max'),
                        no2_2p = metrics_worker( input[['gas']]$no2,'2p'),
                        no2_aft= metrics_worker( input[['gas']]$no2,'afternoon'),
                        noy_M  = metrics_worker( input[['gas']]$noy,'max'),
                        noy_2p = metrics_worker( input[['gas']]$noy,'2p'),
                        noy_aft= metrics_worker( input[['gas']]$noy,'afternoon'),
                        hno3_aft=metrics_worker( input[['gas']]$hno3,'afternoon'),
                        hno3_2p= metrics_worker( input[['gas']]$hno3,'2p'),
                        hno3_M  =metrics_worker( input[['gas']]$hno3,'max'))

  PM.df  <-  data.frame( date	= unique(as.Date( input[['pm']]$date, tz = '')),
                         pm25  = input[['pm']]$pm25)

  Ion.df <- data.frame( date	= unique( as.Date( input[['ion']]$date, tz = '')),
                        so4   = metrics_worker( input[['ion']]$so4, 'mean'),
                        no3   = metrics_worker( input[['ion']]$no3, 'mean'),
                        nh4   = metrics_worker( input[['ion']]$nh4, 'mean'),
                        so4_2p= metrics_worker( input[['ion']]$so4, '2p'),
                        no3_2p= metrics_worker( input[['ion']]$no3, '2p'),
                        nh4_2p= metrics_worker( input[['ion']]$nh4, '2p'))

  Met.df <- data.frame( date = unique( as.Date( input[['met']]$date, tz = '')),
                        rh_md = metrics_worker( input[['met']]$rh, 'midday'),
                        sr_s  = metrics_worker( input[['met']]$sr, 'sum'),
                        sr_M  = metrics_worker( input[['met']]$sr, 'max'),
                        ws_md = metrics_worker( input[['met']]$ws, 'midday'),
                        ws_morn=metrics_worker( input[['met']]$ws, 'morning'),
                        temp_md=metrics_worker( input[['met']]$temp, 'midday'),
                        temp_M= metrics_worker( input[['met']]$temp, 'max'))


  out = list( gas = Gas.df,
              met = Met.df,
              pm = PM.df,
              ion = Ion.df)
}
