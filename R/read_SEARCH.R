#' read SEARCH data in native formats
#'
#' \code{read_SEARCH} takes as input a site abbreviation and a directory location and
#' returns a list of hourly gas, meteorology (met), pm2.5 (pm) and ion species concentrations
#'
#' @param site one of c('GFP', 'PNS', 'BHM', 'JST', 'YRK', 'CTR', 'OLF', 'OAK')
#' @param loc.search File directory that contains SEARCH datasets
#' @return This function returns a list of hourly gas, meteorology (met), and ion species concentrations, and
#' daily PM2.5 concentrations (pm)

read_SEARCH <- function( site,
                         loc.search){

  #create list of files for import, go through years directories
  direc.list.complete <- list.dirs( loc.search,
                                    recursive = F)
  direc.list <- suppressWarnings(
    direc.list.complete[which( is.na( as.numeric(
      substr( list.dirs( loc.search,
                         recursive = F),
              nchar( list.dirs( loc.search,
                                recursive = F)) - 3,
              nchar( list.dirs( loc.search,
                                recursive = F))))) == F)])

  #list of years taken from directory names
  years <- suppressWarnings(
    as.numeric( substr( direc.list,
                        nchar( list.dirs( loc.search,
                                          recursive = F)) - 3,
                        nchar( list.dirs( loc.search,
                                          recursive = F))))[
      which( is.na( as.numeric( substr( direc.list,
                                        nchar( list.dirs( loc.search,
                                                          recursive = F)) - 3,
                                        nchar( list.dirs( loc.search,
                                                          recursive = F))))) == F)])

  #list of files - early gas/met files are combined, later ones are split
  files.list.gm <- unique( list.files( paste( direc.list, '/GasMet', sep = ''),
                                       full.names = T,
                                       pattern = site))
  files.list.ge <- unique( list.files( paste( direc.list, '/Trace Gas', sep = ''),
                                       full.names = T,
                                       pattern = paste( site, ".*", 'OneHour', '.*', sep = '')))
  files.list.g <- unique( list.files( paste( direc.list, '/Trace Gas', sep = ''),
                                      full.names = T,
                                      pattern = paste( site, "\\-Trace Gas.*", 'OneHour', '.*', sep = '')))
  files.list.ga <- files.list.ge[files.list.ge %ni% files.list.g]

  files.list.m <- unique( list.files( paste( direc.list, '/Met', sep = ''),
                                      full.names = T,
                                      pattern = paste( site, ".*", 'OneHour', '.*', sep = '')))
  files.list.pm <- unique( list.files( paste( direc.list, '/PMfine', sep = ''),
                                       full.names = T,
                                       pattern = paste( site, sep='')))
  files.list.ion <- unique( list.files( paste( direc.list, '/ContinuousPM', sep = ''),
                                        full.names = T,
                                        pattern = paste( paste( site, 'Cont', sep=''),
                                                         paste( site, 'PM25 Ions', sep = '-'),
                                                         paste( site, 'Continuous PM-', sep = '-'),
                                                         sep = '|')))

  #this line imports the early data (which is saved at xls files), imports each year
  #as an item in a list using lapply and read.xls, then uses ldply to extract the list
  #items and convert them to a dataframe.
  #ga is extra files, which aren't in the original files
  imports.gm <- suppressWarnings( ldply( lapply( files.list.gm,
                                                 read.xls,
                                                 pattern = 'DATE',
                                                 na.strings = c( '-999', '')),
                                         data.frame))
  imports.g <- suppressWarnings( ldply( lapply( files.list.g,
                                                fread,
                                                skip = 'Date',
                                                na.strings = c( '-999', 'Null'),
                                                data.table = F,
                                                fill = T,
                                                header = T)))
  imports.ga <- suppressWarnings( ldply( lapply( files.list.ga,
                                                 fread,
                                                 skip = 'Date',
                                                 na.strings = c( '-999', 'Null'),
                                                 data.table = F,
                                                 fill = T,
                                                 header = T)))
  imports.m <- suppressWarnings( ldply( lapply( files.list.m,
                                                fread,
                                                skip = 'Date',
                                                na.strings = c( '-999','Null'),
                                                data.table = F,
                                                fill = T,
                                                header = T)))

  #read PM2.5 files - change of format in 2008 (for JST)
  imports.pm <- suppressWarnings( ldply( lapply( files.list.pm,
                                                 read.xls,
                                                 pattern = 'Sample',
                                                 na.strings = c( '-999', '-999.00', 'Null'),
                                                 header = T)))
  ref.normdate <- grep( '00:00:00', imports.pm$SampleDate)
  ref.latedate <- which ( is.na( imports.pm$Sample.Date) == F)
  dates.pm <- rep( NA, dim( imports.pm)[1])
  dates.pm[ref.normdate]  <- as.character( strptime( imports.pm$SampleDate[ref.normdate],
                                                     format = '%F %H'))
  dates.pm[-ref.normdate] <- as.character( strptime( imports.pm$SampleDate[-ref.normdate],
                                                     format = '%F'))
  dates.pm[ref.latedate]  <- as.character( strptime( imports.pm$Sample.Date[ref.latedate],
                                                     format = '%m/%d/%y'))

  #read PM2.5 ion files - change of format in 2005
  if ( length( grep( paste( 1990:2004, collapse='|'), files.list.ion)) != 0){
    imports.ion1 <- suppressWarnings( ldply( lapply( grep( paste( 1990:2004, collapse = '|'),
                                                           files.list.ion,
                                                           value = T),
                                                     read.xls,
                                                     pattern = 'DATE',
                                                     header = T,
                                                     na.strings = c( '-999', '-999.00', 'Null', '-888.00',
                                                                     '-999.0', '-9.99E+02', '-8.88E+02'))))
  }
  imports.ion2 <- suppressWarnings( ldply( lapply( grep( paste(2005:2020, collapse = '|'), files.list.ion, value = T),
                                                   fread,
                                                   skip = 'Date/Time[LST]',
                                                   na.strings = c( '-999', 'Null', '#VALUE!'),
                                                   data.table = F,
                                                   fill = T)))

  #check imports.m date formate and make sure it's consistent
  which.format <- which( strptime( imports.m[, 'Date/Time[LST]'], format = '%m/%d/%Y %H:%M', tz = 'EST') <=
                           strptime( '01/01/1970', format = '%m/%d/%Y', tz = 'EST'))
  imports.m[which.format,'Date/Time[LST]'] <- format(strptime(imports.m[which.format,'Date/Time[LST]'],
                                                              format='%m/%d/%y %H:%M'),format='%m/%d/%Y %H:%M',tz='EST')

  #add the missed data from extra files as available - gases
  colnames.ga <- colnames( imports.ga)[which( colnames( imports.ga) %in% colnames( imports.g))][-c( 1:2)]
  ref.ga <- which( is.na( imports.ga[, colnames.ga]) == F, arr.ind = T)
  if( dim( imports.g)[1] != 0){
    if( dim( imports.ga)[1] != 0){
      rows.ga <- which( imports.g[,1] %in% imports.ga[,1] == T)
      if(length(rows.ga)==0) {
        empty.g <- matrix( NA, ncol = dim( imports.g)[2], nrow = dim( imports.ga)[1])
        colnames( empty.g) <- colnames( imports.g)
        imports.g <- rbind( imports.g, empty.g)
        imports.g[is.na( imports.g[, "Date/Time[LST]"]), "Date/Time[LST]"] <- imports.ga[,"Date/Time[LST]"]
        rows.ga <- which( imports.g[,1] %in% imports.ga[,1] == T)
      }
      imports.g[rows.ga, colnames.ga][ref.ga] <- suppressWarnings( as.double( imports.ga[,colnames.ga][ref.ga]))
    }
    #combine variables into single data frames for gas and met
    date1 <- strptime( imports.gm$DATE.TIME, format = '%m/%d/%y %H:%M', tz = 'UTC')
    date2 <- strptime( imports.g[,'Date/Time[LST]'], format = '%m/%d/%Y %H:%M', tz = 'UTC')
    Gas.df <- data.frame( date = c( date1, date2),
                          o3   = c( colCall( imports.gm, 'O3'),  colCall( imports.g, 'O3')),
                          co   = c( colCall( imports.gm, 'CO'),  colCall( imports.g, 'CO')),
                          so2  = c( colCall( imports.gm, 'SO2'), colCall( imports.g, 'SO2')),
                          no   = c( colCall( imports.gm, 'NO'),  colCall( imports.g, 'NO')),
                          no2  = c( colCall( imports.gm, 'NO2'), colCall( imports.g, 'NO2')),
                          nox  = c( colCall( imports.gm, 'NO'),  colCall( imports.g, 'NO')) +
                            c( colCall( imports.gm, 'NO2'), colCall( imports.g, 'NO2')),
                          hno3 = c( colCall( imports.gm, 'HNO3'),colCall( imports.g, 'HNO3')),
                          noy  = c( colCall( imports.gm, 'NOy'), colCall( imports.g, 'NOy')))
  } else {
    #combine variables into single data frames for gas and met
    Gas.df <- data.frame(date = strptime( imports.gm$DATE.TIME,
                                          format = '%m/%d/%y %H:%M',
                                          tz = 'EST5EDT'),
                         o3   = colCall( imports.gm, 'O3'),
                         co   = colCall( imports.gm, 'CO'),
                         so2  = colCall( imports.gm, 'SO2'),
                         no   = colCall( imports.gm, 'NO'),
                         no2  = colCall( imports.gm, 'NO2'),
                         nox  = colCall( imports.gm, 'NO') + colCall( imports.gm, 'NO2'),
                         hno3 = colCall( imports.gm, 'HNO3'),
                         noy  = colCall( imports.gm, 'NOy'))
  }

  #meteorology - combine the two files types (or just single file if only one type)
  if ( dim( imports.m)[1] != 0) {
    date1 <- strptime( imports.gm$DATE.TIME, format = '%m/%d/%y %H:%M', tz = 'UTC')
    date2 <- strptime(imports.m[,'Date/Time[LST]'], format = '%m/%d/%Y %H:%M', tz = 'UTC')
    Met.df <- data.frame(date = c( date1,date2),
                         wsp  = c( colCall( imports.gm, 'WSP'), colCall( imports.m, 'WSP')),
                         wdr  = c( colCall( imports.gm, 'WDR'), colCall( imports.m, 'WDR')),
                         temp = c( colCall( imports.gm, 'TEMP'),colCall( imports.m, 'TEMP')),
                         rh   = c( colCall( imports.gm, 'RH'),  colCall( imports.m, 'RH')),
                         sr   = c( colCall( imports.gm, 'SR'),  colCall( imports.m, 'SR')),
                         precip=c( colCall( imports.gm, 'PRECIP'),colCall(imports.m, 'PRECIP')),
                         bp	  = c( colCall( imports.gm, 'BP'),  colCall( imports.m, 'BP')))
  } else {
    Met.df <- data.frame(date = strptime( imports.gm$DATE.TIME,
                                          format = '%m/%d/%y %H:%M',
                                          tz = 'EST5EDT'),
                         wsp  = colCall( imports.gm, 'WSP'),
                         wdr  = colCall( imports.gm, 'WDR'),
                         temp = colCall( imports.gm, 'TEMP'),
                         rh   = colCall( imports.gm, 'RH'),
                         sr   = colCall( imports.gm, 'SR'),
                         precip=colCall( imports.gm, 'PRECIP'),
                         bp	  = colCall( imports.gm, 'BP'))

  }
  #ions - combine data
  if ( length( grep( paste( 1990:2004, collapse = '|'), files.list.ion)) == 0){
    date2 <- foo( strptime( imports.ion2[, 'Date/Time[LST]'],
                            format = '%m/%d/%Y %H:%M',
                            tz = 'UTC'),
                  year = 2030)
    Ion.df <- data.frame(date = c( date2),
                         so4  = c( colCall( imports.ion2, 'SO4')),
                         no3  = c( colCall( imports.ion2, 'NO3')),
                         nh4  = c( colCall( imports.ion2, 'NH4')))
  } else{
    date1 <- foo( strptime( imports.ion1$DATE.TIME,
                            format = '%m/%d/%Y %H:%M',
                            tz = 'UTC'),
                  year = 2004)
    date2 <- foo( strptime( imports.ion2[,'Date/Time[LST]'],
                            format = '%m/%d/%Y %H:%M',
                            tz = 'UTC'),
                  year = 2030)
    Ion.df <- data.frame(date = c( date1, date2),
                         so4  = c( colCall( imports.ion1, 'SO4'), colCall( imports.ion2, 'SO4')),
                         no3  = c( colCall( imports.ion1, 'NO3'), colCall( imports.ion2, 'NO3')),
                         nh4  = c( colCall( imports.ion1, 'NH4'), colCall( imports.ion2, 'NH4')))
  }

  rows.pm25 <- imports.pm[,grep( 'FRM.Mass|PM2.5.Mass..ug.m3.|X.FRM.PM2.5.Mass..ug.m3.', colnames( imports.pm))]
  PM.df <- data.frame( date = dates.pm,
                       pm25 = rowSums( rows.pm25, na.rm = T) *
                         ifelse( rowSums( is.na( rows.pm25)) == ncol( rows.pm25), NA, 1))

  #Make sure each date has 24 hours
  Gas.df.out <- datetime_check( Gas.df,'hour')
  Met.df.out <- datetime_check( Met.df,'hour')
  Ion.df.out <- datetime_check( Ion.df,'hour')
  PM.df.out  <- datetime_check( PM.df, 'day')

  out = list( gas = Gas.df.out,
              met = Met.df.out,
              pm = PM.df.out,
              ion = Ion.df.out)
  return( out)
}

#=============
