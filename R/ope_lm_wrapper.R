#calculate ope using lineal models - fit model by year
ope_lm_wrapper <- function( year,
                            dates){
  find.date <- grep( year, dates)

  date.use.year <- dates[find.date]
  o3.use.year <- o3.use[find.date]
  noz.use.year <- noz.use[find.date]
  if ( length( which( is.na( noz.use.year) == F)) == 0) {
    out <- rep( NA,length( date.use.year))
  } else {
    year.lm <- lm( o3.use.year ~ noz.use.year)

    ope <- coef(year.lm)[2]
    out <- rep(ope,length(date.use.year))
    #		date.frame(date = date.use.year,ope= ope,noz = noz.use.year,fit=spline.ope$fit)
  }
  return( out)
}

