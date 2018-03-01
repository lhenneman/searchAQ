#' calculate ope using lineal models - fit model by year
#'
#' \code{ope_lm_wrapper} takes as input a year and a vector of dates, and finds other variables in the.
#' This function is called by the ```ope_worker``` function.
#'
#' @param int assigned intercept
#' @param y response variable. In this use, often ozone
#' @param x independent variable. In this use, often NOz
#' @param base basis functions included in spline model (e.g., from ```make_splineOPE``` function)
#' @return This function returns avector of OPE's from the calculated spline fit
#'
ope_lm_wrapper <- function( year,
                            y,
                            x,
                            dates){
  find.date <- grep( year, dates)

  date.use.year <- dates[find.date]
  y.year <- y[find.date]
  x.year <- x[find.date]
  if ( length( which( is.na( x.year) == F)) == 0) {
    out <- rep( NA,length( date.use.year))
  } else {
    year.lm <- lm( y.year ~ x.year)

    ope <- coef(year.lm)[2]
    out <- rep(ope,length(date.use.year))
  }
  return( out)
}

