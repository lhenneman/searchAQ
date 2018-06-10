#' labeling function for equation (and calculate linear & log OPEs)
#'
#' \code{lm_eqn_df2} takes as input two vectors (```x``` and ```y```), calculates linear and log linear
#' model fits and equation in R format. This function is called by the ```ope_worker``` function.
#'
#' @param y response variable. In this use, often ozone
#' @param x independent variable. In this use, often NOz
#' @return This function returns a list with the log equation, and linear and log OPEs and fits (predicted ozone)
lm_eqn_df2 = function(y,
                      x){
  x[x <= 0] = NA
  m = lm( y ~ log(x), na.action = 'na.exclude')
  m.lin = lm(y ~ x, na.action = 'na.exclude');
  l <- list( a = format( abs( coef( m)[1]), digits = 2),
             a.se = format( abs( coef( summary( m))[1, 2]), digits = 1),
             b = format( coef( m)[2], digits = 2),
             b.se = format( abs( coef( summary( m))[2, 2]), digits = 1),
             r2 = round( summary( m)$r.squared, digits = 2));
  if( is.na( coef(m)[2]) == T) {
    eq <- NA
  } else if( coef(m)[1] >= 0)  {
    eq <- substitute( italic(y) == (b %+-% b.se) %.% italic(logx) + (a %+-% a.se)*","~~italic(R)^2~"="~r2,l)
  } else {
    eq <- substitute( italic(y) == (b %+-% b.se) %.% italic(logx) - (a %+-% a.se)*","~~italic(R)^2~"="~r2,l)
  }
  out <- list( eq = as.character( as.expression( eq)),
               ope.log = coef( m)[2] / x,
               fit.log = predict( m),
               ope.lin = coef( m.lin)[2],
               fit.lin = predict( m.lin))
  return( out)
}

