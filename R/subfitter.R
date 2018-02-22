#==================================================================================================
#
#					Functions to compute best fit of subset of a linear model
#
#					Order of covars should match coefs
#
#==================================================================================================

##more flexible version that matches coefs name to covars
subfitter <- function( coeffs,
                       covars,
                       na = TRUE){

  if ( is.null( dim( covars)) == T){
    fit.tot = coeffs * covars
  } else {
    coeffs[is.na( coeffs) == T] <- 0
    names.coefs <- names( coeffs)
    names.covars <- colnames( covars)

    if( '(Intercept)' %in% names.coefs){
      covars.fit <- as.matrix( covars[, names.coefs[-which( names.coefs == '(Intercept)')]])
      fit.tot <- covars.fit %*% coeffs[-which( names.coefs == '(Intercept)')] + coeffs['(Intercept)']
    } else {
      covars.fit <- as.matrix( covars[,names.coefs])
      fit.tot <- covars.fit %*% coeffs
    }
  }


  return( fit.tot)
}
