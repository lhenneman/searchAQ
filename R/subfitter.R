#' compute fit of subset of a linear model
#'
#' \code{subfitter} takes as input a dataframe and vector of named covariates, multiplies by each and sums the results
#'
#' @param coeffs vector of named coefficients, e.g., from a linear model
#' @param covars data.frame of covariates, including those named in coeffs
#' @return This function returns a vector of fits

subfitter <- function( coeffs,
                       covars){

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
