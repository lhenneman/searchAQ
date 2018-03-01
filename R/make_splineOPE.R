#function to estimate OPE using spline function - no equation included (yet)
make_splineOPE <- function(y,
                           x,
                           nk){
  #grab model fit vector
  basis <- rcs( x, nk)
  form <- reformulate( termlabels = c('basis'),
                       response = 'y',
                       intercept = TRUE)
  model <- ols( form)
  knots <- attr( basis,'parms')

  #get standard error of the intercept
  model2 <- lm( form)
  stder.int <- coef( summary( model2))['(Intercept)', 'Std. Error']

  #t represents the function in a string, so we
  #	need to extract the various peices
  t <- capture.output( Function( model))[1]

  #intercept - not used in the derivative, but good to have
  intercept <- str_extract( t, '[0-9]+\\.[0-9]+')
  intercept.txt <- paste( "Intercept =",
                          round( as.numeric( intercept), 1),
                          '±',
                          round( as.numeric( stder.int), 0),
                          'ppb')

  #remove the intercept from the string, extract the linear portion
  t.rem.intercept <- sub( paste( "^.*\\{.*",
                                 intercept,
                                 sep = ''),
                          "", t, perl = T)
  linear <- str_extract( t.rem.intercept, '[- +][0-9]+\\.[0-9]+')

  #remove the linear portion, extract the knot coefficients
  t.rem.linear <- sub( paste( '\\',
                              linear,
                              '..x',
                              sep = ''),
                       "", t.rem.intercept, perl=T)

  #for each knot, extract the coefficients
  # in deriv.comp, calculate each derivative part
  knot.vec <- rep( NA, length( knots))
  t.tmp <- rep( NA, length( knots) + 1)
  t.tmp[1] <- t.rem.linear
  deriv.components <- matrix( NA,
                              ncol = length( knots),
                              nrow = length( x))
  fit.components <- matrix( NA,
                            ncol = length( knots),
                            nrow = length( x))
  for (p in 1:length( knot.vec)){
    knot.vec[p] <- str_extract( t.tmp[p], '[- +][0-9]+\\.[0-9]+')
    str.tmp <- paste( "^\\",
                     knot.vec[p],
                     "\\*pmax\\(x[- +][0-9]+\\.[0-9]+,0\\)\\^3",
                     sep = '')
    t.tmp[p+1] <- sub( str.tmp, "", t.tmp[p], perl = T)

    deriv.components[,p] <- 3 * as.numeric( knot.vec[p]) * pmax( x - knots[p],0) ^ 2
  }

  #build the derivative from the extracted model parameters
  fit   = predict( model)
  deriv = as.numeric( linear) + rowSums( deriv.components)
  out = list( x = x,
              ope = deriv,
              fit = fit,
              intercept.txt = intercept.txt,
              base = basis,
             intercept = as.numeric( intercept),
             model.int = model2)
  return( out)
}
