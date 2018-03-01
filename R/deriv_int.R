
#function to get derivative with different intercepts
#	nk is number of knots
#	int is assigned intercept
deriv_int <- function(int,
                      x,
                      y,
                      basis){
  form <- reformulate( termlabels = c( 'basis',
                                       'offset(rep(int,length(y)))'),
                       response = 'y',
                       intercept = FALSE)
  model <- lm( form)
  knots <- attr( basis, 'parms')

  #extract readable form of function as string
  t <- eval( attr( rcspline.restate( knots,
                                     model$coef),
                   'function.text'))

  #intercept - not used in the derivative, but good to have
  intercept <- int
  intercept.txt <- paste("Intercept =",
                         round( as.numeric( intercept),
                                1),
                         'ppb')

  #extract the linear portion
  linear <- str_extract( t,
                         '[- +][0-9]+\\.[0-9]+|[0-9]+\\.[0-9]+')

  #remove the linear portion, extract the knot coefficients
  subclear <- ifelse( as.numeric( linear) > 0,
                      paste( linear, '\\*', ' ', 'X', sep = ''),
                     paste( '\\', linear, '\\*', ' ', 'X', sep = ''))
  t.rem.linear <- sub( subclear, "", t, perl = T)

  #for each knot, extract the coefficients
  # in deriv.comp, calculate each derivative part
  knot.vec <- rep( NA, length( knots))
  t.tmp <- rep( NA, length( knots) + 1)
  t.tmp[1] <- t.rem.linear
  deriv.components <- matrix( NA, ncol = length( knots), nrow = length( x))
  fit.components <- matrix( NA, ncol = length( knots), nrow = length( x))
  for (p in 1:length(knot.vec)){
    knot.vec[p] <- str_extract( t.tmp[p],
                                '[- +][0-9]+\\.[0-9]+')
    str.tmp <- paste("^\\",
                     knot.vec[p],
                     "\\*pmax\\(X[- +][0-9]+(\\.[0-9]+?)?,0\\)\\^3",
                     sep='')
    t.tmp[p+1] <- sub(str.tmp,
                      "",
                      t.tmp[p],
                      perl = T)

    deriv.components[,p] <- 3 * as.numeric(knot.vec[p]) * pmax(x - knots[p], 0) ^ 2
  }

  #build the derivative from the extracted model parameters
  #		fit   = predict(model, data.frame(x = x))
  deriv <- as.numeric( linear) + rowSums( deriv.components)
  return( deriv)
}

