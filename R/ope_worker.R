#==================================================#
#plot O3 vs. NOz for all pollutants for days with high PS
#==================================================#

#function that gets O3 and NOz for high-PS days, estimates OPE using log method
#	ps.perc is the PS percentile
#	rm.neg removes noz values (which are estimated using subtraction) corresponding
#		to negatives or lower 10th percentile (see inside function)
ope_worker <- function(gas,
                       knk = 3,
                       metric.select = 'ps',
                       ps.perc = 0.8,
                       rm.neg = T,
                       name = '',
                       loc = '',
                       lat = '',
                       lon = ''){

  #extract appropriate data
  date <- gas$o3_2p$data$date
  ps   <- gas$o3_2p$data$PS
  o3   <- gas$o3_2p$data$obs
  co   <- gas$co_2p$data$obs
  nox  <- gas$nox_2p$data$obs
  noy  <- gas$noy_2p$data$obs
  hno3 <- gas$hno3_2p$data$obs
  noz  <- noy - nox
  if(rm.neg==T) noz[noz <= quantile( noz,
                                     na.rm = T,
                                     probs = 0.1)] <- NA

  #select which ps are greater than percentile (new: option for using HNO3/NOy)
  if ( metric.select == 'hno3_noy'){
    ps.which <- which( hno3 / noy > quantile( hno3 / noy,
                                              probs = ps.perc,
                                              na.rm = T))
  } else if( metric.select == 'ps.low') {
    ps.which <- which( ps < quantile( ps,
                                      probs = ps.perc,
                                      na.rm = T))
  } else if( metric.select == 'ps'){
    ps.which <- which( ps > quantile( ps,
                                      probs = ps.perc,
                                      na.rm = T))
  } else {
    warning('No selection criteria, using all data...')
    ps.which <- seq_along( ps)
  }

  #choose data to use based on ps.which
  date.use <- date[ps.which]
  o3.use   <- o3[ps.which]
  co.use   <- co[ps.which]
  nox.use  <- nox[ps.which]
  noy.use  <- noy[ps.which]
  noz.use  <- noz[ps.which]
  hno3.use <- hno3[ps.which]
  ps.use   <- ps[ps.which]

  #calculate eqn and ope
  ope.eqn <- lm_eqn_df2(y = o3.use,
                        x = noz.use)

  #calculate ope using splines
  spline.ope <- make_splineOPE( y = o3.use,
                                x = noz.use,
                                nk = knk)

  #calculate ope 95% confidence interval for OPE at each noz concentration
  beta <- spline.ope$model.int$coef
  cov <- vcov( spline.ope$model.int)
  int.sim <- mvrnorm( 5000,
                      beta,
                      cov,
                      empirical = T)[,'(Intercept)']
  basis <- spline.ope$base

  #run models for each intercept sample
  OPE.samps <- sapply( int.sim,
                       deriv_int,
                       noz.use,
                       o3.use,
                       basis)

  #select upper and lower bounds of 95% CI
  OPE_025 <- apply( OPE.samps,
                    1,
                    quantile,
                    0.025,
                    na.rm = T)
  OPE_975 <- apply( OPE.samps,
                    1,
                    quantile,
                    0.975,
                    na.rm = T)
  OPE_lims <- data.frame( low = OPE_025,
                          high = OPE_975)

  #calculate ope using lineal models - fit model by year
  ope.lm.years <- unlist( sapply(years,
                                 ope_lm_wrapper,
                                 dates = date.use,
                                 simplify = T))

  #create output data frame
  out = data.frame(date        = date.use,
                   ps          = ps.use,
                   o3          = o3.use,
                   noz         = noz.use,
                   nox         = nox.use,
                   noy         = noy.use,
                   co          = co.use,
                   hno3        = hno3.use,
                   ope.lin     = ope.eqn$ope.lin,
                   ope.log     = ope.eqn$ope.log,
                   ope.spl     = spline.ope$ope,
                   ope.lm.yr   = ope.lm.years,
                   ope.lims    = OPE_lims,
                   fit.lin     = ope.eqn$fit.lin,
                   fit.log     = ope.eqn$fit.log,
                   fit.spl     = spline.ope$fit,
                   eqn.log     = ope.eqn$eq,
                   spl.int     = spline.ope$intercept.txt,
                   spl.int.n   = spline.ope$intercept,
                   name        = name,
                   loc         = loc,
                   lat         = lat,
                   lon         = lon,
                   x.eqn       = 20,
                   y.eqn       = 25,
                   row.names   = NULL)

  return( out)
}
