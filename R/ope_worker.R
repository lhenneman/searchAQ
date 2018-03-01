#' stratify input by photochemistry metric and calculate OPE
#'
#' \code{ope_worker} takes as input a list of daily gas and meteorology
#' species metrics (e.g., from the \code{detrending_SEARCH_gm} function) and outputs a data.frame
#' of multiple OPE calculations, intercepts, and other information for the set of days that match the
#' criteria for photochemical state
#'
#' @param gas list of daily gaseous species metrics (specifically including the 2p metric). E.g., from the
#'   \code{detrending_SEARCH_gm} function
#' @param knk number of knots for spline modeling of O3 vs. NOz (defaults to 3)
#' @param ps.perc percentile cutoff for atmospheric photochemistry metric. Defaults to 0.8, i.e., days with greater than the 80th percentile are used
#' @param metric.select one of ```'hno3_noy'``` (HNO3 / NOy), ```'ps'```, or ```'ps.low'``` for ```ps``` taken below the given ```ps.perc```.
#' @param rm.neg removes noz values (which are estimated using subtraction) corresponding to negatives or lower 10th percentile (see inside function). Defaults to TRUE
#' @param name optional. monitoring station name
#' @param loc optional. monitoring station location description, e.g., ```c( 'Urban', 'Rural', 'Suburban')```
#' @param lat optional. monitoring station latitude
#' @param lon optional. monitoring station longitude
#' @return This function returns a data.frame of length equal to number of days that meet photochemical state criteria with the following columns:
#' #'\enumerate{
#'   \item mda8 - mean daily 8 hr average
#'   \item date dates
#'   \item ps ps
#'   \item o3 ozone
#'   \item noz NOz
#'   \item nox NOx
#'   \item noy NOy
#'   \item co CO
#'   \item hno3 HNO3
#'   \item ope.lin linear (constant) OPE: O3 = OPE * NOz + intercept
#'   \item ope.log log OPE: O3 = m * log( NOz) + intercept; OPE = m / NOz
#'   \item ope.spl spline OPE: OPE derived from slope of spine form
#'   \item ope.lm.yr OPE by year as slope of daily O3 vs NOz in each year
#'   \item ope.lims.low spline OPE 97.5% confidence interval derived from MCMC sampling
#'   \item ope.lims.high spline OPE 2.5% confidence interval derived from MCMC sampling
#'   \item fit.lin O3 as simulated with the linear model
#'   \item fit.log O3 as simulated with the log model
#'   \item fit.spl O3 as simulated with the spline model
#'   \item eqn.log equation used to fit the log model
#'   \item spl.int string. intercept from the spline model (taken by Henneman et al. 2018 as the regional background ozone concentration)
#'   \item spl.int.n numeric. intercept from the spline model (taken by Henneman et al. 2018 as the regional background ozone concentration)
#'   \item name station name
#'   \item loc station location description taken from function inputs
#'   \item lat station latitude taken from function inputs
#'   \item lon station latitude taken from function inputs
#'   \item x.eqn numeric. equation location for plotting.
#'   \item y.eqn numeric. equation location for plotting.
#' }

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
