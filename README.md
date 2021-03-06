# ```searchAQ```

```searchAQ``` is an R package that includes utility to read in SEARCH air quality network data in its original format and combine it into consistent lists and data frames. Other included functions perform detrending, and calculate photochemical state and ozone production efficiency (OPE) from observations.

# Getting started
Download all SEARCH data from Dropbox, using one of these two links (the first is still hosted by ARA, the second is hosted on my own Dropbox):
1) https://www.dropbox.com/sh/o9hxoa4wlo97zpe/AACbm6LetQowrpUgX4vUxnoDa?dl=0
2) https://www.dropbox.com/sh/1tsdqdmiwixbgfd/AACxwbKilNVbC52H1VH4geJma?dl=0

Download the entire directory, maintaining the original structure. You should end up with a single SEARCH directory with subdirectories named for each year.

### Install ```searchAQ```
In R or RStudio, install the ```searchAQ``` R package from github.  

#### Install and load ```devtools```
```
install.packages( c('devtools'))
library(devtools)
```

#### Install and load ```searchAQ```
```{r}
devtools::install_github("lhenneman/searchAQ")
library(searchAQ)
```

# Importing the data
searchAQ relies on the original file formats for SEARCH data, which change in different years (2008, for example, saw a big change in file format). For Jefferson Street, for example:
```{r}
JSTdata <- read_SEARCH('JST',loc.search)
```
Where ```loc.search``` is the local path of your SEARCH data (i.e., the directory that contains subdirectories named for each year). ```JSTdata``` will have the structure of a list containing hourly gas, meteorology, and ion measurements, and daily PM2.5:

```{r}
summary(JSTdata)
#     Length Class      Mode
# gas 9      data.frame list
# met 8      data.frame list
# pm  2      data.frame list
# ion 4      data.frame list

summary(JSTdata$gas[, 1:4])
#      date                           o3                co              so2         
# Min.   :1998-07-31 20:00:00   Min.   : -3.487   Min.   :-829.0   Min.   : -0.766  
# 1st Qu.:2002-09-08 01:45:00   1st Qu.:  8.153   1st Qu.: 192.8   1st Qu.:  0.490  
# Median :2006-10-16 07:30:00   Median : 21.627   Median : 254.2   Median :  1.381  
# Mean   :2006-10-16 07:30:00   Mean   : 25.019   Mean   : 377.5   Mean   :  3.533  
# 3rd Qu.:2010-11-23 12:15:00   3rd Qu.: 36.582   3rd Qu.: 380.5   3rd Qu.:  3.710  
# Max.   :2014-12-31 18:00:00   Max.   :167.186   Max.   :7687.5   Max.   :129.454  
#                               NA's   :5848      NA's   :8651     NA's   :8922     

```

# Creating daily metrics
The ```metrics_worker``` utility calculates daily metrics from hourly concentrations. For example,
```{r}
o3_md8a <- metrics_worker( x = JSTdata$gas$o3, 
                           metric = 'mda8', 
                           datehour = JSTdata$gas$date, 
                           species = 'o3')

summary( o3_md8a)
#      date               o3_mda8         
# Min.   :1998-08-01   Min.   :  0.03817  
# 1st Qu.:2002-09-08   1st Qu.: 26.81612  
# Median :2006-10-16   Median : 38.39598  
# Mean   :2006-10-16   Mean   : 41.09705  
# 3rd Qu.:2010-11-23   3rd Qu.: 53.73363  
# Max.   :2014-12-31   Max.   :130.81412  
#                      NA's   :80         
```

The ```metric``` option in ```metrics_worker``` has multiple potential values corresponding to potentially useful daily metrics (more can be added later):
- 'mda8' - maximum daily 8 hr average
- 'afternoon' - mean of hourly values from 11am-7pm
- 'sum' - sum of hourly values
- 'midday' - mean of hourly values from 11am-3pm
- 'morning' - mean of hourly values from 8am-11pm
- 'max' - 24h maximum
- '2p' - 2pm hourly value

```dailymetrics_SEARCH_gm``` is a built in function to calculate various daily metrics of all of the species included in output from ```read_SEARCH```
```{r}
JSTmetrics <- dailymetrics_SEARCH_gm( JSTdata)

summary( JSTmetrics)
#     Length Class      Mode
# gas 26     data.frame list
# met  8     data.frame list
# pm   2     data.frame list
# ion  7     data.frame list
```

# Detrending
```searchAQ``` contains functions that allow for the detrending of SEARCH data (and potentially other daily data) using the methods described in Henneman et al. (2015). Splitting into detrending components is acheived using the ```detrending_SEARCH_gm``` function. 

First, however, rainfall data needs to be downloaded and included. Potential future functionality could include the option to detrend without some variables, but these are all needed for now.

### Download and read in rainfall data

Airport rainfall data in the correct format can be downloaded here: https://www.ncdc.noaa.gov/cdo-web/orders?email=lhenneman@gmail.com&id=875479 (click RESUBMIT). ```input_NCDC_CDO_rf``` can then be used to read and format the data for detrending:

```{r}
rainfall <- input_NCDC_CDO_rf(file.rf)
```

Where ```file.rf``` is the path of the rainfall file downloaded from NCDC.

### Do the detrending
First, it is recommended to discard all values of species metrics that are below zero (alternatively, you may decide to use some sort of filling procedure for these values).
```{r}
JSTmetrics$gas[JSTmetrics$gas <= 0] <- NA
JSTmetrics$pm[JSTmetrics$pm <= 0] <- NA
JSTmetrics$ion[JSTmetrics$ion <= 0] <- NA
```

The ```detrending_SEARCH_gm``` function can then be used to detrend each species:
```{r}
JSTdetrending.gas.o3_md8a <- detrending_SEARCH_gm( n = "o3_mda8",
                                                   x.df = JSTmetrics$gas,
                                                   met = JSTmetrics$met,
                                                   rf = data.frame( rainfall$ATL))

summary(JSTdetrending.gas.o3_md8a)
#      Length Class      Mode   
# data 6      data.frame list   
# r.sq 1      -none-     numeric

names(JSTdetrending.gas.o3_md8a$data)
# [1] "date" "obs"  "STM"  "S"    "PS"   "Det" 
```
The result is a list with 2 objects. The second, ```r.sq```: R^2 fit of reconstructed signal (Table 3 in Henneman et al. (2015)). The first, ```data``` is a data.frame with the following columns:
- date: a vector of dates
- obs: osberved daily metrics input into detrending function
- STM: short-term meteorology portion of signal
- S: seasonal (annual) portion of signal
- PS: photochemical state (PS from MDA8hO3 discussed in detail by Henneman et al. (2017))
- Det: Meteorologically detrended data 

The detrending can easily be run for all species and metric combinations at a single site using ```sapply```:
```{r}
nlist.gas = colnames(JSTmetrics$gas)[-1]
nlist.pm  = colnames(JSTmetrics$pm)[-1]

JSTdetrending.gas <- sapply( nlist.gas,
                             detrending_SEARCH_gm,
                             x.df = JSTmetrics$gas,
                             met = JSTmetrics$met,
                             rf = data.frame( rainfall$ATL),
                             simplify = F,
                             USE.NAMES = T)
JSTdetrending.pm <- sapply( nlist.pm,
                             detrending_SEARCH_gm,
                             x.df = JSTmetrics$pm,
                             met = JSTmetrics$met,
                             rf = data.frame( rainfall$ATL),
                             simplify = F,
                             USE.NAMES = T)

```

# Ozone Production Efficiency (OPE)
OPE is a way to estimate the impacts of NOx emissions controls on ozone concentrations, and has been estimated for stationary monitors using various approaches. ```searchAQ``` incorporates many of these as described by Henneman et al. (2018). To calculate the spline-based OPE with 3 knots and other OPE approaches at Jefferson Street (JST), for example, use:
```{r}
JSTope <- ope_worker( gas = JSTdetrending.gas,
                      knk = 3,
                      ps.perc = 0.8,
                      rm.neg = T,
                      name = 'JST',
                      loc = 'urban',
                      lat = 33.776,
                      lon = -84.413)
dim( JSTope)
# [1] 863  26

```
```ope_worker``` takes many inputs:
- ```gas```: a list object output by applying ```detrending_SEARCH_gm``` for multiple species (see example above)
- ```knk```: number of knots for spline modeling of O3 vs. NOz (defaults to 3)
- ```ps.perc```: percentile cutoff for atmospheric photochemistry metric. Defaults to 0.8, i.e., days with greater than the 80th percentile are used
- ```metric.select```: one of ```'hno3_noy'``` (HNO3 / NOy), ```'ps'```, or ```'ps.low'``` for ```ps``` taken below the given ```ps.perc```.
- ```rm.neg```: removes noz values (which are estimated using subtraction) corresponding to negatives or lower 10th percentile (see inside function). Defaults to TRUE
- ```name```: optional. monitoring station name
- ```loc```: optional. monitoring station location description, e.g., ```c( 'Urban', 'Rural', 'Suburban')``` 
- ```lat```: optional. monitoring station latitude
- ```lon```: optional. monitoring station longitude

The result is a data table with 26 rows and rows totalling the number of observations that meet the criteria (here, PS higher than the 80th percentile):
- ```date```: dates
- ```ps```: ps
- ```o3```: ozone
- ```noz```: NOz
- ```nox```: NOx
- ```noy```: NOy
- ```co```: CO
- ```hno3```: HNO3
- ```ope.lin```: linear (constant) OPE: O3 = OPE * NOz + intercept
- ```ope.log```: log OPE: O3 = m * log( NOz) + intercept; OPE = m / NOz
- ```ope.spl```: spline OPE, OPE derived from slope of spine form
- ```ope.lm.yr```: OPE by year as slope of daily O3 vs NOz in each year
- ```ope.lims.low```: spline OPE 97.5% confidence interval derived from MCMC sampling
- ```ope.lims.high```: spline OPE 2.5% confidence interval derived from MCMC sampling
- ```fit.lin```: O3 as simulated with the linear model
- ```fit.log```: O3 as simulated with the log model
- ```fit.spl```: O3 as simulated with the spline model
- ```eqn.log```: equation used to fit the log model
- ```spl.int```: string. intercept from the spline model (taken by Henneman et al. 2018 as the regional background ozone concentration)
- ```spl.int.n```: numeric. intercept from the spline model (taken by Henneman et al. 2018 as the regional background ozone concentration)
- ```name```: station name
- ```loc```: station location description taken from function inputs
- ```lat```: station latitude taken from function inputs
- ```lon```: station latitude taken from function inputs
- ```x.eqn```: numeric. equation location for plotting.
- ```y.eqn```: numeric. equation location for plotting.

OPE results can easily be visualized. The following code reproduces the Jefferson Street (JST) panel of Figure 1 in Henneman et al. (2017):
```{r}
ggplot( data = JSTope,
        aes( x = noz,
             y = o3,
             colour = ope.spl)) +
  geom_point( size = .75) +
  geom_line( aes( y = fit.spl),
             color = 'black') +
  geom_label(aes( x = x.eqn,
                  y = y.eqn,
                  label = spl.int),
             parse = FALSE,
             inherit.aes = F,
             size = 7,
             show.legend = F) +
  scale_colour_gradientn( colours = c( 'orange', 'green', 'blue'),
                          name = 'OPE') +
  guides( color = guide_colourbar( title.position = 'left')) +
  ylab( 'Ozone, ppb') +
  xlab( 'NOz, ppb') +
  expand_limits( x = 0,
                 y = 0) +
  theme_bw() +
  theme(axis.text = element_text( size=14),
        axis.title = element_text( size=16),
        legend.background = element_rect(),
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        legend.text = element_text( size = 11),
        legend.title = element_text( size = 12),
        strip.text = element_text( size = 16),
        plot.title = element_text( size = 20),
        strip.background = element_rect( fill='white'))
```
![Alt text](figures/JST_Figure1_Hennemanetal2017.png)


# References
Henneman, Lucas RF, Heather A Holmes, James A Mulholland, and Armistead G Russell. 2015. “Meteorological Detrending of Primary and Secondary Pollutant Concentrations: Method Application and Evaluation Using Long-Term (2000-2012) Data in Atlanta.” Atmospheric Environment 119. Elsevier Ltd: 201–10. doi:10.1016/j.atmosenv.2015.08.007.

Henneman, Lucas RF, Howard H Chang, David Lavoue, James A Mulholland, and Armistead G Russell. 2017. “Accountability Assessment of Regulatory Impacts on Ozone and PM2.5 Concentrations Using Statistical and Deterministic Pollutant Sensitivities.” Air Quality Atmosphere and Health 10 (6): 695–711. doi:10.1007/s11869-017-0463-2.

Henneman, Lucas RF, Huizhong Shen, Cong Liu, Yongtao Hu, James A Mulholland, and Armistead G Russell. 2018. “Responses in Ozone and Its Production Efficiency Attributable to Recent and Future Emissions Changes in the Eastern United States.” Environmental Science & Technology 2017 51 (23), 13797-13805. DOI: 10.1021/acs.est.7b04109.
