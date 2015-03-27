#--------------------------------
# Name:         Exploratory_Analysis.R
# Purpose:      Start exploring the data for relationships
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/09/10
# R:            3.1.1
#--------------------------------

library(sp)
library(spacetime)


#### Read in the datasets
setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

## 2m
m_tav = read.csv('./Aggregated_Data/m-tav.csv')
m_tmx = read.csv('./Aggregated_Data/m-tmx.csv')
m_tmn = read.csv('./Aggregated_Data/m-tmn.csv')

## ground level
g_tav = read.csv('./Aggregated_Data/g-tav.csv')
g_tmx = read.csv('./Aggregated_Data/g-tmx.csv')
g_tmn = read.csv('./Aggregated_Data/g-tmn.csv')

## tdiff
d_tav = read.csv('./Aggregated_Data/d-tav.csv')
d_tmn = read.csv('./Aggregated_Data/d-tmn.csv')
d_tmx = read.csv('./Aggregated_Data/d-tmx.csv')

#### Set up the dates
m_tav$date = as.Date(m_tav$date)
m_tmx$date = as.Date(m_tmx$date)
m_tmn$date = as.Date(m_tmn$date)

g_tav$date = as.Date(g_tav$date)
g_tmx$date = as.Date(g_tmx$date)
g_tmn$date = as.Date(g_tmn$date)

d_tav$m.tav.date = as.Date(d_tav$m.tav.date)
d_tmx$m.tav.date = as.Date(d_tav$m.tav.date)
d_tmn$m.tav.date = as.Date(d_tav$m.tav.date)
names(d_tav)[1] = 'date'
names(d_tmx)[1] = 'date'
names(d_tmn)[1] = 'date'


#### Read in the site data and the reanalysis data
snake = read.csv('./Site-Data/snake2.csv', as.is=1)
coordinates(snake) = ~x_utm+y_utm
proj4string(snake) = CRS('+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs')

ra = read.csv('./Reanalysis/Reanalysis-Data.csv')
ra$date = as.Date(ra$date)


############################################
#### Explore basic pearsons with site data and 
#### reanalysis tair

##### Create a function called corMat.  The function takes a 
### column from the ra (reanalysis) data frame and builds
### a matrix showing correlation.  The matrix is structured
### such that the different temperature variables are the columns
### and each location is in a row.

### INPUTS: 
### reanalysisData = a vector of reanalysis data that are of the same length
###                  as the temperature data.
### ... = passes arguments to cor()

### OUTPUTS:
### returns a matrix of correlations with temperature variables as columns
### and locations as rows

### NOTES:
### As this function is written, it can only be used if the 2 m tav is read in as
### m_tav, 2 m tmn is read in as m_tmn, etc.  If the need arises, it should be
### easy to translate this function to be more generic.

corMat = function(reanalysisData, ...){
  m = matrix(nrow=40, ncol=6)
  colnames(m) = c('m_tav', 'm_tmx', 'm_tmn', 'g_tav', 'g_tmx', 'g_tmn')
  rownames(m) = 1:nrow(m)
  
  for(i in 2:ncol(m_tav)){
    rownames(m)[i-1] = names(m_tav)[i]
    m[i-1, 1] = cor(m_tav[-1,i], reanalysisData, ...)
    m[i-1, 2] = cor(m_tmx[-1,i], reanalysisData, ...)
    m[i-1, 3] = cor(m_tmn[-1,i], reanalysisData, ...)
    m[i-1, 4] = cor(g_tav[-1,i], reanalysisData, ...)
    m[i-1, 5] = cor(g_tmx[-1,i], reanalysisData, ...)
    m[i-1, 6] = cor(g_tmn[-1,i], reanalysisData, ...)
  }
  return(round(m, 3))
}

#### Use corMat on the different reanalysis datasets
tair  = corMat(ra$tair, use='complete.obs')
hgt   = corMat(ra$hgt, use='complete.obs')
omega = corMat(ra$omega, use='complete.obs')
rh    = corMat(ra$rh, use='complete.obs')
shum  = corMat(ra$shum, use='complete.obs')
uwnd  = corMat(ra$uwnd, use='complete.obs')
vwnd  = corMat(ra$vwnd, use='complete.obs')


############################################
#### Take a look at lapse rates

### Create a lapse function that will calculate lapse
### rates with temperature datasets that are structured
### such that time is rows, space is columns.  it will
### systematically run a linear model of elevation predicting
### each rows temperature data, thus the columns must be 
### in the same order as the elev input to make sure each
### elevation is aligned with the proper measurement.

### INPUTS:
### df          = the temperature data.frame structured in the proper
###               format, space as columns, rows as time
### elev        = elevation for each site in the same order as the
###               columns, such that each elev measurement is in the
###               same position as the associated column (not paying
###               attention to date)
### rm_date     = TRUE/FALSE, should a date column be removed? Defaults
###               TRUE
### date_column = if rm_date is TRUE, which column (by number) are the
###               dates in? Defaults 1
###
### OUTPUTS:
### a vector of lapse rates, one for each day (row of df)

lapse = function(df, elev, rm_date=TRUE, date_column=1){
  # create an empty list to store model summaries
  l = list()
  
  # linear model of each day's temperature (row in df) minus
  # the first element which is the date
  for(i in 1:nrow(df)){
    if(rm_date){
      v = as.vector(as.matrix(df[i, ][-date_column]))
    } else{
      v = as.vector(as.matrix(df[i, ]))
    }
    l[[i]] = summary(lm(v ~ elev))
  }
  
  return(sapply(l, function(x) coef(x)[2]))
  
}

#### convert elevation to km
elev <- snake$elev/1000

#### Calculate lapse rates for 2 m sensors
m_tav_lapse = lapse(m_tav, elev)
m_tmn_lapse = lapse(m_tmn, elev)
m_tmx_lapse = lapse(m_tmx, elev)


#### Calculate lapse rates for ground level sensors
## Set the snow covered ground level sensors to NA
g_tav[117:346, c('D4P02', 'D4P03', 'D4P04')] = NA
g_tmx[117:346, c('D4P02', 'D4P03', 'D4P04')] = NA
g_tmn[117:346, c('D4P02', 'D4P03', 'D4P04')] = NA

g_tav_lapse = lapse(g_tav, elev)
g_tmn_lapse = lapse(g_tmn, elev)
g_tmx_lapse = lapse(g_tmx, elev)

lapse_df = data.frame('date' = m_tav$date[-1],
                      'm_tav' = m_tav_lapse[-1],
                      'm_tmn' = m_tmn_lapse[-1],
                      'm_tmx' = m_tmx_lapse[-1],
                      'g_tav' = g_tav_lapse[-1],
                      'g_tmn' = g_tmn_lapse[-1],
                      'g_tmx' = g_tmx_lapse[-1])

#### Plot time series of lapse rates for 2 m sensors
ymx = max(c(m_tav_lapse, m_tmn_lapse, m_tmx_lapse,
            g_tav_lapse, g_tmn_lapse, g_tmx_lapse))
ymn = min(c(m_tav_lapse, m_tmn_lapse, m_tmx_lapse,
            g_tav_lapse, g_tmn_lapse, g_tmx_lapse))

x11(height=7, width=16)
plot(m_tav$date, m_tav_lapse, type='o', xlab='Date',
     ylab='Lapse Rate (°C/km)', ylim=c(ymn, ymx),
     main='2 m Lapse Rates')
lines(m_tmn$date, m_tmn_lapse, type='o', col='dodgerblue')
lines(m_tmx$date, m_tmx_lapse, type='o', col='darkred')
legend('topleft', legend=c('tav', 'tmn', 'tmx'), 
       col=c('black', 'dodgerblue', 'darkred'), lty=1, pch=1)

#### Plot time series of lapse rates for ground level sensors
x11(height=7, width=16)
plot(g_tav$date, g_tav_lapse, type='o', xlab='Date',
     ylab='Lapse Rate (°C/km)', ylim=c(ymn, ymx),
     main='Ground Level Lapse Rates')
lines(g_tmn$date, g_tmn_lapse, type='o', col='dodgerblue')
lines(g_tmx$date, g_tmx_lapse, type='o', col='darkred')
legend('topleft', legend=c('tav', 'tmn', 'tmx'), 
       col=c('black', 'dodgerblue', 'darkred'), lty=1, pch=1)

