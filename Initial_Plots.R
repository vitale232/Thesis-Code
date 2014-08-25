#--------------------------------
# Name:         Initial_Plots.R
# Purpose:      Take a look at the data
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/08/25
# R:            3.1.1
#--------------------------------

library(sp)
library(spacetime)


#### Read in the two meter data
setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

m_tav = read.csv('./Aggregated_Data/m-tav.csv')
m_tmx = read.csv('./Aggregated_Data/m-tmx.csv')
m_tmn = read.csv('./Aggregated_Data/m-tmn.csv')

g_tav = read.csv('./Aggregated_Data/g-tav.csv')
g_tmx = read.csv('./Aggregated_Data/g-tmx.csv')
g_tmn = read.csv('./Aggregated_Data/g-tmn.csv')

#### Set up the dates
m_tav$date = as.Date(m_tav$date)
m_tmx$date = as.Date(m_tmx$date)
m_tmn$date = as.Date(m_tmn$date)

g_tav$date = as.Date(g_tav$date)
g_tmx$date = as.Date(g_tmx$date)
g_tmn$date = as.Date(g_tmn$date)


#### Read in the site data
snake = read.csv('./Site-Data/snake2.csv', as.is=1)
snake = snake[order(snake$elev), ]
coordinates(snake) = ~x_utm+y_utm
proj4string(snake) = CRS('+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs')


#*****************************************************************************************
#############################################################
# 2 METER

#### calculate y limits
ymx = ceiling(max(apply(m_tmx[ , -1], 1, max, na.rm = TRUE)))
ymn = floor(min(apply(m_tmn[ , -1], 1, min, na.rm = TRUE)))

#### time series plot of mean temperature
x11(height=7, width=12)
plot(m_tav$date, m_tav[ , 2], type = 'l', xlab = 'Date', ylab = 'Mean Temperature (°C)',
     ylim = c(ymn, ymx), main = '2m Mean Temperature')
for(i in 3:ncol(m_tav)){
  lines(m_tav$date, m_tav[ , i], col=i, type='l')
}

#### time series plot of max temperature
x11(height=7, width=12)
plot(m_tmx$date, m_tmx[ , 2], type = 'l', xlab = 'Date', ylab = 'Maximum Temperature (°C)',
     ylim = c(ymn, ymx), main = '2m Maximum Temperature')
for(i in 3:ncol(m_tmx)){
  lines(m_tmx$date, m_tmx[ , i], col=i, type='l')
}

#### time series plot of min temperature
x11(height=7, width=12)
plot(m_tmn$date, m_tmn[ , 2], type = 'l', xlab = 'Date', ylab = 'Minimum Temperature (°C)',
     ylim = c(ymn, ymx), main = '2m Minimum Temperature')
for(i in 3:ncol(m_tmn)){
  lines(m_tmn$date, m_tmn[ , i], col=i, type='l')
}




# ********************************************************************************
###############################
# GROUND LEVEL

#### calculate y limits
ymx = ceiling(max(apply(g_tmx[ , -1], 1, max, na.rm = TRUE)))
ymn = floor(min(apply(g_tmn[ , -1], 1, min, na.rm = TRUE)))
#### time series plot of mean temperature
x11(height=7, width=12)
plot(g_tav$date, g_tav[ , 2], type = 'l', xlab = 'Date', ylab = 'Mean Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Mean Temperature')
for(i in 3:ncol(g_tav)){
  lines(g_tav$date, g_tav[ , i], col=i, type='l')
}

#### time series plot of max temperature
x11(height=7, width=12)
plot(g_tmx$date, g_tmx[ , 2], type = 'l', xlab = 'Date', ylab = 'Maximum Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Maximum Temperature')
for(i in 3:ncol(g_tmx)){
  lines(g_tmx$date, g_tmx[ , i], col=i, type='l')
}

#### time series plot of min temperature
x11(height=7, width=12)
plot(g_tmn$date, g_tmn[ , 2], type = 'l', xlab = 'Date', ylab = 'Minimum Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Minimum Temperature')
for(i in 3:ncol(g_tmn)){
  lines(g_tmn$date, g_tmn[ , i], col=i, type='l')
}



###########################
# CREATE A SPACETIME OBJECT

space = list(values=snake$ID)
m_tmx_st = stConstruct(x=m_tmx, space=space, time=m_tmx$date, SpatialObj=snake)
m_tmn_st = stConstruct(x=m_tmn, space=space, time=m_tmn$date, SpatialObj=snake)
m_tav_st = stConstruct(x=m_tav, space=space, time=m_tav$date, SpatialObj=snake)

g_tmx_st = stConstruct(x=g_tmx, space=space, time=g_tmx$date, SpatialObj=snake)
g_tmn_st = stConstruct(x=g_tmn, space=space, time=g_tmn$date, SpatialObj=snake)
g_tav_st = stConstruct(x=g_tav, space=space, time=g_tav$date, SpatialObj=snake)

cols = colorRampPalette(c('deepskyblue', 'red'))(255)
stplot(m_tmx_st, mode='xt', col.regions=cols)
stplot(m_tmn_st, mode='xt', col.regions=cols)
stplot(m_tav_st, mode='xt', col.regions=cols)

stplot(g_tmx_st, mode='xt', col.regions=cols)
stplot(g_tmn_st, mode='xt', col.regions=cols)
stplot(g_tav_st, mode='xt', col.regions=cols)
