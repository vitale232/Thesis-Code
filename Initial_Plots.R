#--------------------------------
# Name:         Initial_Plots.R
# Purpose:      Take a look at the data
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/08/25
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
lines(ra$date, ra$tair, lty=2, lwd=2)

#### time series plot of max temperature
x11(height=7, width=12)
plot(m_tmx$date, m_tmx[ , 2], type = 'l', xlab = 'Date', ylab = 'Maximum Temperature (°C)',
     ylim = c(ymn, ymx), main = '2m Maximum Temperature')
for(i in 3:ncol(m_tmx)){
  lines(m_tmx$date, m_tmx[ , i], col=i, type='l')
}
lines(ra$date, ra$tair, lty=2, lwd=2)

#### time series plot of min temperature
x11(height=7, width=12)
plot(m_tmn$date, m_tmn[ , 2], type = 'l', xlab = 'Date', ylab = 'Minimum Temperature (°C)',
     ylim = c(ymn, ymx), main = '2m Minimum Temperature')
for(i in 3:ncol(m_tmn)){
  lines(m_tmn$date, m_tmn[ , i], col=i, type='l')
}
lines(ra$date, ra$tair, lty=2, lwd=2)




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
lines(ra$date, ra$tair, lty=2, lwd=2)

#### time series plot of max temperature
x11(height=7, width=12)
plot(g_tmx$date, g_tmx[ , 2], type = 'l', xlab = 'Date', ylab = 'Maximum Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Maximum Temperature')
for(i in 3:ncol(g_tmx)){
  lines(g_tmx$date, g_tmx[ , i], col=i, type='l')
}
lines(ra$date, ra$tair, lty=2, lwd=2)

#### time series plot of min temperature
x11(height=7, width=12)
plot(g_tmn$date, g_tmn[ , 2], type = 'l', xlab = 'Date', ylab = 'Minimum Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Minimum Temperature')
for(i in 3:ncol(g_tmn)){
  lines(g_tmn$date, g_tmn[ , i], col=i, type='l')
}
lines(ra$date, ra$tair, lty=2, lwd=2)




# ********************************************************************************
###############################
# TDIFF

#### calculate y limits
ymx = ceiling(max(c(apply(d_tmx[ , -1], 1, max, na.rm = TRUE),
                    apply(d_tmn[ , -1], 1, max, na.rm = TRUE),
                    apply(d_tmx[ , -1], 1, min, na.rm = TRUE),
                    apply(d_tmn[ , -1], 1, min, na.rm = TRUE))))
ymn = ceiling(min(c(apply(d_tmx[ , -1], 1, max, na.rm = TRUE),
                    apply(d_tmn[ , -1], 1, max, na.rm = TRUE),
                    apply(d_tmx[ , -1], 1, min, na.rm = TRUE),
                    apply(d_tmn[ , -1], 1, min, na.rm = TRUE))))
#### time series plot of mean temperature
x11(height=7, width=12)
plot(d_tav$date, d_tav[ , 2], type = 'l', xlab = 'Date', ylab = 'Mean Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Mean Tdiff')
for(i in 3:ncol(d_tav)){
  lines(d_tav$date, d_tav[ , i], col=i, type='l')
}
lines(ra$date, ra$tair, lty=2, lwd=2)

#### time series plot of max temperature
x11(height=7, width=12)
plot(d_tmx$date, d_tmx[ , 2], type = 'l', xlab = 'Date', ylab = 'Maximum Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Maximum Tdiff')
for(i in 3:ncol(d_tmx)){
  lines(d_tmx$date, d_tmx[ , i], col=i, type='l')
}
lines(ra$date, ra$tair, lty=2, lwd=2)

#### time series plot of min temperature
x11(height=7, width=12)
plot(d_tmn$date, d_tmn[ , 2], type = 'l', xlab = 'Date', ylab = 'Minimum Temperature (°C)',
     ylim = c(ymn, ymx), main = 'Ground Level Minimum Tdiff')
for(i in 3:ncol(d_tmn)){
  lines(d_tmn$date, d_tmn[ , i], col=i, type='l')
}
lines(ra$date, ra$tair, lty=2, lwd=2)


###########################
## CREATE A SPACETIME OBJECT

#### reusable info where so is spatial object (spdf), id is column names
#### in order of increasing elevation, and space is those same names as a list
so = snake[order(snake$elev), ]
id = so$ID
space = list(values=so$ID)

#### Apply to 2m
m_tmx_st = stConstruct(x=m_tmx[ , id], space=space, time=m_tmx$date, SpatialObj=so)
m_tmn_st = stConstruct(x=m_tmn[ , id], space=space, time=m_tmn$date, SpatialObj=so)
m_tav_st = stConstruct(x=m_tav[ , id], space=space, time=m_tav$date, SpatialObj=so)

#### Apply to ground
g_tmx_st = stConstruct(x=g_tmx[ , id], space=space, time=g_tmx$date, SpatialObj=so)
g_tmn_st = stConstruct(x=g_tmn[ , id], space=space, time=g_tmn$date, SpatialObj=so)
g_tav_st = stConstruct(x=g_tav[ , id], space=space, time=g_tav$date, SpatialObj=so)

#### Apply to diff
d_tmx_st = stConstruct(x=d_tmx[ , id], space=space, time=g_tmx$date, SpatialObj=so)
d_tmn_st = stConstruct(x=d_tmn[ , id], space=space, time=g_tmn$date, SpatialObj=so)
d_tav_st = stConstruct(x=d_tav[ , id], space=space, time=g_tav$date, SpatialObj=so)


#### Make 'xt' type stplots
cols = colorRampPalette(c('deepskyblue', 'red'))(255)
x11('', 8, 8)
stplot(m_tmx_st, mode='xt', col.regions=cols, main = '2m Tmx')
x11('', 8, 8)
stplot(m_tmn_st, mode='xt', col.regions=cols, main = '2m Tmn')
x11('', 8, 8)
stplot(m_tav_st, mode='xt', col.regions=cols, main = '2m Tav')

x11('', 8, 8)
stplot(g_tmx_st, mode='xt', col.regions=cols, main = 'Ground Tmx')
x11('', 8, 8)
stplot(g_tmn_st, mode='xt', col.regions=cols, main = 'Ground Tmn')
x11('', 8, 8)
stplot(g_tav_st, mode='xt', col.regions=cols, main = 'Ground Tav')

x11('', 8, 8)
stplot(d_tmx_st, mode='xt', col.regions=cols, main = 'Diff Tmx')
x11('', 8, 8)
stplot(d_tmn_st, mode='xt', col.regions=cols, main = 'Diff Tmn')
x11('', 8, 8)
stplot(d_tav_st, mode='xt', col.regions=cols, main = 'Diff Tav')




################################################################
# Annual average vs elevation
####Tav

avs = snake[, c('ID', 'elev')]
avs$elev_km = avs$elev/1000
avs$mtav = apply(m_tav[ , -1], 2, mean, na.rm=TRUE)
avs$mtmn = apply(m_tmn[ , -1], 2, mean, na.rm=TRUE)
avs$mtmx = apply(m_tmx[ , -1], 2, mean, na.rm=TRUE)

### Ground level
avs$gtav = apply(g_tav[ , -1], 2, mean, na.rm=TRUE)
avs$gtmn = apply(g_tmn[ , -1], 2, mean, na.rm=TRUE)
avs$gtmx = apply(g_tmx[ , -1], 2, mean, na.rm=TRUE)

#### Get rid of the sensors that are missing tons of data
avs[which(avs$ID == 'D4P02'), c('mtav', 'mtmn', 'mtmx')] = c(NA, NA, NA)
avs[which(avs$ID == 'D3P10'), c('mtav', 'mtmn', 'mtmx')] = c(NA, NA, NA)
# avs = avs[which(avs$ID == 'D3P10'), ]

#### Linear model of 2m tav annual mean predicted by elevation
lm1 = lm(mtav ~ elev_km, avs)
lm2 = lm(mtmn ~ elev_km, avs)
lm3 = lm(mtmx ~ elev_km, avs)

x11()
plot(mtav ~ elev_km, avs, xlab='Elevation (km)', ylab='Mean Annual Tav (°C)',
     main='2m Tav')
abline(lm1, col='red', lty=2, lwd=2)
summary(lm1)
rlab = bquote(italic(R)^2 == .(format(summary(lm1)$r.squared, digits=2)))
text(3.2, 10, 
     labels=paste('y = ', round(coef(lm1)[1], 2), ' + ', round(coef(lm1)[2], 2), 
                  ' x', sep=''))
text(3.2, 9.55, labels=rlab)



x11()
plot(mtmn ~ elev_km, avs, xlab='Elevation (km)', ylab='Mean Annual Tmn (°C)',
     main='2m Tmn')
abline(lm2, col='red', lty=2, lwd=2)
summary(lm2)
rlab = bquote(italic(R)^2 == .(format(summary(lm2)$r.squared, digits=2)))
text(3.2, 4.5, 
     labels=paste('y = ', round(coef(lm2)[1], 2), ' + ', round(coef(lm2)[2], 2), 
                  ' x', sep=''))
text(3.2, 4.1, labels=rlab)



x11()
plot(mtmx ~ elev_km, avs, xlab='Elevation (km)', ylab='Mean Annual Tmx(°C)',
     main='2m Tmx')
abline(lm3, col='red', lty=2, lwd=2)
summary(lm3)
rlab = bquote(italic(R)^2 == .(format(summary(lm3)$r.squared, digits=2)))
text(3.2, 19.75, 
     labels=paste('y = ', round(coef(lm3)[1], 2), ' + ', round(coef(lm3)[2], 2), 
                  ' x', sep=''))
text(3.2, 19.1, labels=rlab)


### Ground levels
lm7 = lm(gtav ~ elev_km, avs)
lm8 = lm(gtmn ~ elev_km, avs)
lm9 = lm(gtmx ~ elev_km, avs)


x11()
plot(gtav ~ elev_km, avs, xlab='Elevation (km)', ylab='Mean Annual Tav',
     main='Ground Level Tav')
abline(lm7, col='red', lty=2, lwd=2)
summary(lm7)
rlab = bquote(italic(R)^2 == .(format(summary(lm7)$r.squared, digits=2)))
text(3.2, 10.5, 
     labels=paste('y = ', round(coef(lm7)[1], 2), ' + ', round(coef(lm7)[2], 2), 
                  ' x', sep=''))
text(3.2, 10.1, labels=rlab)


x11()
plot(gtmn ~ elev_km, avs, xlab='Elevation (km)', ylab='Mean Annual Tmn',
     main='Ground Level Tmn')
abline(lm8, col='red', lty=2, lwd=2)
summary(lm8)
rlab = bquote(italic(R)^2 == .(format(summary(lm8)$r.squared, digits=2)))
text(3.2, 4.75, 
     labels=paste('y = ', round(coef(lm8)[1], 2), ' + ', round(coef(lm8)[2], 2), 
                  ' x', sep=''))
text(3.2, 4.4, labels=rlab)



x11()
plot(gtmx ~ elev_km, avs, xlab='Elevation (km)', ylab='Mean Annual Tmx',
     main='Ground Level Tmx')
abline(lm9, col='red', lty=2, lwd=2)
summary(lm9)
rlab = bquote(italic(R)^2 == .(format(summary(lm9)$r.squared, digits=2)))
text(3.2, 21, 
     labels=paste('y = ', round(coef(lm9)[1], 2), ' + ', round(coef(lm9)[2], 2), 
                  ' x', sep=''))
text(3.2, 20.4, labels=rlab)




################################################################
# Annual tdiffs vs elevation
####Tav

difs = snake[, c('ID', 'elev', 'tci', 'ccm.pct')]
difs$elev_km = difs$elev/1000
difs$dtav = apply(d_tav[ , -1], 2, mean, na.rm=TRUE)
difs$dtmn = apply(d_tmn[ , -1], 2, mean, na.rm=TRUE)
difs$dtmx = apply(d_tmx[ , -1], 2, mean, na.rm=TRUE)

#### Get rid of the sensors that are missing tons of data
difs = difs[-which(difs$ID == 'D4P02'), ]
difs = difs[-which(difs$ID == 'D3P10'), ]

lm4 = lm(dtav ~ elev_km, difs)
lm5 = lm(dtmn ~ elev_km, difs)
lm6 = lm(dtmx ~ elev_km, difs)


x11()
plot(dtav ~ elev_km, difs, xlab='Elevation (km)', ylab='Mean Annual Tdiff for Tav (°C)',
     main='2m Tdiff Tav')
abline(lm4, col='red', lty=2, lwd=2)
summary(lm4)
rlab = bquote(italic(R)^2 == .(format(summary(lm4)$r.squared, digits=2)))
text(2.1, 1.9, 
     labels=paste('y = ', round(coef(lm4)[1], 2), ' + ', round(coef(lm4)[2], 2), 
                  ' x', sep=''))
text(2.1, 1.7, labels=rlab)
summary(lm4)





x11()
plot(dtmn ~ elev_km, difs, xlab='Elevation (km)', ylab='Mean Annual Tdiff for Tmn (°C)',
     main='2m Tdiff Tmn')
abline(lm5, col='red', lty=2, lwd=2)
summary(lm5)
rlab = bquote(italic(R)^2 == .(format(summary(lm5)$r.squared, digits=2)))
text(2.1, 4, 
     labels=paste('y = ', round(coef(lm5)[1], 2), ' + ', round(coef(lm5)[2], 2), 
                  ' x', sep=''))
text(2.1, 3.8, labels=rlab)
summary(lm5)




x11()
plot(dtmx ~ elev_km, difs, xlab='Elevation (km)', ylab='Mean Annual Tdiff for Tmx (°C)',
     main='2m Tdiff Tmx')
abline(lm6, col='red', lty=2, lwd=2)
summary(lm6)
rlab = bquote(italic(R)^2 == .(format(summary(lm6)$r.squared, digits=2)))
text(2.6, 2.25, 
     labels=paste('y = ', round(coef(lm6)[1], 2), ' + ', round(coef(lm6)[2], 2), 
                  ' x', sep=''))
text(2.6, 2.12, labels=rlab)
summary(lm5)



lm10 = lm(dtav ~ ccm.pct, difs)


