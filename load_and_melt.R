#--------------------------------
# Name:         load_and_melt.R
# Purpose:      Load the Snake Range sensor data and melt into long format
#               for modeling in nlme or elsewhere.
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/09/30
# R:            3.1.1
#--------------------------------
library(reshape)
library(sp)

if(!(exists(as.character(substitute(melt_them))))){
  melt_them = TRUE
}
if(!(exists(as.character(substitute(merge_them))))){
  merge_them = TRUE
}
if(!(exists(as.character(substitute(load_ra))))){
  load_ra = TRUE
}
if(!(melt_them)){
  merge_them = FALSE
  print('WARNING: merge_them set FALSE, as no new data to merge.')
}
if(!(exists(as.character(substitute(load_som))))){
  load_som = TRUE
}
if(!(exists(as.character(substitute(load_eof))))){
  load_eof = TRUE
}
if(!(exists(as.character(substitute(load_eof))))){
  load_wind = TRUE
}
#### Read in the datasets
setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

## 2m
m_tmx = read.csv('./Aggregated_Data/m-tmx.csv')
m_tmn = read.csv('./Aggregated_Data/m-tmn.csv')

## ground level
g_tmx = read.csv('./Aggregated_Data/g-tmx.csv')
g_tmn = read.csv('./Aggregated_Data/g-tmn.csv')

## tdiff
d_tmn = read.csv('./Aggregated_Data/d-tmn.csv')
d_tmx = read.csv('./Aggregated_Data/d-tmx.csv')

## site specific irradiance
irrad = read.csv('./Site-Data/irradiance.csv')

#### Set up the dates
m_tmx$date = as.Date(m_tmx$date)
m_tmn$date = as.Date(m_tmn$date)

g_tmx$date = as.Date(g_tmx$date)
g_tmn$date = as.Date(g_tmn$date)

names(d_tmx)[1] = 'date'
names(d_tmn)[1] = 'date'

d_tmx$date = as.Date(d_tmx$date)
d_tmn$date = as.Date(d_tmn$date)

irrad$date = as.Date(irrad$date)

wind = read.csv('./Aggregated_Data/wind.csv', header=TRUE)
wind$date = as.Date(wind$date, format='%m/%d/%Y %I:%M:%S %p')
wind$site_wind = apply(wind[ , c('sage_res', 'pj_res', 'montane_res', 'subalpine_res')], 
                       1, mean, na.rm=TRUE)



#### Read in the site data and the reanalysis data
snake = read.csv('./Site-Data/snake5.csv', as.is=1)
coordinates(snake) = ~x_utm+y_utm
proj4string(snake) = CRS('+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs')

ra = read.csv('./Reanalysis/Reanalysis-Data.csv')
ra$date = as.Date(ra$date)


#### Address the sensor issues and set data to NA appropriately
#### where data are missing and snow covered sensors exist

## Throw out data on D1P03 from when it fell over/off of the 2 m pole
m_tmx$D1P03[m_tmx$date >= as.Date('2014-04-22')] = NA
m_tmn$D1P03[m_tmn$date >= as.Date('2014-04-22')] = NA


g_tmx$D4P02[g_tmx$date >= as.Date('2013-10-11') &
            g_tmx$date <= as.Date('2014-05-28')] = NA
g_tmx$D4P03[g_tmx$date >= as.Date('2013-10-11') &
            g_tmx$date <= as.Date('2014-05-28')] = NA
g_tmx$D4P04[g_tmx$date >= as.Date('2013-10-11') &
            g_tmx$date <= as.Date('2014-05-28')] = NA

g_tmn$D4P02[g_tmn$date >= as.Date('2013-10-11') &
            g_tmn$date <= as.Date('2014-05-28')] = NA
g_tmn$D4P03[g_tmn$date >= as.Date('2013-10-11') &
            g_tmn$date <= as.Date('2014-05-28')] = NA
g_tmn$D4P04[g_tmn$date >= as.Date('2013-10-11') &
            g_tmn$date <= as.Date('2014-05-28')] = NA

if(melt_them){
  sites = names(m_tmx)[-1]
  
  m_tmx = melt(m_tmx, measure.vars=sites, variable_name='site')
  names(m_tmx)[3] = 'tmx'
  m_tmn = melt(m_tmn, measure.vars=sites, variable_name='site')
  names(m_tmn)[3] = 'tmn'  
  
  g_tmx = melt(g_tmx, measure.vars=sites, variable_name='site')
  names(g_tmx)[3] = 'tmx'
  g_tmn = melt(g_tmn, measure.vars=sites, variable_name='site')
  names(g_tmn)[3] = 'tmn'  
  
  d_tmx = melt(d_tmx, measure.vars=sites, variable_name='site')
  names(d_tmx)[3] = 'tmx'
  d_tmn = melt(d_tmn, measure.vars=sites, variable_name='site')
  names(d_tmn)[3] = 'tmn'   
  
  irrad = melt(irrad, measure.vars=sites, variable_name='site')
  names(irrad)[3] = 'irrad'
}

if(merge_them){
  ## Merge them with irradiance the computationally cheap way
  ## If m_tmx and irrad date and site columns are identical,
  ## assign the irradiance column
  
  # 2 m
  stopifnot(identical(m_tmx[,1:2], irrad[,1:2]))
  m_tmx$irrad = irrad$irrad
  stopifnot(identical(m_tmn[,1:2], irrad[,1:2]))
  m_tmn$irrad = irrad$irrad
  
  # ground
  stopifnot(identical(g_tmx[,1:2], irrad[,1:2]))
  g_tmx$irrad = irrad$irrad
  stopifnot(identical(g_tmn[,1:2], irrad[,1:2]))
  g_tmn$irrad = irrad$irrad
  
  # dif
  stopifnot(identical(d_tmx[,1:2], irrad[,1:2]))
  d_tmx$irrad = irrad$irrad
  stopifnot(identical(d_tmn[,1:2], irrad[,1:2]))
  d_tmn$irrad = irrad$irrad
  
  
  ## Merge with snake dataset the computationally
  ## expensive way
  m_tmx = merge(m_tmx, snake, by.x='site', by.y='ID')
  m_tmx = merge(m_tmx, wind, by='date')
  m_tmn = merge(m_tmn, snake, by.x='site', by.y='ID')
  m_tmn = merge(m_tmn, wind, by='date')
  
  g_tmx = merge(g_tmx, snake, by.x='site', by.y='ID')
  g_tmn = merge(g_tmn, snake, by.x='site', by.y='ID')
  
  d_tmx = merge(d_tmx, snake, by.x='site', by.y='ID')
  d_tmn = merge(d_tmn, snake, by.x='site', by.y='ID')
}

if(load_som){
  som = read.csv('./Reanalysis/SOM-Data.csv', header = TRUE)
  som$date = as.Date(som$date)
  if(merge_them) {
    m_tmx = merge(m_tmx, som, by='date')
    m_tmn = merge(m_tmn, som, by='date')
    
    g_tmx = merge(g_tmx, som, by='date')
    g_tmn = merge(g_tmn, som, by='date')
    
    d_tmx = merge(d_tmx, som, by='date')
    d_tmn = merge(d_tmn, som, by='date')    
  }
}

if(load_ra){
  ra = read.csv('./Reanalysis/Reanalysis-Data.csv', header = TRUE)
  ra$date = as.Date(ra$date)
  if(merge_them) {
    m_tmx = merge(m_tmx, ra, by='date')
    m_tmn = merge(m_tmn, ra, by='date')
    
    g_tmx = merge(g_tmx, ra, by='date')
    g_tmn = merge(g_tmn, ra, by='date')
    
    d_tmx = merge(d_tmx, ra, by='date')
    d_tmn = merge(d_tmn, ra, by='date')    
  }
}
if(load_eof){
  eof = read.csv('./Reanalysis/EOF_PCs-time-series.csv', header=TRUE)
  eof$date = as.Date(eof$date)
  if(merge_them){
    m_tmx = merge(m_tmx, eof, by='date')
    m_tmn = merge(m_tmn, eof, by='date')
    
    g_tmx = merge(g_tmx, eof, by='date')
    g_tmn = merge(g_tmn, eof, by='date')
    
    d_tmx = merge(d_tmx, eof, by='date')
    d_tmn = merge(d_tmn, eof, by='date')  
  }
}