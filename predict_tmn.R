#--------------------------------
# Name:         predict_tmn.R
# Purpose:      Scale up the best minimum temperature model to the landscape
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/11/13
# R:            3.1.1
#--------------------------------

library(raster)
library(nlme)
library(lme4)
library(ggplot2)
library(visreg)
library(psych)
library(spdep)
library(devtools)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

melt_them = TRUE
merge_them = TRUE
load_som = TRUE
load_ra = TRUE
load_eof = TRUE
source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

m_tmn$jday = as.numeric(format(m_tmn$date, '%j'))
m_tmn$jday_cos = cos(2*pi/365 * m_tmn$jday)
m_tmn$jday_sin = sin(2*pi/365 * m_tmn$jday)
m_tmn$elev = m_tmn$elev/1000 # convert to km
m_tmn$irrad = m_tmn$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmn$zone = as.factor(sapply(strsplit(as.character(m_tmn$site), split='P'), function(x) x[1]))

#### Fit the model ####
## tmn_mod is the final model, as displayed in the hierarchical-models_tmn.R script
## It includes crossed random effects terms for date and site nested within zone, both random intercepts.
## The model also includes a random slope for elevation and its quadratic,
## which is justified by the persistant cold air drainage at the study site.

tmn_mod = lmer(formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+
                       cos(2*pi/365*jday)+sin(2*pi/365*jday)+(1|site)+((elev+I(elev^2))|date)),
               data=m_tmn, REML=FALSE, na.action=na.omit)

#### Prepare the GIS Data (level 2 of model) ####
jdays = as.numeric(format(unique(m_tmn$date), '%j'))
dates = unique(m_tmn$date)
temp = raster('./GIS-Data/NLCD/nlcd_utm.tif')
elev = crop(raster('./GIS-Data/DEM/merged_UTM11.tif'), temp)/1000
tci = crop(raster('./GIS-Data/DEM/TCI_Whole-Range_UTM.tif'), temp)
tri = crop(terrain(elev, opt='TRI'), temp)
gis.slope = crop(terrain(elev*1000, opt='slope', unit='degrees'), temp)
cc_nlcd = resample(temp, tri)
irrad = stack('./GIS-Data/Irradiance/irrad_stack.grd')
## Resampled irrad and now load it from disk
# irrad = stack(list.files('./GIS-Data/Irradiance/new_name/', 
#                          pattern='.tif$', full.names=TRUE)[jdays])
# irrad = resample(irrad, cc_nlcd)
# irrad = irrad/1000
# irrad = writeRaster(irrad, './GIS-Data/Irradiance/irrad_stack.grd', overwrite=TRUE)

#### Prepare the daily values (level 1 of model) ####
tair_values = ra$tair
PC4_values= eof$PC4

#### Predict for tmn_mod
pred_list = list()
for(i in 1:length(dates)){
  print(paste('Starting prediction for', dates[i]))
  fname = file.path(getwd(), 'Temperature-Maps', 'Tmn', 'tmn_mod', paste0(dates[i], '_tmn.tif'))
  jday = setValues(raster(cc_nlcd), jdays[i])
  tair = setValues(raster(cc_nlcd), tair_values[i])
  PC4 = setValues(raster(cc_nlcd), PC4_values[i])
  
  s = stack(elev, tci, tri, gis.slope, cc_nlcd, irrad[[i]], tair, PC4, jday)
  names(s) = c('elev', 'tci', 'tri', 'gis.slope', 'cc_nlcd', 'irrad', 'tair', 'PC4', 'jday')
  
  pred_list[[i]] = predict(object=s, model=tmn_mod, re.form=~0, 
                           filename=fname, overwrite=TRUE)
  print(paste(' Wrote:', fname))
}
pred_stack = stack(pred_list)

# #### predict for cross17
# pred_list_cross17 = list()
# for(i in 1:length(dates)){
#   print(paste('Starting prediction for', dates[i]))
#   fname = file.path(getwd(), 'Temperature-Maps', 'Tmn', 'cross17', paste0(dates[i], '_tmn.tif'))
#   jday = setValues(raster(cc_nlcd), jdays[i])
#   tair = setValues(raster(cc_nlcd), tair_values[i])
#   PC4 = setValues(raster(cc_nlcd), PC4_values[i])
#   
#   s = stack(elev, tci, tri, gis.slope, cc_nlcd, irrad[[i]], tair, PC4, jday)
#   names(s) = c('elev', 'tci', 'tri', 'gis.slope', 'cc_nlcd', 'irrad', 'tair', 'PC4', 'jday')
#   
#   pred_list_cross17[[i]] = predict(object=s, model=cross17, re.form=~0, filename=fname)
#   print(paste(' Wrote:', fname))
# }
# pred_stack = stack(pred_list_cross17)

#### Try to predict with random effect - this does not seem to work
#### as when a day has a strong elevation effect on slope, it ends
#### up looking like a DEM with nonsensical values
# level1 = ranef(cross15)$date
# level1$date = as.Date(row.names(level1))
# names(level1) = c('intercept', 'elev', 'elev2', 'date')
# ranef_list = list()
# for(i in 1:length(dates)){
#   print(paste('Applying random effects for', dates[i]))
#   fname2 = file.path(getwd(), 'Temperature-Maps', 'Tmn', 'cross14_re', paste0(dates[i], '_tmn_re.tif'))
#   e  = s[['elev']] * level1$elev[i]
#   e2 = s[['elev']] * level1$elev2[i]
#   ranef_list[[i]] = pred_stack[[i]] + (level1$intercept[i] + e + e2)
#   writeRaster(ranef_list[[i]], filename=fname2)
# }
# 
# ran_stack = stack(ranef_list)
