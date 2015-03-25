#--------------------------------
# Name:         leave-one-out_tmn.R
# Purpose:      Perform a leave-one-out k-fold validation of tmn and tmn models
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2015/02/02
# R:            3.1.2
#--------------------------------
library(raster)
library(lme4)
library(ggplot2)
library(devtools)

setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

then = Sys.time() # right now is then
predict = FALSE

melt_them = TRUE
merge_them = TRUE
load_som = TRUE
load_ra = TRUE
load_eof = TRUE
source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

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
# 
# tmn_mod = lmer(formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+
#                          cos(2*pi/365*jday)+sin(2*pi/365*jday)+(1|site)+((elev+I(elev^2))|date)),
#                data=m_tmn, REML=FALSE, na.action=na.omit)

#### Prepare the GIS Data (level 2 of model) ####
jdays = as.numeric(format(unique(m_tmn$date), '%j'))
dates = unique(m_tmn$date)
temp = raster('./GIS-Data/NLCD/nlcd_utm.tif')
elev = crop(raster('./GIS-Data/DEM/merged_UTM11.tif'), temp)/1000
tci = crop(raster('./GIS-Data/DEM/TCI_Whole-Range_UTM.tif'), temp)
tri = crop(terrain(elev, opt='TRI'), temp)
gis.slope = crop(terrain(elev, opt='slope', unit='degrees'), temp)
cc_nlcd = resample(temp, tri)
irrad = stack('./GIS-Data/Irradiance/irrad_stack.grd')

#### Prepare the daily values (level 1 of model) ####
tair_values = ra$tair
PC4_values= eof$PC4


#### Iterate through the sites. Each site will be left out of the model fitting,
#### the tmn model will be run to create predictions for the year, and
#### the difference between the observations and predictions will be the model bias.
#### This procedure is done n times where n = 40
n_list = levels(m_tmn$site)
for(n in n_list){
  print('Leave One Out')
  print(paste(' Fitting model: n = ', n, sep=''))
  ## pull out the nth site's data
  left_out = m_tmn[m_tmn$site == n, ]
  put_in   = m_tmn[m_tmn$site != n, ]
  
  if(predict){
    ## Fit the tmn model to the data that will be included or put_in the model i.e. != n
    ## na.action=na.omit is required on lme4 1.1-7
    tmn_mod = lmer(tmn~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                     cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                     ((elev+I(elev^2))|date)+(1|site), data=put_in, REML=FALSE, na.action=na.omit)
    
    #### Prepare the daily values (level 1 of model) ####
    tair_values = ra$tair
    PC4_values= eof$PC4
    #### Predict for tmn_mod
    pred_list = list()
    stopifnot(predict)
    for(i in 1:length(dates)){
      ## Print to screen and manage directories
      print(paste('   Predicting', ' n = ', n, ':  ', dates[i], sep=''))
      dir = file.path(getwd(), 'Temperature-Maps', 'Tmn', 'leave_one_out', n)
      fname = file.path(dir, paste0(dates[i], '_tmn.tif'))
      dir.create(dir, showWarnings=FALSE)
      
      ## Rasters of constant values for level 1 of the model
      jday = setValues(raster(cc_nlcd), jdays[i])
      tair = setValues(raster(cc_nlcd), tair_values[i])
      PC4 = setValues(raster(cc_nlcd), PC4_values[i])
      
      ## Create a stack of the raster layers with names matching tmn_mod
      s = stack(elev, tci, tri, gis.slope, cc_nlcd, 
                irrad[[i]], tair, PC4, jday)
      names(s) = c('elev', 'tci', 'tri', 'gis.slope', 'cc_nlcd', 
                   'irrad', 'tair', 'PC4', 'jday')
      
      ## Store the prediction in a list
      pred_list[[i]] = predict(object=s, model=tmn_mod, re.form=~0, 
                               filename=fname, overwrite=TRUE)
      

      #     r = predict(object=s, model=tmn_mod, re.form=~0, filename=fname, overwrite=TRUE)
      print(paste('     Wrote:', fname))
    }
    ## Make the prediction a raster stack and extract the predictions for n
    pred_stack = stack(pred_list)
    
    #   pred_stack = stack(pred_list)
  }
}

Sys.time() - then





#### ---------------------------------------------------------------------
#### the difference between the observations and predictions will be the model bias.
#### This procedure is done n times where n = 40
print('Leave One Out')
print(' Extracting predicted Tmn values...')
l_tmn = list()
for(n in n_list){
  print(paste('  ', n))
  ## pull out the nth site's data
  left_out = m_tmn[m_tmn$site == n, ]
  put_in   = m_tmn[m_tmn$site != n, ]
  
  ## Set up the directories and read all GeoTiff files in the directory
  dir = file.path(getwd(), 'Temperature-Maps', 'Tmn', 'leave_one_out', n)
  lf = list.files(dir, full.names=TRUE, pattern='.tif$')
  ## create a raster stack of all the tmn predictions
  tmn_stack = stack(lf)
  
  ## Make the left_out data a SpatialPointsDataFrame
  coordinates(left_out) = ~x_utm+y_utm
  proj4string(left_out) = CRS(projection(tmn_stack))
  e = extract(tmn_stack, left_out[1, ])
  
  ## Store the extracted predictions with site n left out in a list
  l_tmn[[length(l_tmn)+1]] = as.numeric(e)
  # }
}
## Assign site names to the list
names(l_tmn) = n_list
save(l_tmn, file='./Temperature-Maps/Tmn/leave_one_out/l_tmn.rData')


# 
# #### Prepare the GIS Data (level 2 of model) ####
# jdays = as.numeric(format(unique(m_tmn$date), '%j'))
# dates = unique(m_tmn$date)
# temp = raster('./GIS-Data/NLCD/nlcd_utm.tif')
# elev = crop(raster('./GIS-Data/DEM/merged_UTM11.tif'), temp)/1000
# tci = crop(raster('./GIS-Data/DEM/TCI_Whole-Range_UTM.tif'), temp)
# tri = crop(terrain(elev, opt='TRI'), temp)
# gis.slope = crop(terrain(elev, opt='slope', unit='degrees'), temp)
# cc_nlcd = resample(temp, tri)
# irrad = stack('./GIS-Data/Irradiance/irrad_stack.grd')
# #### Resampled irrad and now load it from disk ####
# # irrad = stack(list.files('./GIS-Data/Irradiance/new_name/', 
# #                          pattern='.tif$', full.names=TRUE)[jdays])
# # irrad = resample(irrad, cc_nlcd)
# # irrad = irrad/1000
# # irrad = writeRaster(irrad, './GIS-Data/Irradiance/irrad_stack.grd', overwrite=TRUE)
# 
# #### Prepare the daily values (level 1 of model) ####
# tair_values = ra$tair
# PC4_values= eof$PC4
