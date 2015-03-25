#--------------------------------
# Name:         leave_one_out.R
# Purpose:      Perform a leave-one-out k-fold validation of tmn and tmx models
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

m_tmx$jday = as.numeric(format(m_tmx$date, '%j'))
m_tmx$jday_cos = cos(2*pi/365 * m_tmx$jday)
m_tmx$jday_sin = sin(2*pi/365 * m_tmx$jday)
m_tmx$elev = m_tmx$elev/1000 # convert to km
m_tmx$irrad = m_tmx$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmx$zone = as.factor(sapply(strsplit(as.character(m_tmx$site), split='P'), function(x) x[1]))

#### Prepare the GIS Data (level 2 of model) ####
jdays = as.numeric(format(unique(m_tmx$date), '%j'))
dates = unique(m_tmx$date)
temp = raster('./GIS-Data/NLCD/nlcd_utm.tif')
elev = crop(raster('./GIS-Data/DEM/merged_UTM11.tif'), temp)/1000
tci = crop(raster('./GIS-Data/DEM/TCI_Whole-Range_UTM.tif'), temp)
tri = crop(terrain(elev, opt='TRI'), temp)
gis.slope = crop(terrain(elev, opt='slope', unit='degrees'), temp)
cc_nlcd = resample(temp, tri)
irrad = stack('./GIS-Data/Irradiance/irrad_stack.grd')


#### Iterate through the sites. Each site will be left out of the model fitting,
#### the tmx model will be run to create predictions for the year, and
#### the difference between the observations and predictions will be the model bias.
#### This procedure is done n times where n = 40
n_list = levels(m_tmx$site)
for(n in n_list){
  print('Leave One Out')
  print(paste(' Fitting model: n = ', n, sep=''))
  ## pull out the nth site's data
  left_out = m_tmx[m_tmx$site == n, ]
  put_in   = m_tmx[m_tmx$site != n, ]
  
  if(predict){
    ## Fit the tmx model to the data that will be included or put_in the model i.e. != n
    ## na.action=na.omit is required on lme4 1.1-7
    tmx_mod = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                     cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                     ((elev+I(elev^2))|date)+(1|site), data=put_in, REML=FALSE, na.action=na.omit)
    
    #### Prepare the daily values (level 1 of model) ####
    tair_values = ra$tair
    PC4_values= eof$PC4
    
    #### Predict for tmx_mod
    pred_list = list()
    stopifnot(predict)
    for(i in 1:length(dates)){
      ## Print to screen and manage directories
      print(paste('   Predicting', ' n = ', n, ':  ', dates[i], sep=''))
      dir = file.path(getwd(), 'Temperature-Maps', 'Tmx', 'leave_one_out', n)
      fname = file.path(dir, paste0(dates[i], '_tmx.tif'))
      dir.create(dir, showWarnings=FALSE)
      
      ## Rasters of constant values for level 1 of the model
      jday = setValues(raster(cc_nlcd), jdays[i])
      tair = setValues(raster(cc_nlcd), tair_values[i])
      PC4 = setValues(raster(cc_nlcd), PC4_values[i])
      
      ## Create a stack of the raster layers with names matching tmx_mod
      s = stack(elev, tci, tri, gis.slope, cc_nlcd, irrad[[i]], 
                tair, PC4, jday)
      names(s) = c('elev', 'tci', 'tri', 'gis.slope', 'cc_nlcd', 
                   'irrad', 'tair', 'PC4', 'jday')
      
      ## Store the prediction in a list
      pred_list[[i]] = predict(object=s, model=tmx_mod, re.form=~0, filename=fname, overwrite=TRUE)
      
  #     r = predict(object=s, model=tmx_mod, re.form=~0, filename=fname, overwrite=TRUE)
      print(paste('     Wrote:', fname))
  }
#   pred_stack = stack(pred_list)
  
}
}

Sys.time() - then





#### ---------------------------------------------------------------------
#### the difference between the observations and predictions will be the model bias.
#### This procedure is done n times where n = 40
print('Leave One Out')
print(' Extracting predicted Tmx values...')
l_tmx = list()
for(n in n_list){
  print(paste('  ', n))
  ## pull out the nth site's data
  left_out = m_tmx[m_tmx$site == n, ]
  put_in   = m_tmx[m_tmx$site != n, ]
  
  ## Set up the directories and read all GeoTiff files in the directory
  dir = file.path(getwd(), 'Temperature-Maps', 'Tmx', 'leave_one_out', n)
  lf = list.files(dir, full.names=TRUE, pattern='.tif$')
  ## create a raster stack of all the tmx predictions
  tmx_stack = stack(lf)
  ## Make the left_out data a SpatialPointsDataFrame
  coordinates(left_out) = ~x_utm+y_utm
  proj4string(left_out) = CRS(projection(tmx_stack))
  e = extract(tmx_stack, left_out[1, ])
  
  ## Store the extracted predictions with site n left out in a list
  l_tmx[[length(l_tmx)+1]] = as.numeric(e)
  # }
}
## Assign site names to the list
names(l_tmx) = n_list
save(l_tmx, file='./Temperature-Maps/Tmx/leave_one_out/l_tmx.rData')

# 
# #### Prepare the GIS Data (level 2 of model) ####
# jdays = as.numeric(format(unique(m_tmx$date), '%j'))
# dates = unique(m_tmx$date)
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
