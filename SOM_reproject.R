#--------------------------------
# Name:         SOM_reproject.R
# Purpose:      Prep Reanalysis SLP data for SOM
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/09/24
# R:            3.1.1
#--------------------------------

library(raster)
library(rgdal)
library(fields)
library(ncdf4)
library(kohonen)

plot_the_maps = TRUE


setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

#### Set up the extent I want to work in
e = extent(-180, -75, 15, 90)

# #### Read in the file list
# slp_files = list.files('./Reanalysis/SLP/', pattern='.nc$', full.names=TRUE)
# 
# #### Correct for the time issues.  Origin of the time files
# #### is '1-1-1 00:00:00', but there is a 48 hour error in R's
# #### interpretation of the times.  This error is due to the origin
# #### occuring prior to the Gregorian Calendar change.
# time = NULL
# for(file in slp_files){
#   time = c(time, ncvar_get(nc_open(file), varid='time'))
# }
# time = (time - 48) * 3600 # convert to seconds for R
# time = as.POSIXct(time, origin='1-1-1 00:00:00', tz='GMT')
# 
# #### Read in the NetCDF files as a raster stack
# slp = stack(slp_files)
# names(slp) = paste0('x', time)
# slp = setZ(slp, time)
# 
# #### Rotate to -180, 180 longitude
# slp = rotate(slp)
# 
# #### Crop to the extent of interest
# slp = crop(slp, e)
# 
# #### Reproject the data
# slp = projectRaster(slp, crs='+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
# e2 = extent(-5016183, -127126.6, -1294725, 3178209)
# slp = crop(slp, e2)
# 
# #### Convert to hPa
# slp = slp/100
# 
# #### Calculate the anomolies using the entire period as baseline
# climo = mean(slp)
# anoms = slp - climo
# anoms = setZ(anoms, time)
# names(anoms) = paste0('x', time)


#-----------------------------------------------------------------------------
#### Load in the anoms that were calculated using the code above
anoms = brick('./Reanalysis/SLP_Anoms/slp_anoms.grd')

#### Set up an empty raster from the anoms metadata to capture cell size etc.
r = raster(anoms)

#### Create a matrix of the anomolies
m = t(as.matrix(anoms))

#### Run the SOM on the SLP anomoly data
set.seed(67728)
slp_som = som(m, grid=somgrid(xdim=4, ydim=5, topo='rectangular'), 
              n.hood='square', keep.data=FALSE)

#### Extract the code values and create a raster stack
l = list()
for(i in 1:nrow(slp_som$codes)){
  l[[i]] = setValues(r, slp_som$codes[i, ])
}
codes = stack(l)

#### If plot_the_maps flag is on, plot the maps using the Plot_SOM_Rasters script
if(plot_the_maps){
  source('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/Plot_SOM_Rasters.R')
}

#### Plot the training process
x11()
plot(slp_som, type='changes', 'SLP SOM Training Process', 
     cex.axis=1.2, cex.lab=1.2, lwd=1.5)


#### Subset the anoms for the dates of my time series
start_date = as.Date('2013-06-17')
end_date = as.Date('2014-06-24')

dates = as.Date(getZ(anoms))
which_layers = which(dates >= start_date & dates <= end_date)

these_anoms = anoms[[which_layers]]

#### coerce to matrix and map to slp_som
map_m = t(as.matrix(these_anoms))

mapped = map.kohonen(slp_som, map_m)

dates_mapped = data.frame('date'=dates[which_layers],
                          'unit'=mapped$unit.classif,
                          'dist'=mapped$distances)

