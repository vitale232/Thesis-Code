#--------------------------------
# Name:         SOM.R
# Purpose:      Prep Reanalysis SLP data for SOM
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/09/24
# R:            3.1.1
#--------------------------------
system.time({
library(raster)
library(ncdf4)
library(kohonen)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

#### Set up the extent I want to work in
e = extent(-145, -100, 25, 60)

#### Read in the file list
slp_files = list.files('./Reanalysis/SLP/', pattern='.nc$', full.names=TRUE)

#### Correct for the time issues.  Origin of the time files
#### is '1-1-1 00:00:00', but there is a 48 hour error in R's
#### interpretation of the times.  This error is due to the origin
#### occuring prior to the Gregorian Calendar change.
time = NULL
for(file in slp_files){
  time = c(time, ncvar_get(nc_open(file), varid='time'))
}
time = (time - 48) * 3600 # convert to seconds for R
time = as.POSIXct(time, origin='1-1-1 00:00:00', tz='GMT')

#### Read in the NetCDF files as a raster stack
slp = stack(slp_files)
names(slp) = paste0('x', time)
slp = setZ(slp, time)

#### Rotate to -180, 180 longitude
slp = rotate(slp)

#### Crop to the extent of interest
slp = crop(slp, e)
#### Convert to hPa
slp = slp/100

#### Calculate the anomolies using the entire period as baseline
climo = mean(slp)
anoms = slp - climo
anoms = setZ(anoms, time)

#### Set up an empty raster from the anoms metadata to capture cell size etc.
r = raster(anoms)

#### Create a matrix of the anomolies
m = t(as.matrix(anoms))

#### Run the SOM on the SLP anomoly data
set.seed(67728)
slp_som = som(m, grid=somgrid(xdim=7, ydim=5, topo='rectangular'), 
              n.hood='square', keep.data=FALSE)

#### Extract the code values and create a raster stack
l = list()
for(i in 1:nrow(slp_som$codes)){
  l[[i]] = setValues(r, slp_som$codes[i, ])
}
codes = stack(l)

})
