#--------------------------------
# Name:         process_daymet.R
# Purpose:      Calculate December avg Tmn from Daymet
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/04/01
# R:            3.1.2
#--------------------------------

library(ncdf4)
library(raster)
library(rgdal)

## Read in the Daymet tmn netcdf
daymet_tmn = stack('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Daymet/tmin.nc')

## get the dates from the raster layer names and convert to R date format
z = names(daymet_tmn)
dates = as.Date(substring(z, 2, 11), format='%Y.%m.%d')

## get the indices of the layers that include dec and subset the whole stack with these layers
dec = which(format(dates, '%b') == 'Dec')
daymet_tmn_dec = daymet_tmn[[dec]]

mean_tmn = mean(daymet_tmn_dec)

dec_files = list.files('~/Dropbox/UNR/UNR-Thesis/Data/Temperature-Maps/Tmn/tmn_mod/', 
                       pattern='201[3-4]-12-[0-9][0-9]_tmn.tif$',
                       full.names=TRUE)
dec_stack = stack(dec_files)
dec = mean(dec_stack)

## reproject daymet to UTM
mean_tmn = projectRaster(mean_tmn, crs=projection(dec))

writeRaster(dec, '/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Daymet/predicted_mean-dec-tmn.tif',
            overwrite=TRUE)
writeRaster(mean_tmn, '/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Daymet/daymet_mean-dec-tmn.tif',
            overwrite=TRUE)
