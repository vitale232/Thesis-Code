#--------------------------------
# Name:         extract_canopy-cover.R
# Purpose:      Quick script to add canopy cover to snake2.csv
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/10/07
# R:            3.1.1
#--------------------------------

library(raster)

melt_them = FALSE
merge_them = FALSE
load_som = FALSE
load_ra = FALSE
source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

nlcd = stack('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/GIS-Data/NLCD/nlcd_utm.tif')
elev= raster('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/GIS-Data/DEM/merged_UTM11.tif')

par(mfrow=c(2,1))
plot(nlcd, 1, main='% CC')
plot(snake, add=TRUE)
plot(nlcd, 2, main='Std Error')
plot(snake, add=TRUE)

e = extract(nlcd, snake)

snake$cc_nlcd = e[,1]
snake$cc_nlcd_std_error = e[,2]

e = extract(elev, snake)
snake$elev = e

write.csv(snake, '~/Dropbox/UNR/UNR-Thesis/Data/Site-Data/snake3.csv', row.names=FALSE)
