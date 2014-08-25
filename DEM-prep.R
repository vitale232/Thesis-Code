#--------------------------------
# Name:         DEM_Prep.R
# Purpose:      Merge, crop, reproject the DEM's for the Snake Range
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/07/13
# R:            3.1.1
#--------------------------------

#### NOTE:
#### There's an equivalent script that runs much quicker
#### on Linux that uses the GDAL command line tools
#### GDAL_DEM-prep.bat 

library(raster)
library(rgdal)

#### Read in the files
setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

lf <- list.files('./GIS-Data/DEM/', pattern = '.img$',
                 full.names = TRUE)

r1 <- raster(lf[1])
r2 <- raster(lf[2])
r3 <- raster(lf[3])
r4 <- raster(lf[4])

#### Merge the rasters
r <- merge(r1, r2, r3, r4)
plot(r)

#### Project the raster to UTM
pr <- projectRaster(r, crs = '+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs')

#### Set up the extent, crop, write out
e <- extent(c(715826.8, 754724.2, 4288532, 4331252))

cr <- crop(pr, e, './GIS-Data/DEM/All-Range_UTM11.tif')
