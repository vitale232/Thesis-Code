#!/usr/bin/env bash

####### Copy and paste this goodness into the Bash shell

#### Change the working directory
cd ~/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/DEM

#### Merge the 4 tiles that the Snake Range crosses
gdal_merge.py -o merged_dem.tif  imgn39w114_1.img imgn39w115_1.img imgn40w114_1.img imgn40w115_1.img

#### Clip and reproject
gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs' -te 715826.8 4288532 754724.2 4331252 merged_dem.tif merged_UTM11.tif


#### Boot up grass gis
grass

#### Will need some GUI input
#### Set up the location and mapset as needed

#### Read in the raster using gdal
r.in.gdal input=merged_UTM11.tif output=elevation_raster

#### set the project's region to the location of the dem
#### and print out the info to the shell
g.region rast=elevation_raster -p

#### Run the r.terraflow algorithm to obtain
#### TCI for the entire southern Snake Range
r.terraflow elev=elevation_raster filled=elevation_raster.filled dir=elevation_raster.dir \
     swatershed=elevation_raster.swatershed accumulation=elevation_raster.accumulation \
     tci=elevation_raster.tci memory=2048

#### Write out TCI
r.out.gdal input=elevation_raster.tci format=GTiff \
      output=/home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/TCI_Whole-Range_UTM.tif
