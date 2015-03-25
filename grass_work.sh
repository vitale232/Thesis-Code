#!/usr/bin/env bash

#### Change the working directory
cd /home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/DEM

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
r.out.gdal --verbose input=elevation_raster.tci format=GTiff \
     output=TCI_Whole-Range_UTM.tif

pdir=$(pwd)
echo
echo "OUTPUT: $pdir/TCI_Whole-Range_UTM.tif"
echo

#### Make a png map
GRASS_PNGFILE=tci.png
export GRASS_PNGFILE

d.mon start=PNG

d.rast elevation_raster.tci
d.mon stop=PNG

echo
echo "OUTPUT: $pdir/tci.png"
echo
