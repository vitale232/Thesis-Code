#!/usr/bin/env bash
#--------------------------------
# Name:         do_grass.sh
# Purpose:      Executable script to merge, reproject, and clip USGS DEM's,
#               launch GRASS GIS 6.4, run r.terraflow, and output TCI. 
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/07/13
# R:            3.1.1
#data.table:    1.9.2
#--------------------------------

#### Change the working directory
cd /home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/DEM

#####################################################
# Run gdal_merge.py to merge the 4 DEM's
#####################################################
echo 'Starting gdal_merge.py...'
gdal_merge.py -o merged_dem.tif  imgn39w114_1.img imgn39w115_1.img imgn40w114_1.img imgn40w115_1.img
echo 'SUCCESS: gdal_merge.py!'
echo

#####################################################
# Run gdalwarp to reproject and clip the merged image
#####################################################
echo 'Starting gdalwarp...'
gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs' \
         -te 715826.8 4288532 754724.2 4331252 merged_dem.tif merged_UTM11.tif
echo 'SUCCESS: gdalwarp!'
echo

#####################################################
# Setup environment for grass
#####################################################
export GISBASE=/usr/local/grass64
export PATH=$PATH:$GISBASE/bin:$GISBASE/scripts
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GISBASE/lib

# use process ID (PID) as lock file number:
export GIS_LOCK=$$

# Incase you need grass add-ons
GRASS_BASEDIR='/usr/lib/grass64'
GRASS_ADDON_PATH='$GRASS_BASEDIR/addons'
export GRASS_ADDON_PATH

GRASS_HTML_BROWSER=false
export GRASS_HTML_BROWSER

# Set up the location/mapset/etc...
export LOCATION='/home/vitale232/Documents/GrassGISdata/SnakeRange/PERMANENT'
export GISDBASE='/home/vitale232/Documents/GrassGISdata'
export LOCATION_NAME='SnakeRange'
export MAPSET='PERMANENT'

# Export the job as an environment variable
GRASS_BATCH_JOB=/home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/Scripts/grass_work.sh
export GRASS_BATCH_JOB

#####################################################
# Call the grass executable
#####################################################
grass64 - /home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/Scripts/grass_work.sh
unset GRASS_BATCH_JOB

echo
echo "SUCCESS: do_grass.sh!"
exit
