#--------------------------------
# Name:         Reanalysis_Extract.py
# Purpose:      Extract the data for the Snake Range from
#               the NCEP Reanalysis 1 dataset
# Author:       Andrew Vitale
# Email:        vitale232@gmail.com
# Created       2014-04-23
# Python:       2.7
#--------------------------------

import os, sys, re
from osgeo import gdal
import matplotlib.pyplot as plt

def extract_from_nc(workspace=os.getcwd()):
    ### Register all of the drivers and set up paths
    item = 'hgt.2013.nc'
    path = os.path.join(workspace, item)
    gdal.AllRegister()

    ### Print the purpose
    print("\nExtracting Values from {0}".format(item))
    print("File located at {0}\n".format(path))

    ### Open the netcdf
    nc_file = gdal.Open(path)

    if nc_file is None:
        sys.exit('ERROR: Something went wrong opening the file.  Check file and paths')

    ### Get the projection, geotransform and metadata
    proj = nc_file.GetProjection()
    gt = nc_file.GetGeoTransform()
    meta = nc_file.GetMetadata()

    ### Print the goods
    print('{0:<16}: {1}'.format('PROJECTION', proj))
    print('{0:<16}: {1}'.format('GEOTRANFORM', gt))
    print('{0:<16}: {1}'.format('METADATA', list(meta)[5]))

    sd = nc_file.GetSubDatasets()
    print(sd)
    for thing in sd:
        print thing

    print(nc_file.RasterCount)




if __name__ == '__main__':
    extract_from_nc()