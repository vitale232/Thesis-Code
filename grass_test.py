import grass.script as grass
import grass.script.array as garray
import os

def main():

    rast1 = '/home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/DEM/n39w114/imgn39w114_1.img'    
    rast2 = '/home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/DEM/n39w115/imgn39w115_1.img'
    rast3 = '/home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/DEM/n40w114/imgn40w114_1.img'
    rast4 = '/home/vitale232/Google\ Drive/UNR/UNR-Thesis/Data/GIS-Data/DEM/n40w115/imgn40w115_1.img'


    a = garray.array()
    a.read(rast1)

    print(grass.raster_info(rast1))

if __name__== '__main__':
    main()


