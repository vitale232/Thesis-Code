"""Creates new folder in the workpsace called Quick-Looks,
Reads in the tiff files, uses matplotlib to create PNGs of the rasters"""

import gdal
import os

import matplotlib.pyplot as plt
import numpy as np

def main(workspace):
    names = [item.split('.tif')[0] for item in os.listdir(workspace) if item.endswith('.tif')]
    png_folder = os.path.join(workspace, 'Quick-Looks')
    if not os.path.exists(png_folder):
        os.mkdir(png_folder)

    for item in names:
        raster_filename = os.path.join(workspace, str(item + '.tif'))
        print('Opening:\n {0}'.format(raster_filename))
        png_filename = os.path.join(png_folder, str(item + '.png'))
        title = item

        layer = gdal.Open(raster_filename)
        band = layer.GetRasterBand(1)
        array = band.ReadAsArray()
        array[array == array[0]] = np.nan

        plt.imshow(array, cmap='hot')
        plt.colorbar(orientation='horizontal', cmap='hot')
        frame = plt.gca()
        frame.axes.xaxis.set_ticklabels([])
        frame.axes.yaxis.set_ticklabels([])
        plt.suptitle(title)
        plt.savefig(png_filename, bbox_inches='tight')
        print(' Wrote:\n  {0}\n'.format(png_filename))
        plt.close('all')

    print('\n\nFINISHED!')

if __name__ == '__main__':
    main('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Temperature-Maps/Tmx/tmx_mod')