import gdal
import matplotlib.pyplot as plt
import os

def list_files(dir):
    out = []
    for item in os.listdir(dir):
        if item.endswith('.nc'):
            out.append(item)
    return(out)

def open_slp_as_array(slp_file):
    print 'NETCDF:"{0}":slp'.format(slp_file)
    ds = gdal.Open('NETCDF:"{0}":slp'.format(slp_file))
    band = ds.GetRasterBand(1)
    print(ds.RasterCount)
    meta = ds.GetMetadata()
    add_offset = meta['slp#add_offset']
    slp = band.ReadAsArray()
    slp = slp + int(add_offset)
    ds = None
    del ds
    return slp

def plot_array(array):
    fig = plt.figure()
    plt.imshow(array)
    plt.colorbar()
    plt.show()

def plot_4x4(array1, array2, array3, array4):
    fig, axes = plt.subplots(2, 3, figsize=(10, 10))
    fig.subplots_adjust(hspace=0.1, wspace=0.05)
    array_list = [array1, array2, array3, array4]
    ax.imshow(array1)
    ax.set_title('array 1')
    ax.imshow(array2)
    ax.set_title('array 2')
    ax.imshow(array3)
    ax.set_title('array 3')
    ax.imshow(array4)
    ax.set_title('array 4')
    plt.show()

if __name__ == '__main__':
    os.chdir('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/SLP')
    file_list = sorted(list_files(os.getcwd()))
    print file_list
    slp = open_slp_as_array(file_list[20])
    print slp
    plot_array(slp)