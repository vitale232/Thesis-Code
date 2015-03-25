"""
Calculating EOFs and their associated PCs for NCEP Reanalysis SLP
data spanning the period 1958-01-01 to 2014-90-21.  This script was
made to compare results with the R function spacetime:::eof.
"""

import numpy as np
import matplotlib.pyplot as plt
import os
from netCDF4 import Dataset
from eofs.standard import Eof
from eofs.examples import example_data_path
from mpl_toolkits.basemap import Basemap
import datetime as dt

def movingaverage(interval, window_size):
    window = np.ones(int(window_size))/float(window_size)
    return np.convolve(interval, window, 'same')

def main(mFilepath, xFilepath, yFilepath, window, windowFlag=True):
    ## load in the data matrix as a numpy array
    m = np.loadtxt(mFilepath, dtype='float', delimiter=',', skiprows=1)
    # lon = np.loadtxt(xFilepath, dtype='float', delimiter=',', skiprows=1)
    # lat = np.loadtxt(yFilepath, dtype='float', delimiter=',', skiprows=1)
    # time =  np.arange('1958-01-01', '2014-09-22', dtype='datetime64')
    # years = range(1958, 2014)
    ## Create a list of dates spanning the study period
    base = dt.datetime(2014, 9, 21, 1, 1, 1, 1)
    dates = [base - dt.timedelta(days=x) for x in range(0, 20718)]
    date_list = [item for item in reversed(dates)]


    ## attempted to read in the raw data, but was struggling with
    ## the array dimensions
    # ncFiles = os.listdir(workspace)
    # slpList, lonList, latList, timeList = [], [], [], []
    # for fileIn in ncFiles:
    #     ncIn = Dataset(os.path.join(workspace, fileIn), 'r')
    #     slpList.append(ncIn.variables['slp'][:]/100)
    #     lonList.append(ncIn.variables['lon'][:])
    #     latList.append(ncIn.variables['lat'][:])
    #     timeList.append(ncIn.variables['time'][:])
    #     ncIn.close()

    # slp = np.array(slpList)
    # print(slp)
    # print(slp.shape)
    # # print(slp)
    # # print(np.shape(slp))

    ## create an EOF solver object and extrac the first
    ## 4 EOFs and their associated PCs. Scaling can be 
    ## applied if desired
    ## http://ajdawson.github.io/eofs/api/eofs.standard.html#eofs.standard.Eof
    solver = Eof(m)
    eofs = solver.eofs(neofs=4, eofscaling=0)
    pcs = solver.pcs(npcs=4, pcscaling=0)

    # lon, lat = np.meshgrid(lon, lat)

    ## plot the EOFs as nongeographic data for simplicity
    fig = plt.figure(figsize=(10, 10))
    for i in range(4):
        ax = fig.add_subplot(2, 2, i+1)
        lab = 'EOF' + str(i + 1)
        main =  'Unscaled ' + lab

        eofPlot = eofs[i,].reshape(17, 32)

        plt.imshow(eofPlot, cmap=plt.cm.RdBu_r)
        plt.title(main)
        cb = plt.colorbar(orientation='horizontal', cmap=plt.cm.RdBu_r)
        cb.set_label(lab, fontsize=12)

        ## Basemap failure below.  Something with the y cell size went wrong
        # bm = Basemap(projection='cyl', llcrnrlat=16.17951, urcrnrlat=68.48459,
        #              llcrnrlon=-176.0393, urcrnrlon=-98.07901, resolution='c')

        # # bm.contourf(x, y, eof1.squeeze(), clevs, cmap=plt.cm.RdBu_r)
        # bm.drawcoastlines()
        # bm.drawstates()
        # im = bm.pcolormesh(lon, lat, eofPlot, cmap=plt.cm.RdBu_r, latlon=True)
        # # bm.fillcontinents(color='coral', lake_color='aqua')
        # bm.drawparallels(np.arange(-90.,91.,15.))
        # bm.drawmeridians(np.arange(-180.,181.,30.))
        # # bm.drawmapboundary(fill_color='aqua')
        # cb = plt.colorbar(orientation='horizontal')
        # cb.set_label(lab, fontsize=12)
        # plt.title(main, fontsize=16)
        # plt.show()
    plt.show()
    ## Plot the PCs as a time series
    fig = plt.figure(figsize=(16, 16))
    for i in range(4):
        ylab = 'PC' + str(i+1)
        title = ylab + ' Time Series'

        pcPlot = pcs[:,i]
        if i==0:
            theAx = fig.add_subplot(4, 1, i+1)
            plt.setp(theAx.get_xticklabels(), visible=False)
            theAx.set_xlabel('')
        if i>0 and i<3:
            ax = fig.add_subplot(4, 1, i+1, sharex=theAx)
            plt.setp(ax.get_xticklabels(), visible=False)
        if i==3:
            ax = fig.add_subplot(4, 1, i+1, sharex=theAx)
            plt.xlabel('Date')

        plt.plot(date_list, pcPlot, color='b')
        if windowFlag:
            plt.plot(date_list, movingaverage(pcPlot, window), 
                     color='r', linestyle='-')
        plt.axhline(0, color='k')
        plt.title(title)
        plt.ylabel(ylab)
    plt.show()

    ## Subset the dates to the last year of the dataset
    short_date = [item for item in date_list if 
                  item >= dt.datetime(2013, 6, 17) 
                  and item < dt.datetime(2014, 6, 25)]
    indices = [date_list.index(item) for item in short_date]

    fig = plt.figure(figsize=(16, 16))
    ## Plot out the last year of the PCs to get a more detailed
    ## pattern for comparison to the R results
    for i in range(4):
        ylab = 'PC' + str(i+1)
        title = ylab + ' Time Series (1 year)'

        pcPlot = pcs[np.array(indices),i]
        if i==0:
            theAx = fig.add_subplot(4, 1, i+1)
            plt.setp(theAx.get_xticklabels(), visible=False)
            theAx.set_xlabel('')
        if i>0 and i<3:
            ax = fig.add_subplot(4, 1, i+1, sharex=theAx)
            plt.setp(ax.get_xticklabels(), visible=False)
        if i==3:
            ax = fig.add_subplot(4, 1, i+1, sharex=theAx)
            plt.xlabel('Date')

        plt.plot(short_date, pcPlot, color='b')
        
        if windowFlag:
            plt.plot(short_date, 
                     movingaverage(pcPlot, window), color='r')

        plt.axhline(0, color='k')
        plt.title(title)
        plt.ylabel(ylab)
    plt.show()

        ## Subset the dates to the last year of the dataset
    decade = [item for item in date_list if 
              item >= dt.datetime(2004, 6, 17) 
                  and item < dt.datetime(2014, 6, 17)]
    decadeIndices = [date_list.index(item) for item in decade]

    fig = plt.figure(figsize=(16, 16))
    ## Plot out the last year of the PCs to get a more detailed
    ## pattern for comparison to the R results
    for i in range(4):
        ylab = 'PC' + str(i+1)
        title = ylab + ' Time Series (1 decade)'

        pcPlot = pcs[np.array(decadeIndices),i]
        if i==0:
            theAx = fig.add_subplot(4, 1, i+1)
            plt.setp(theAx.get_xticklabels(), visible=False)
            theAx.set_xlabel('')
        if i>0 and i<3:
            ax = fig.add_subplot(4, 1, i+1, sharex=theAx)
            plt.setp(ax.get_xticklabels(), visible=False)
        if i==3:
            ax = fig.add_subplot(4, 1, i+1, sharex=theAx)
            plt.xlabel('Date')

        plt.plot(decade, pcPlot, color='b')
        if windowFlag:
            plt.plot(decade, 
                     movingaverage(pcPlot, window), color='r')
        plt.axhline(0, color='k')
        plt.title(title)
        plt.ylabel(ylab)
    plt.show()

# def main2(filepath):
#     ncin = Dataset(filepath, 'r')
#     slp = ncin.variables['value'][:]
#     print(slp)
#     lat = ncin.variables['latitude'][:]
#     print(lat)

if __name__ == '__main__':
    main('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/SLP_Anoms/anoms_matrix_equi-dist.csv',
        '/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/SLP_Anoms/lon-vec.csv',
        '/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/SLP_Anoms/lat-vec.csv',
        window=30, windowFlag=False)
    # main('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/SLP')
    # main2('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/slp.nc')