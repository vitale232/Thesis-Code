#--------------------------------
# Name:         validation_initial-manipulation.py
# Purpose:      Aggregate the 10 min data from NevCAN stations to daily data 
# Created       2014/10/30
# Copyright:    (c) UNR and DRI
# Python:       2.7
#--------------------------------
import pandas as pd


def main(filenameIn, filenameOut, plot=True, figureOut=None):
    """
    Main function of this script.  Operates on disk.  It takes an input csv
    file that has been trimmed a bit that includes the crazy-dense NevCAN data.
    The function aggregates the data to daily maximum and daily minimum values,
    and then write these values to disk.  The maximum data that I originally passed
    in were downloaded from the http://nevcan.sensor.edu website.  I calculated
    tmx values on the "maximum" value that can bee obtained from the site, and vice
    versa for the "minimum" values and tmn.  This function can optionally plot the data
    and optionally write the plot to disk as a png.

    Arguments:
    filenameIn  = input filepath of the raw NevCAN data that's to be aggregated.
                  Must be a CSV file
    filenameOut = output filepath that is to be written with the aggregated values.
                  Must be a CSV file
    plot        = Defaults True.  Should the data be plotted?
    figureOut   = filepath for the plot. Must be a PNG file.
    """
    print('Reading {0}'.format(filenameIn))
    ## read in the data as a pandas dataframe
    nevcan = pd.read_csv(filenameIn, skiprows=9, index_col=0, parse_dates=True)
    if nevcan is None: ## check if it's a file
        print('\n\nERROR: Error reading file.')
        SystemExit() ## exit the program if it's not
    print(' success')

    ## access the column names and get the indices for which
    ## columns represent maximum temperature and which columns
    ## represent minimum temperature.
    names = list(nevcan.columns.values)
    max_indices = [i for i, x in enumerate(names) if x.split('_')[2] == 'tmx']
    min_indices = [i for i, x in enumerate(names) if x.split('_')[2] == 'tmn']

    ## subset the dataframe as 2 separate dataframes, and calculate the maximum
    ## and minimum value for each.  This operation is performed on each day, as
    ## specified by the groupby() function, which is essentailly aggregating the
    ## data.
    nevcan_daily_max = nevcan.iloc[:,max_indices].groupby(nevcan.index.date).max()
    nevcan_daily_min = nevcan.iloc[:,min_indices].groupby(nevcan.index.date).min()

    ## use pd.concat() to concatenate the two dataframes (tmx and tmn) back into
    ## one dataframe.  axis=1 tells pandas to combine them by columns, keeping
    ## each timeseries unaffected.
    daily_nevcan = pd.concat([nevcan_daily_max, nevcan_daily_min], axis=1)
    ## reset the index - pandas forgot they were dates for some reason
    daily_nevcan = daily_nevcan.set_index(pd.to_datetime(daily_nevcan.index))
    ## subset just the dates I need
    daily_nevcan = daily_nevcan['2013-06-17':'2014-06-24'] 

    ## write the dataframe out as a CSV
    print('Writing {0}'.format(filenameOut))
    daily_nevcan.to_csv(filenameOut)
    print(' NevCAN temperature 2 m temperature data has been aggregated to daily max and min.')

    ## print the data that's been saved out to the console for a quick and dirty look
    print('Data saved:')
    print(daily_nevcan)

    ## If the plot argument is True, use the plot_daily() function [defined below]
    ## to plot the data and optionally save 
    if plot: plot_daily(daily_nevcan, figureOut)

def plot_daily(daily_nevcan, figureOut):
    """
    Helper function to plot the nevcan data.  This function is called in main()
    if plot=True in the main() arguments.  The data will be plotted on two subplots
    with the x-axis shared.  These plots are interactive, and both the maximum
    and minimum plots will snap if you pan and zoom.  The maximum values are plotted
    on the top subplot, and the minimum values are on the bottom subplot, as indicated
    on the y-axes.

    Arguments:
    daily_nevcan    = The dataframe to be plotted.  This object is passed from within
                      the main() function [defined above]
    figureOut       = filename for the figure.  Must be PNG.  This argument is typically
                      passed from main() [defined above]
    """
    import matplotlib.pyplot as plt

    ## initialize the figure with a 20"x7" inch space (width x height)
    fig = plt.figure(figsize=(20, 8))

    ## manually set up the labels for the plot (sloppy, sloppy!)
    labs = ['sage_w_tmx', 'pj_w_tmn', 'montane_w_tmn', 'subalpine_w_tmx',
            'subalpine_e_tmn', 'sage_e_tmn', 'saltshrub_e_tmn']
    ## list comprehension that takes the label names as formatted for the
    ## data frame columns, and loops through them.  Each item in the
    ## labs list is split on the underscores.  The first and second 
    ## strings of the resultant split are passed through format, which
    ## is finally passed to .upper(), resulting in all caps names.
    labs = ['{0} {1}'.format(lab.split('_')[0], lab.split('_')[1]).upper() for lab in labs]

    ## Set up subplot1 as ax1.  211 indicates 2 rows, 1 column, select subplot 1
    ax1 = plt.subplot(211)
    ## plot each tmx value as a line in ax1
    l1, = ax1.plot(daily_nevcan.index, daily_nevcan['sage_west_tmx'], color='red', label=labs[0])
    l2, = ax1.plot(daily_nevcan.index, daily_nevcan['pj_west_tmx'], color='orange', label=labs[1])
    l3, = ax1.plot(daily_nevcan.index, daily_nevcan['montane_west_tmx'], color='gold', label=labs[2])
    l4, = ax1.plot(daily_nevcan.index, daily_nevcan['subalpine_west_tmx'], color='green', label=labs[3])
    l5, = ax1.plot(daily_nevcan.index, daily_nevcan['subalpine_east_tmx'], color='blue', label=labs[4])
    l6, = ax1.plot(daily_nevcan.index, daily_nevcan['sage_east_tmx'], color='violet', label=labs[5])
    l7, = ax1.plot(daily_nevcan.index, daily_nevcan['saltshrub_east_tmx'], color='black', label=labs[6])
    ## set y label.  the encoded text is a degree symbol (LaTex style)
    ax1.set_ylabel('Maximum Temperature ($^\circ$C)')
    ## make the x axis invisible, as it's shared with the following plot (tmn)
    plt.setp(ax1.get_xticklabels(), visible=False)

    ## Set up sublplot 2.  212 means 2 rows, 1 column, select the second plot
    ## The sharex=ax1 is telling matplotlib to share the x axis with the data
    ## plotted in ax1, which in this case is the first subplot.
    ax2 = plt.subplot(212, sharex=ax1)
    ## plot all of the tmn values as lines
    ax2.plot(daily_nevcan.index, daily_nevcan['sage_west_tmn'], color='red')
    ax2.plot(daily_nevcan.index, daily_nevcan['pj_west_tmn'], color='orange')
    ax2.plot(daily_nevcan.index, daily_nevcan['montane_west_tmn'], color='gold')
    ax2.plot(daily_nevcan.index, daily_nevcan['subalpine_west_tmn'], color='green')
    ax2.plot(daily_nevcan.index, daily_nevcan['subalpine_east_tmn'], color='blue')
    ax2.plot(daily_nevcan.index, daily_nevcan['sage_east_tmn'], color='violet')
    ax2.plot(daily_nevcan.index, daily_nevcan['saltshrub_east_tmn'], color='black')  
    ## set y label, degree symbol is encoded as $^\circ$ (LaTex style)
    ax2.set_ylabel('Minimum Temperature ($^\circ$C)')     

    ## get the locations and labels of the xticks from the plot
    locs, labels = plt.xticks()
    ## set the labels, and tell them to be rotated 45 degrees
    plt.setp(labels, rotation=45)
    ## set the x-axis label
    plt.xlabel('Date')

    ## set up parameters to pass for control of the legend size
    params = {'legend.fontsize': 12,
              'legend.linewidth': 2}
    ## update the rcParams parameters, which is accessing
    ## the 'highest level' matplotlib module
    ## it seems like rcParams controls all of the default values for mpl,
    ## like font size, style, color, etc.
    plt.rcParams.update(params)
    ## set up the legend, passing the lines from tmx for the colors,
    ## and each of the labels.  Since I'm operating on fig rather than 
    ## ax1 or ax2, the legend will be outside of the plot (mostly)
    fig.legend((l1, l2, l3, l4, l5, l6, l7), (labs[0], labs[1], labs[2], labs[3], 
                                              labs[4], labs[5], labs[6]), 'center right')

    ## True if figureOut filepath was specified.  If it was, save the figure.
    ## if not, move on.
    if figureOut is not None: plt.savefig(figureOut, bbox_inches='tight')

    plt.show() # show the figure

if __name__ == '__main__':
    main(filenameIn='/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Validation/validation_nevcan-raw.csv',
        filenameOut='/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Validation/validation_nevcan-daily_two.csv',
        plot=True, figureOut='/home/vitale232/Desktop/validation_plot.png')