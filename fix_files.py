#--------------------------------
# Name:         fix_files.py
# Purpose:      Makes the logtag CSV files readable on linux
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/06/30
# Python:       2.7
#--------------------------------

import os, sys, csv
import pandas

"""
This script loops through a directory of logtag data and rewrites the files.
I needed to do this as R was unable to read the files in Windows or Linux
and most Linux programs were struggling.  It seems like the LogTag software
is using a funky character encoding when saving to CSV.  Waiting to hear back from the 
manufacturer on what their default is.

The only piece of software needed in addition to python 2.7 to run this script
is the Pandas module.

To run the program:

(1) Move all LogTag (CSV) data to the same directory.
(2) Open the terminal and change directory to directory of interest.
(3) Run the program.

To run on Linux/Mac:

cd <directory_containing_data>
python <path_to_this_file>

OR  if you do not want to change the terminal to the data directory,
change workspace after __name__ == '__main__' call to the directory 
where the data are stored (bottom of script).
"""

def main(workspace=os.getcwd()):

    ## Change the directory to that of the terminal by default
    ## Can specify if need be
    os.chdir(workspace)

    # set up the output directory
    out_dir = os.path.join(workspace, 'Python_Output')

    print('\nRe-Writing the LogTag CSVs to be read through R')
    print(' Run the program from folder with all of the data!!')

    ## test for and create output directory
    if not os.path.isdir(out_dir):
        os.mkdir(out_dir)
        print('\nCreating directory: {0}'.format(out_dir))
    else:
        print('\nDirectory already exists: {0}'.format(out_dir))

    # initialize and fill file list
    file_list = []

    for item in os.listdir(os.getcwd()):
        if item.endswith('.csv') or item.endswith('.CSV'):
            file_list.append(item)

    ### Do the work
    # loop through the files and read them in as pandas csv
    # strip date and time of excessive quiotes
    # write as csv
    for item in file_list:
        out_path = os.path.join(out_dir, item)
        print('\nWRITING FILE: {0}'.format(out_path))

        df = pandas.read_csv(item, header=0)
        # print(df)
        try:
            df["Date"] = df["Date"].str.replace('"', '')
        except:
            continue

        try:
            df["Time"] = df["Time"].str.replace('"', '')
        except:
            continue

        df.to_csv(os.path.join(out_dir, item), sep='\t', 
            index=False, quoting=csv.QUOTE_NONE, doublequote=False)

        print('  Write successful')
        del out_path, df

if __name__ == '__main__':
    main()