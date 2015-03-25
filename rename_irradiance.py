import os
import shutil
import glob

def main(workspace=os.getcwd()):
    os.chdir(workspace)
    file_list = glob.glob('*.tif')

    split_names = []
    for item in file_list:
        split_names.append(item.split('.')[0])

    doy = []
    for item in split_names:
        doy.append(item.split('_')[2])

    padded = []
    for item in doy:
        padded.append('{0:03d}'.format(int(item)))

    for i in range(len(file_list)):
        shutil.copyfile(file_list[i], 
            os.path.join(os.getcwd(), 'new_name', 'irradiance_{0}.tif'.format(padded[i])))

if __name__ == '__main__':
    main('/media/george/Student_Folders/Common_Data/Snake-Range_Irradiance/Whole_Range/new_name')