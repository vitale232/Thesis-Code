import pandas as pd
import matplotlib.pyplot as plt
import os

def main(filename, workspace=os.getcwd()):
    df = pd.read_csv(os.path.join(workspace, filename))
    print(df)
    df = df.set_index('date')
    print(df)

    fig = plt.figure(figsize=(20, 12))
    ax = plt.subplot(111)
    # fig = plt.figure(num=1, figsize=(30, 30), dpi=100)
    # df.plot(figsize=(30,8), ax=ax, legend=False, title=filename.split('.')[0])
    ax.plot(df)
    
    fig.autofmt_xdate()
    # plt.figure(num=1, figsize=(30, 30))
    ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    # plt.suptitle()
    plt.show()

    df.hist()
    plt.suptitle(filename.split('.')[0])
    plt.figure(num=1, figsize=(20,10))
    plt.show()


    df.boxplot()
    plt.show()


if __name__ == '__main__':
    workspace = '/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Aggregated_Data'
    filenames = []
    for item in os.listdir(workspace):
        if item.endswith('.csv'):
            filenames.append(item)

    for item in filenames:
        print item
        main(item, workspace)