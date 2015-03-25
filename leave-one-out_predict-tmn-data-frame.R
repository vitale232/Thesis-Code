#--------------------------------
# Name:         leave-one-out_predict-tmn-data-frame.R
# Purpose:      Perform a leave-one-out k-fold validation of tmn and tmx models
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2015/02/02
# R:            3.1.2
#--------------------------------
mse_for_me = function(error, ...){
  return(mean(error^2, ...))
}
mae_for_you = function(error, ...){
  return(mean(abs(error), ...))
}

library(raster)
library(lme4)
library(ggplot2)
library(reshape2)

setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

then_again = Sys.time() # now it's then again, not now
predict = FALSE

melt_them = TRUE
merge_them = TRUE
load_som = TRUE
load_ra = TRUE
load_eof = TRUE
source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

m_tmn$jday = as.numeric(format(m_tmn$date, '%j'))
m_tmn$jday_cos = cos(2*pi/365 * m_tmn$jday)
m_tmn$jday_sin = sin(2*pi/365 * m_tmn$jday)
m_tmn$elev = m_tmn$elev/1000 # convert to km
m_tmn$irrad = m_tmn$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmn$zone = as.factor(sapply(strsplit(
  as.character(m_tmn$site), split='P'), function(x) x[1]))
dates = unique(m_tmx$date)

# #### Prepare the GIS Data (level 2 of model) ####
# jdays = as.numeric(format(unique(m_tmx$date), '%j'))
# temp = raster('./GIS-Data/NLCD/nlcd_utm.tif')
# elev = crop(raster('./GIS-Data/DEM/merged_UTM11.tif'), temp)/1000
# tci = crop(raster('./GIS-Data/DEM/TCI_Whole-Range_UTM.tif'), temp)
# tri = crop(terrain(elev, opt='TRI'), temp)
# gis.slope = crop(terrain(elev, opt='slope', unit='degrees'), temp)
# cc_nlcd = resample(temp, tri)
# irrad = stack('./GIS-Data/Irradiance/irrad_stack.grd')
# 

#### Iterate through the sites. Each site will be left out of the model fitting,
#### the tmn model will be run to create predictions for the year, and
#### the difference between the observations and predictions will be the model bias.
#### This procedure is done n times where n = 40
n_list = levels(m_tmn$site)
bias_tmn = data.frame(matrix(ncol=40, nrow=373))
names(bias_tmn) = n_list

print('Leave One Out Cross Validation for Tmn')
print(' Fitting models...')
for(i in 1:length(n_list)){
  n = n_list[i]
  print(paste('   ', n))
  
  ## pull out the nth site's data
  left_out = m_tmn[m_tmn$site == n, ]
  put_in   = m_tmn[m_tmn$site != n, ]
  
  if(predict){
    ## Fit the tmn model to the data that will be included or put_in the model i.e. != n
    ## na.action=na.omit is required on lme4 1.1-7
    tmn_mod = lmer(tmn~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                     cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                     ((elev+I(elev^2))|date)+(1|site), data=put_in, REML=FALSE, na.action=na.omit)
    
    #### Predict for tmn_mod
    stopifnot(predict)
    preds = predict(tmn_mod, left_out, re.form=~0)
    bias_tmn[ , i] = round(left_out$tmn - preds, 3)
    #   pred_stack = stack(pred_list)
  }
}


#### Add the dates to the bias_tmn dataframe. Note that this
#### is positional and will break with more or less data.
bias_tmn$date = dates
bias_tmn$month = format(dates, '%b')


Sys.time() - then_again
if(predict){
  bias_tmn_filepath = './Temperature-Maps/Tmn/leave_one_out/bias_tmn.csv'
  write.csv(bias_tmn, bias_tmn_filepath,
            row.names=FALSE)
  print(paste('Wrote ', bias_tmn_filepath, ' to disk.', sep=''))
}


bias_tmn_filepath = './Temperature-Maps/Tmn/leave_one_out/bias_tmn.csv'
#### Calculate some fit metrics like MAE, MSE, RMSE
bias_tmn = read.csv(bias_tmn_filepath)
bias_tmn$month = format(dates, '%b')

#### let it rip on some mean squared error
melt_bias_tmn = melt(bias_tmn[ , -(ncol(bias_tmn)-1)], id.vars='month')
casted_mse_tmn = dcast(melt_bias_tmn, month ~ variable, mse_for_me, 
                       na.rm=TRUE, margins=FALSE)
casted_mae_tmn = dcast(melt_bias_tmn, month ~ variable, mae_for_you,
                       na.rm=TRUE, margins=FALSE)

#### Calculate mean square error
mse = apply(casted_mse_tmn[ , -1], 1, mean, na.rm=TRUE)
names(mse) = casted_mse_tmn$month
rmse = sqrt(mse)

#### Calculate mean absolute error 
mae = apply(casted_mae_tmn[ , -1], 1, mean, na.rm=TRUE)
names(mae) = casted_mae_tmn$month

## aggregate bias by month
bias = aggregate(value ~ month, melt_bias_tmn, mean, na.rm=TRUE)
bias = round(bias$value, 2)

error_df = data.frame(mse, rmse, mae, bias,
                      'month'=casted_mse_tmn$month)
error_df$numeric_month = format(as.Date(paste(error_df$month, '01, 2001'),
                                        format='%b %d, %Y'), '%m')
error_df = error_df[order(error_df$numeric_month), ]
error_df = error_df[ , -which(names(error_df) == 'numeric_month')]
write.csv(error_df, './Temperature-Maps/Tmn/leave_one_out/error_df.csv',
          row.names=FALSE)

error_df_out = as.data.frame(
  round(t(error_df[ , -(which(names(error_df) == 'month'))]), 2))
error_df_out$Overall = round(apply(error_df_out, 1, mean), 2)

row.names(error_df_out) = c('MSE', 'RMSE', 'MAE', 'Bias')
write.csv(error_df_out,
          './Temperature-Maps/Tmn/leave_one_out/error_df_table_tmn.csv')


#### DONT FORGET ABOUT THOSE BEAUTIFUL HISTOGRAMS BELOW
# bias_tmn_melt = melt(data=bias_tmn, id.vars=c('date', 'month'))
# names(bias_tmn_melt) = c('date', 'month', 'site', 'bias')
# bias_tmn_melt$month = factor(bias_tmn_melt$month, 
#                              levels=c('Jan', 'Feb', 'Mar',
#                                       'Apr', 'May', 'Jun', 
#                                       'Jul', 'Aug', 'Sep',
#                                       'Oct', 'Nov', 'Dec'))
# bias_tmn_melt$zone= substring(
#   as.character(bias_tmn_melt$site), 1, 2)
# 
# histsTmn = ggplot(data=bias_tmn_melt) +
#   geom_histogram(aes(bias, fill=month)) +
#   facet_wrap( ~ month) +
#   theme_gray(base_size=20) +
#   geom_vline(xintercept=0, lty=2, color='gray40') +
#   ggtitle('Minimum Temperature Model Bias (obs. - pred.)') +
#   xlab(expression(paste('Bias (', degree, 'C)'))) +
#   ylab('Count') +
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'))
# print(histsTmn)
# ggsave('~/Google Drive/UNR/UNR-Thesis/Figures/bias_tmn-leave_one_out.pdf',
#        width=10, height=7)