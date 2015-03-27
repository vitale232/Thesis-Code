#--------------------------------
# Name:         leave-one-out_predict-tmx-data-frame.R
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
library(devtools)
library(reshape2)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

then = Sys.time() # right now is then
predict = FALSE

melt_them = TRUE
merge_them = TRUE
load_som = TRUE
load_ra = TRUE
load_eof = TRUE
source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

m_tmx$jday = as.numeric(format(m_tmx$date, '%j'))
m_tmx$jday_cos = cos(2*pi/365 * m_tmx$jday)
m_tmx$jday_sin = sin(2*pi/365 * m_tmx$jday)
m_tmx$elev = m_tmx$elev/1000 # convert to km
m_tmx$irrad = m_tmx$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmx$zone = as.factor(sapply(strsplit(as.character(m_tmx$site), split='P'), function(x) x[1]))

#### Prepare the GIS Data (level 2 of model) ####
jdays = as.numeric(format(unique(m_tmx$date), '%j'))
dates = unique(m_tmx$date)
temp = raster('./GIS-Data/NLCD/nlcd_utm.tif')
elev = crop(raster('./GIS-Data/DEM/merged_UTM11.tif'), temp)/1000
tci = crop(raster('./GIS-Data/DEM/TCI_Whole-Range_UTM.tif'), temp)
tri = crop(terrain(elev, opt='TRI'), temp)
gis.slope = crop(terrain(elev, opt='slope', unit='degrees'), temp)
cc_nlcd = resample(temp, tri)
irrad = stack('./GIS-Data/Irradiance/irrad_stack.grd')


#### Iterate through the sites. Each site will be left out of the model fitting,
#### the tmx model will be run to create predictions for the year, and
#### the difference between the observations and predictions will be the model bias.
#### This procedure is done n times where n = 40
n_list = levels(m_tmx$site)
bias_tmx = data.frame(matrix(ncol=40, nrow=373))
names(bias_tmx) = n_list

print('Leave One Out Cross Validation for Tmx')
print(' Fitting models...')
for(i in 1:length(n_list)){
  n = n_list[i]
  print(paste('   ', n))
  
  ## pull out the nth site's data
  left_out = m_tmx[m_tmx$site == n, ]
  put_in   = m_tmx[m_tmx$site != n, ]
  
  if(predict){
    ## Fit the tmx model to the data that will be included or put_in the model i.e. != n
    ## na.action=na.omit is required on lme4 1.1-7
    tmx_mod = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                     cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                     ((elev+I(elev^2))|date)+(1|site), data=put_in, REML=FALSE, na.action=na.omit)
    
    #### Predict for tmx_mod
    stopifnot(predict)
    preds = predict(tmx_mod, left_out, re.form=~0)
    bias_tmx[ , i] = round(left_out$tmx - preds, 3)
    #   pred_stack = stack(pred_list)
    
  }
}


### Tell time if you can
Sys.time() - then

#### Add the dates to the bias_tmx dataframe. Note that this
#### is positional and will break with more or less data.
bias_tmx$date = dates
bias_tmx$month = format(dates, '%b')


#### write bias to disk
bias_tmx_filepath = './Temperature-Maps/Tmx/leave_one_out/bias_tmx.csv'

if(predict){
  # write.csv(bias_tmx, bias_tmx_filepath,
  #           row.names=FALSE)
  print(paste('Wrote ', bias_tmx_filepath, ' to disk.', sep=''))
}

bias_tmx = read.csv(bias_tmx_filepath)
bias_tmx$month = format(dates, '%b')

#### let it rip on some mean squared error
melt_bias_tmx = melt(bias_tmx[ , -(ncol(bias_tmx)-1)], id.vars='month')
casted_mse_tmx = dcast(melt_bias_tmx, month ~ variable, mse_for_me, 
                       na.rm=TRUE, margins=FALSE)
casted_mae_tmx = dcast(melt_bias_tmx, month ~ variable, mae_for_you,
                       na.rm=TRUE, margins=FALSE)

#### Calculate mean square error
mse = apply(casted_mse_tmx[ , -1], 1, mean, na.rm=TRUE)
names(mse) = casted_mse_tmx$month
rmse = sqrt(mse)

#### Calculate mean absolute error 
mae = apply(casted_mae_tmx[ , -1], 1, mean, na.rm=TRUE)
names(mae) = casted_mae_tmx$month


## aggregate bias by month
bias = aggregate(value ~ month, melt_bias_tmx, mean, na.rm=TRUE)
bias = round(bias$value, 2)

error_df = data.frame(mse, rmse, mae, bias,
                      'month'=casted_mse_tmx$month)

error_df$numeric_month = format(as.Date(paste(error_df$month, '01, 2001'),
                                        format='%b %d, %Y'), '%m')
error_df = error_df[order(error_df$numeric_month), ]
error_df = error_df[ , -which(names(error_df) == 'numeric_month')]
write.csv(error_df, './Temperature-Maps/Tmx/leave_one_out/error_df.csv',
          row.names=FALSE)

error_df_out = as.data.frame(
  round(t(error_df[ , -(which(names(error_df) == 'month'))]), 2))
error_df_out$Overall = round(apply(error_df_out, 1, mean), 2)

row.names(error_df_out) = c('MSE', 'RMSE', 'MAE', 'Bias')
write.csv(error_df_out,
          './Temperature-Maps/Tmx/leave_one_out/error_df_table_tmx.csv')

#### DONT FORGET ABOUT THOSE BEAUTIFUL HISTOGRAMS BELOW
bias_tmx_melt = melt(data=bias_tmx, id.vars=c('date', 'month'))
names(bias_tmx_melt) = c('date', 'month', 'site', 'bias')
bias_tmx_melt$month = factor(bias_tmx_melt$month, 
                             levels=c('Jan', 'Feb', 'Mar',
                                      'Apr', 'May', 'Jun', 
                                      'Jul', 'Aug', 'Sep',
                                      'Oct', 'Nov', 'Dec'))
bias_tmx_melt$zone= substring(
  as.character(bias_tmx_melt$site), 1, 2)


dateBias = ggplot(data=bias_tmx_melt) +
  geom_point(aes(date, bias, col=zone)) 
#   facet_wrap( ~ month)
print(dateBias)

histsTmx = ggplot(data=bias_tmx_melt) +
  geom_histogram(aes(bias, fill=month)) +
  facet_wrap( ~ month) +
  theme_gray(base_size=20) +
  geom_vline(xintercept=0, lty=2, color='gray40') +
  ggtitle('Maximum Temperature Model Bias (obs. - pred.)') +
  xlab(expression(paste('Bias (', degree, 'C)'))) +
  ylab('Count') +
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))
print(histsTmx)
ggsave('~/Dropbox/UNR/UNR-Thesis/Figures/bias_tmx-leave_one_out.pdf',
       width=10, height=7)

# mse_tmx = apply()

# #### calculate the error matrix
# errors_tmx = data.frame(date=dates,
#                         mse=mse_tmx,
#                         rmse=sqrt(mse_tmx), 
#                         site=names(mse_tmx),
#                         month=format(dates, '%b'))
# meleted_errors_tmx = metl(errors_tmx, id.vars='date'

#### melt bias down for some ggplott(2)ing
# melted_bias_tmx = melt(bias_tmx[ , -(ncol(bias_tmx)-1)], id.vars='month')
# melted_bias_tmx$month = factor(melted_bias_tmx$month, 
#                                levels=unique(format(
#                                  seq(as.Date('2000-01-01'), 
#                                      as.Date('2001-01-01'), 
#                                      by='day'), '%b')))
# names(melted_bias_tmx) = c('month', 'site', 'bias')
# 
# #### Plot out the bias
# histsTmx = ggplot(data=melted_bias_tmx) +
#   geom_histogram(aes(value, fill=month)) +
#   facet_wrap( ~ month) +
#   theme_gray(base_size=16) +
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'))
# print(histsTmx)
# 
# oneHistTmx = ggplot(data=melted_bias_tmx) +
#   geom_histogram(aes(value, fill=month)) +
#   theme_gray(base_size=16) +
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'))
# print(oneHistTmx)
