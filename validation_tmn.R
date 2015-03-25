#--------------------------------
# Name:         validation_tmn.R
# Purpose:      Validate the tmn predictions using the NevCAN data
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/11/13
# R:            3.1.2
#--------------------------------

library(raster)

setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

#### Define functions
mae = function(preds, obs, ...){
  mean(abs(preds - obs), ...)
}
rmse = function(preds, obs, ...){
  err = preds - obs
  sqrt(mean(err^2, ...))
}
TP = function(x, y, ...){
  t = t.test(x, y, ...)$statistic
  p = t.test(x, y, ...)$p.value
  out = c(t, p)
  names(out) = c('t', 'p')
  out
}

#### Read in the prediction maps for Tmn from cross16 preds
lf = list.files(file.path(getwd(), 'Temperature-Maps', 'Tmn', 'tmn_mod'), 
                pattern='.tif$', full.names=TRUE)

tmn_stack = stack(lf)

#### Read in the NevCAN data that's been cleaned up in Python
validation = read.csv('./Validation/validation_nevcan-daily.csv')
names(validation)[1] = 'date'
validation$date = as.Date(validation$date)

#### Read in the station coordinates
stations = read.csv('./Validation/stations.csv')
stations$long = stations$long * -1
coordinates(stations) = ~long+lat
proj4string(stations) = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
stations = spTransform(stations, CRS(projection(tmn_stack)))

#### Read in the road shapefile for mapping
roads = readOGR(dsn='/home/vitale232/Google Drive/UNR/UNR-Thesis/SiteMap', layer='DirtRoads')

#### Extract the predicted Tmn values
preds = extract(tmn_stack, stations)
preds = t(preds)
preds = as.data.frame(preds)
names(preds) = stations$station
preds$date = as.Date(row.names(preds), format='X%Y.%m.%d_tmn')

#### Make a data.frame from validation and preds
df = merge(validation[ , c('date', 'sage_west_tmn', 'pj_west_tmn', 
                           'montane_west_tmn', 'subalpine_west_tmn')],
           preds[ , c('date', 'sagebrush_west', 'pj_west', 
                      'montane_west', 'subalpine_west')])
df$sage = df$sagebrush_west - df$sage_west_tmn
df$pj = df$pj_west - df$sage_west_tmn
df$montane = df$montane_west - df$montane_west_tmn
df$subalpine = df$subalpine_west - df$subalpine_west_tmn

tp = matrix(rep(NA, 8), ncol=2)
rownames(tp) = c('sage', 'pj', 'montane', 'subalpine')
colnames(tp) = c('t', 'p')

tp['sage', c('t' ,'p')] = TP(df$sage_west_tmn, df$sagebrush_west)
tp['pj', c('t' ,'p')] = TP(df$pj_west_tmn, df$pj_west)
tp['montane', c('t' ,'p')] = TP(df$montane_west_tmn, df$montane_west)
tp['subalpine', c('t' ,'p')] = TP(df$subalpine_west_tmn, df$subalpine_west)

m = matrix(rep(NA, 15), ncol=3)
rownames(m) = c('sage', 'pj', 'montane', 'subalpine', 'overall')
colnames(m) = c('MAE', 'RMSE', 'R2')

m['sage', 'MAE'] = mae(df$sagebrush_west, df$sage_west_tmn, na.rm=TRUE)
m['pj', 'MAE'] = mae(df$pj_west, df$pj_west_tmn, na.rm=TRUE)
m['montane', 'MAE'] = mae(df$montane_west, df$montane_west_tmn, na.rm=TRUE)
m['subalpine', 'MAE'] = mae(df$subalpine_west, df$subalpine_west_tmn, na.rm=TRUE)
m['overall', 'MAE'] = with(df, {
                           mae(c(sagebrush_west, pj_west, montane_west, subalpine_west),
                               c(sage_west_tmn, pj_west_tmn, montane_west_tmn, subalpine_west_tmn),
                               na.rm=TRUE)})

m['sage', 'RMSE'] = rmse(df$sagebrush_west, df$sage_west_tmn, na.rm=TRUE)
m['pj', 'RMSE'] = rmse(df$pj_west, df$pj_west_tmn, na.rm=TRUE)
m['montane', 'RMSE'] = rmse(df$montane_west, df$montane_west_tmn, na.rm=TRUE)
m['subalpine', 'RMSE'] = rmse(df$subalpine_west, df$subalpine_west_tmn, na.rm=TRUE)
m['overall', 'RMSE'] = with(df, {
                           rmse(c(sagebrush_west, pj_west, montane_west, subalpine_west),
                                c(sage_west_tmn, pj_west_tmn, montane_west_tmn, subalpine_west_tmn),
                                na.rm=TRUE)})

m['sage', 'R2'] = cor(df$sagebrush_west, df$sage_west_tmn, use='complete.obs')
m['pj', 'R2'] = cor(df$pj_west, df$pj_west_tmn, use='complete.obs')
m['montane', 'R2'] = cor(df$montane_west, df$montane_west_tmn, use='complete.obs')
m['subalpine', 'R2'] = cor(df$subalpine_west, df$subalpine_west_tmn, use='complete.obs')
m['overall', 'R2'] = with(df, {
                           cor(c(sagebrush_west, pj_west, montane_west, subalpine_west),
                           c(sage_west_tmn, pj_west_tmn, montane_west_tmn, subalpine_west_tmn),
                           use='complete.obs')})


write.csv(m, './Validation/accuracy-table_tmn_mod.csv')
write.csv(tp, './Validation/t-test-table_tmn_mod-tmn.csv')


#### Calculate bias for table 5
tmn_by_month_raw = df[, c('date', 'sage', 'pj', 'montane', 'subalpine')]
tmn_by_month_raw$month = factor(format(tmn_by_month_raw$date, '%b'),
                            levels=c('Jun', 'Jul', 'Aug', 'Sep',
                                     'Oct', 'Nov', 'Dec', 'Jan',
                                     'Feb', 'Mar', 'Apr', 'May'))

tmn_by_month = aggregate(. ~ month, 
                         data=tmn_by_month_raw[ , -which(names(tmn_by_month_raw) == 'date')], 
                         mean, na.rm=TRUE)
tmn_by_month$month = as.character(tmn_by_month$month)

#### Add row for overall and take the mean for each site
tmn_by_month[13, 'month'] = 'Overall'
tmn_by_month[13, 'sage'] = mean(df$sage, na.rm=TRUE)
tmn_by_month[13, 'pj'] = mean(df$pj, na.rm=TRUE)
tmn_by_month[13, 'montane'] = mean(df$montane, na.rm=TRUE)
tmn_by_month[13, 'subalpine'] = mean(df$subalpine, na.rm=TRUE)

#### add a column for monthly overalls and calculate overall overall
tmn_by_month_raw$overall = apply(tmn_by_month_raw[ , c('sage', 'pj', 'montane', 'subalpine')], 1, 
                                 mean, na.rm=TRUE)
tmn_by_month$overall = c(aggregate(overall ~ month, tmn_by_month_raw, mean, na.rm=TRUE)$overall, NA)

tmn_by_month[which(tmn_by_month$month == 'Overall'), 'overall'] = with(tmn_by_month_raw, {
                                              mean(c(sage, pj, montane, subalpine), na.rm=TRUE)})

row.names(tmn_by_month) = tmn_by_month$month
tmn_out = t(tmn_by_month[, -which(names(tmn_by_month) == 'month')])
row.names(tmn_out) = c('Sage', 'PJ', 'Montane', 'Subalpine', 'Overall')

write.csv(signif(tmn_out, 3), 
          './Tables/Tmn/overall-bias.csv')


#### Caclulate the MAE by month and by site, including overalls for table 6 ####
tmn_mae = df
tmn_mae$month = format(tmn_mae$date, '%b')


months = c('Jun', 'Jul', 'Aug', 'Sep',
           'Oct', 'Nov', 'Dec', 'Jan',
           'Feb', 'Mar', 'Apr', 'May')
tmn_mae_out = as.data.frame(matrix(nrow=5, ncol=14))
colnames(tmn_mae_out) = c('Site', months, 'Overall')
tmn_mae_out$Site = c('Sage', 'PJ', 'Montane', 
                     'Subalpine', 'Overall')

for(i in 1:length(months)){
  tmn_mae_out[tmn_mae_out$Site == 'Sage', months[i]] = with(tmn_mae[tmn_mae$month == months[i], ], {
    mae(sagebrush_west, sage_west_tmn, na.rm=TRUE)})
  tmn_mae_out[tmn_mae_out$Site == 'PJ', months[i]] = with(tmn_mae[tmn_mae$month == months[i], ], {
    mae(pj_west, pj_west_tmn, na.rm=TRUE)})
  tmn_mae_out[tmn_mae_out$Site == 'Montane', months[i]] = with(tmn_mae[tmn_mae$month == months[i], ], {
    mae(montane_west, montane_west_tmn, na.rm=TRUE)})
  tmn_mae_out[tmn_mae_out$Site == 'Subalpine', months[i]] = with(tmn_mae[tmn_mae$month == months[i], ], {
    mae(subalpine_west, subalpine_west_tmn, na.rm=TRUE)})
  tmn_mae_out[tmn_mae_out$Site == 'Overall', months[i]] = with(tmn_mae[tmn_mae$month == months[i], ], {
    mae(c(sagebrush_west, pj_west, montane_west, subalpine_west),
        c(sage_west_tmn, pj_west_tmn, montane_west_tmn, subalpine_west_tmn),
        na.rm=TRUE)})
}
tmn_mae_out$Overall = m[ , 'MAE']

write.csv(tmn_mae_out, './Tables/Tmn/MAE_by-site_by-month.csv')



#### Calculate RMSE table 7 for paper
tmn_rmse = df
tmn_rmse$month = format(tmn_rmse$date, '%b')

months = c('Jun', 'Jul', 'Aug', 'Sep',
           'Oct', 'Nov', 'Dec', 'Jan',
           'Feb', 'Mar', 'Apr', 'May')
tmn_rmse_out = as.data.frame(matrix(nrow=5, ncol=14))
colnames(tmn_rmse_out) = c('Site', months, 'Overall')
tmn_rmse_out$Site = c('Sage', 'PJ', 'Montane', 
                      'Subalpine', 'Overall')

for(i in 1:length(months)){
  tmn_rmse_out[tmn_rmse_out$Site == 'Sage', months[i]] = with(tmn_rmse[tmn_rmse$month == months[i], ], {
    rmse(sagebrush_west, sage_west_tmn, na.rm=TRUE)})
  tmn_rmse_out[tmn_rmse_out$Site == 'PJ', months[i]] = with(tmn_rmse[tmn_rmse$month == months[i], ], {
    rmse(pj_west, pj_west_tmn, na.rm=TRUE)})
  tmn_rmse_out[tmn_rmse_out$Site == 'Montane', months[i]] = with(tmn_rmse[tmn_rmse$month == months[i], ], {
    rmse(montane_west, montane_west_tmn, na.rm=TRUE)})
  tmn_rmse_out[tmn_rmse_out$Site == 'Subalpine', months[i]] = with(tmn_rmse[tmn_rmse$month == months[i], ], {
    rmse(subalpine_west, subalpine_west_tmn, na.rm=TRUE)})
  tmn_rmse_out[tmn_rmse_out$Site == 'Overall', months[i]] = with(tmn_rmse[tmn_rmse$month == months[i], ], {
    rmse(c(sagebrush_west, pj_west, montane_west, subalpine_west),
         c(sage_west_tmn, pj_west_tmn, montane_west_tmn, subalpine_west_tmn),
         na.rm=TRUE)})
}
tmn_rmse_out$Overall = m[ , 'RMSE']

write.csv(tmn_rmse_out, './Tables/Tmn/RMSE_by-site_by-month.csv')



# 
# plot(df$date, df$sage, type='l', ylim=c(-10, 20),
#      xlab='Date', ylab='prediction - observation (°C)')
# lines(df$date, df$pj, col='orange')
# lines(df$date, df$montane, col='darkgreen')
# lines(df$date, df$subalpine, col='blue')
# abline(0, 0, lty=2)
# legend('topleft', legend=c('Sagebrush West', 'Pinyon-Juniper', 
#                            'Montane', 'Subalpine West'),
#        col=c('black', 'orange', 'darkgreen', 'blue'),
#        lty=1)
# 
# setEPS(height=7, width=11)
# postscript('~/Google Drive/UNR/UNR-Thesis/Figures/validation_tmn_pred-obs.eps')
# plot(df$date, df$sage, ylim=c(-10, 20),
#      xlab='Date', ylab='prediction - observation (°C)',
#      cex.lab=1.25, cex.axis=1.25)
# points(df$date, df$pj, col='orange')
# points(df$date, df$montane, col='darkgreen')
# points(df$date, df$subalpine, col='blue')
# abline(0, 0, lty=2)
# legend('topleft', legend=c('Sagebrush West', 'Pinyon-Juniper', 
#                            'Montane', 'Subalpine West'),
#        col=c('black', 'orange', 'darkgreen', 'blue'),
#        pch=1)
# dev.off()
