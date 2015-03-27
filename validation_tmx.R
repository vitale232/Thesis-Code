#--------------------------------
# Name:         validation_tmn.R
# Purpose:      Validate the tmn predictions using the NevCAN data
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/11/13
# R:            3.1.2
#--------------------------------

library(raster)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

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

#### Read in the prediction maps for tmx from cross17 preds
lf = list.files(file.path(getwd(), 'Temperature-Maps', 'Tmx', 'tmx_mod'), 
                pattern='.tif$', full.names=TRUE)

tmx_stack = stack(lf)

#### Read in the NevCAN data that's been cleaned up in Python
validation = read.csv('./Validation/validation_nevcan-daily.csv')
names(validation)[1] = 'date'
validation$date = as.Date(validation$date)

#### Read in the station coordinates
stations = read.csv('./Validation/stations.csv')
stations$long = stations$long * -1
coordinates(stations) = ~long+lat
proj4string(stations) = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
stations = spTransform(stations, CRS(projection(tmx_stack)))

#### Read in the road shapefile for mapping
roads = readOGR(dsn='/home/vitale232/Dropbox/UNR/UNR-Thesis/SiteMap', layer='DirtRoads')

#### Extract the predicted tmx values
preds = extract(tmx_stack, stations)
preds = t(preds)
preds = as.data.frame(preds)
names(preds) = stations$station
preds$date = as.Date(row.names(preds), format='X%Y.%m.%d_tmx')

#### Make a data.frame from validation and preds
df2 = merge(validation[ , c('date', 'sage_west_tmx', 'pj_west_tmx', 
                           'montane_west_tmx', 'subalpine_west_tmx')],
           preds[ , c('date', 'sagebrush_west', 'pj_west', 
                      'montane_west', 'subalpine_west')])
df2$sage = df2$sagebrush_west - df2$sage_west_tmx
df2$pj = df2$pj_west - df2$sage_west_tmx
df2$montane = df2$montane_west - df2$montane_west_tmx
df2$subalpine = df2$subalpine_west - df2$subalpine_west_tmx
df2[358, -1] = rep(NA, 12)

tp = matrix(rep(NA, 8), ncol=2)
rownames(tp) = c('sage', 'pj', 'montane', 'subalpine')
colnames(tp) = c('t', 'p')

tp['sage', c('t' ,'p')] = TP(df2$sage_west_tmx, df2$sagebrush_west)
tp['pj', c('t' ,'p')] = TP(df2$pj_west_tmx, df2$pj_west)
tp['montane', c('t' ,'p')] = TP(df2$montane_west_tmx, df2$montane_west)
tp['subalpine', c('t' ,'p')] = TP(df2$subalpine_west_tmx, df2$subalpine_west)

m = matrix(rep(NA, 15), ncol=3)
rownames(m) = c('sage', 'pj', 'montane', 'subalpine', 'overall')
colnames(m) = c('MAE', 'RMSE', 'R2')

m['sage', 'MAE'] = mae(df2$sagebrush_west, df2$sage_west_tmx, na.rm=TRUE)
m['pj', 'MAE'] = mae(df2$pj_west, df2$pj_west_tmx, na.rm=TRUE)
m['montane', 'MAE'] = mae(df2$montane_west, df2$montane_west_tmx, na.rm=TRUE)
m['subalpine', 'MAE'] = mae(df2$subalpine_west, df2$subalpine_west_tmx, na.rm=TRUE)
m['overall', 'MAE'] = with(df2, {
  mae(c(sagebrush_west, pj_west, montane_west, subalpine_west),
      c(sage_west_tmx, pj_west_tmx, montane_west_tmx, subalpine_west_tmx),
      na.rm=TRUE)})

m['sage', 'RMSE'] = rmse(df2$sagebrush_west, df2$sage_west_tmx, na.rm=TRUE)
m['pj', 'RMSE'] = rmse(df2$pj_west, df2$pj_west_tmx, na.rm=TRUE)
m['montane', 'RMSE'] = rmse(df2$montane_west, df2$montane_west_tmx, na.rm=TRUE)
m['subalpine', 'RMSE'] = rmse(df2$subalpine_west, df2$subalpine_west_tmx, na.rm=TRUE)
m['overall', 'RMSE'] = with(df2, {
  rmse(c(sagebrush_west, pj_west, montane_west, subalpine_west),
       c(sage_west_tmx, pj_west_tmx, montane_west_tmx, subalpine_west_tmx),
       na.rm=TRUE)})

m['sage', 'R2'] = cor(df2$sagebrush_west, df2$sage_west_tmx, use='complete.obs')
m['pj', 'R2'] = cor(df2$pj_west, df2$pj_west_tmx, use='complete.obs')
m['montane', 'R2'] = cor(df2$montane_west, df2$montane_west_tmx, use='complete.obs')
m['subalpine', 'R2'] = cor(df2$subalpine_west, df2$subalpine_west_tmx, use='complete.obs')
m['overall', 'R2'] = with(df2, {
  cor(c(sagebrush_west, pj_west, montane_west, subalpine_west),
      c(sage_west_tmx, pj_west_tmx, montane_west_tmx, subalpine_west_tmx),
      use='complete.obs')})

write.csv(round(m, 3), './Tables/Tmx/accuracy-table_mod_tmx.csv')
write.csv(round(tp, 3), './Tables/Tmx/t-test-table_mod_tmx.csv')


#### Calculate bias for table 5
tmx_by_month_raw = df2[, c('date', 'sage', 'pj', 'montane', 'subalpine')]
tmx_by_month_raw$month = factor(format(tmx_by_month_raw$date, '%b'),
                                levels=c('Jun', 'Jul', 'Aug', 'Sep',
                                         'Oct', 'Nov', 'Dec', 'Jan',
                                         'Feb', 'Mar', 'Apr', 'May'))

tmx_by_month = aggregate(. ~ month, 
                         data=tmx_by_month_raw[ , -which(names(tmx_by_month_raw) == 'date')], 
                         mean, na.rm=TRUE)
tmx_by_month$month = as.character(tmx_by_month$month)

#### Add row for overall and take the mean for each site
tmx_by_month[13, 'month'] = 'Overall'
tmx_by_month[13, 'sage'] = mean(df2$sage, na.rm=TRUE)
tmx_by_month[13, 'pj'] = mean(df2$pj, na.rm=TRUE)
tmx_by_month[13, 'montane'] = mean(df2$montane, na.rm=TRUE)
tmx_by_month[13, 'subalpine'] = mean(df2$subalpine, na.rm=TRUE)

#### add a column for monthly overalls and calculate overall overall
tmx_by_month_raw$overall = apply(tmx_by_month_raw[ , c('sage', 'pj', 'montane', 'subalpine')], 1, 
                                 mean, na.rm=TRUE)
tmx_by_month$overall = c(aggregate(overall ~ month, tmx_by_month_raw, mean, na.rm=TRUE)$overall, NA)

tmx_by_month[which(tmx_by_month$month == 'Overall'), 'overall'] = with(tmx_by_month_raw, {
  mean(c(sage, pj, montane, subalpine), na.rm=TRUE)})

row.names(tmx_by_month) = tmx_by_month$month
tmx_out = t(tmx_by_month[, -which(names(tmx_by_month) == 'month')])
row.names(tmx_out) = c('Sage', 'PJ', 'Montane', 'Subalpine', 'Overall')

write.csv(signif(tmx_out, 3), 
          './Tables/Tmx/overall-bias.csv')

### MAE by month
mae_by_month = df2
mae_by_month$month = factor(format(mae_by_month$date, '%b'),
                            levels=c('Jun', 'Jul', 'Aug', 'Sep',
                                     'Oct', 'Nov', 'Dec', 'Jan',
                                     'Feb', 'Mar', 'Apr', 'May'))


#### Caclulate the MAE by month and by site, including overalls for table 6 ####
tmx_mae = df2
tmx_mae$month = format(tmx_mae$date, '%b')


months = c('Jun', 'Jul', 'Aug', 'Sep',
           'Oct', 'Nov', 'Dec', 'Jan',
           'Feb', 'Mar', 'Apr', 'May')
tmx_mae_out = as.data.frame(matrix(nrow=5, ncol=14))
colnames(tmx_mae_out) = c('Site', months, 'Overall')
tmx_mae_out$Site = c('Sage', 'PJ', 'Montane', 
                     'Subalpine', 'Overall')

for(i in 1:length(months)){
  tmx_mae_out[tmx_mae_out$Site == 'Sage', months[i]] = with(tmx_mae[tmx_mae$month == months[i], ], {
    mae(sagebrush_west, sage_west_tmx, na.rm=TRUE)})
  tmx_mae_out[tmx_mae_out$Site == 'PJ', months[i]] = with(tmx_mae[tmx_mae$month == months[i], ], {
    mae(pj_west, pj_west_tmx, na.rm=TRUE)})
  tmx_mae_out[tmx_mae_out$Site == 'Montane', months[i]] = with(tmx_mae[tmx_mae$month == months[i], ], {
    mae(montane_west, montane_west_tmx, na.rm=TRUE)})
  tmx_mae_out[tmx_mae_out$Site == 'Subalpine', months[i]] = with(tmx_mae[tmx_mae$month == months[i], ], {
    mae(subalpine_west, subalpine_west_tmx, na.rm=TRUE)})
  tmx_mae_out[tmx_mae_out$Site == 'Overall', months[i]] = with(tmx_mae[tmx_mae$month == months[i], ], {
    mae(c(sagebrush_west, pj_west, montane_west, subalpine_west),
        c(sage_west_tmx, pj_west_tmx, montane_west_tmx, subalpine_west_tmx),
        na.rm=TRUE)})
}
tmx_mae_out$Overall = m[ , 'MAE']

write.csv(tmx_mae_out, './Tables/Tmx/MAE_by-site_by-month.csv')


#### Calculate RMSE table 7 for paper
tmx_rmse = df2
tmx_rmse$month = format(tmx_rmse$date, '%b')

months = c('Jun', 'Jul', 'Aug', 'Sep',
           'Oct', 'Nov', 'Dec', 'Jan',
           'Feb', 'Mar', 'Apr', 'May')
tmx_rmse_out = as.data.frame(matrix(nrow=5, ncol=14))
colnames(tmx_rmse_out) = c('Site', months, 'Overall')
tmx_rmse_out$Site = c('Sage', 'PJ', 'Montane', 
                     'Subalpine', 'Overall')

for(i in 1:length(months)){
  tmx_rmse_out[tmx_rmse_out$Site == 'Sage', months[i]] = with(tmx_rmse[tmx_rmse$month == months[i], ], {
    rmse(sagebrush_west, sage_west_tmx, na.rm=TRUE)})
  tmx_rmse_out[tmx_rmse_out$Site == 'PJ', months[i]] = with(tmx_rmse[tmx_rmse$month == months[i], ], {
    rmse(pj_west, pj_west_tmx, na.rm=TRUE)})
  tmx_rmse_out[tmx_rmse_out$Site == 'Montane', months[i]] = with(tmx_rmse[tmx_rmse$month == months[i], ], {
    rmse(montane_west, montane_west_tmx, na.rm=TRUE)})
  tmx_rmse_out[tmx_rmse_out$Site == 'Subalpine', months[i]] = with(tmx_rmse[tmx_rmse$month == months[i], ], {
    rmse(subalpine_west, subalpine_west_tmx, na.rm=TRUE)})
  tmx_rmse_out[tmx_rmse_out$Site == 'Overall', months[i]] = with(tmx_rmse[tmx_rmse$month == months[i], ], {
    rmse(c(sagebrush_west, pj_west, montane_west, subalpine_west),
        c(sage_west_tmx, pj_west_tmx, montane_west_tmx, subalpine_west_tmx),
        na.rm=TRUE)})
}
tmx_rmse_out$Overall = m[ , 'RMSE']

write.csv(tmx_rmse_out, './Tables/Tmx/RMSE_by-site_by-month.csv')





# month['sage', 'MAE'] = mae(tmx_by_month$sagebrush_west, tmx_by_month$sage_west_tmx, na.rm=TRUE)
# month['pj', 'MAE'] = mae(tmx_by_month$pj_west, tmx_by_month$pj_west_tmx, na.rm=TRUE)
# month['montane', 'MAE'] = mae(tmx_by_month$montane_west, tmx_by_month$montane_west_tmx, na.rm=TRUE)
# month['subalpine', 'MAE'] = mae(tmx_by_month$subalpine_west, tmx_by_month$subalpine_west_tmx, na.rm=TRUE)
# 
# month['sage', 'RMSE'] = rmse(tmx_by_month$sagebrush_west, tmx_by_month$sage_west_tmx, na.rm=TRUE)
# month['pj', 'RMSE'] = rmse(tmx_by_month$pj_west, tmx_by_month$pj_west_tmx, na.rm=TRUE)
# month['montane', 'RMSE'] = rmse(tmx_by_month$montane_west, tmx_by_month$montane_west_tmx, na.rm=TRUE)
# month['subalpine', 'RMSE'] = rmse(tmx_by_month$subalpine_west, tmx_by_month$subalpine_west_tmx, na.rm=TRUE)
# 
# month['sage', 'R2'] = cor(tmx_by_month$sagebrush_west, tmx_by_month$sage_west_tmx, use='complete.obs')
# month['pj', 'R2'] = cor(tmx_by_month$pj_west, tmx_by_month$pj_west_tmx, use='complete.obs')
# month['montane', 'R2'] = cor(tmx_by_month$montane_west, tmx_by_month$montane_west_tmx, use='complete.obs')
# month['subalpine', 'R2'] = cor(tmx_by_month$subalpine_west, tmx_by_month$subalpine_west_tmx, use='complete.obs')

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
# 
# plot(df$date, df$sage, ylim=c(-10, 20),
#      xlab='Date', ylab='prediction - observation (°C)')
# points(df$date, df$pj, col='orange')
# points(df$date, df$montane, col='darkgreen')
# points(df$date, df$subalpine, col='blue')
# abline(0, 0, lty=2)
# legend('topleft', legend=c('Sagebrush West', 'Pinyon-Juniper', 
#                            'Montane', 'Subalpine West'),
#        col=c('black', 'orange', 'darkgreen', 'blue'),
#        pch=1)