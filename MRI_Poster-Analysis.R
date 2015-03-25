#--------------------------------
# Name:         MRI_Poster-Analysis_tmn.R
# Purpose:      Script for basic manipulation of data (see Data-Manipulation_Initial.R)
#               and the analysis conducted for the 2014 MRI poster
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/07/13
# R:            3.1.1
#--------------------------------

library(raster)
library(rgdal)

setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

# #### Read in and merge reanalysis data
# ra13 = read.csv('./Reanalysis/Reanalysis-Data_2013.csv')
# ra14 = read.csv('./Reanalysis/Reanalysis-Data_2014.csv')
# 
# ra = rbind(ra13, ra14)
# 
# ## Write out the rbinded data
# write.csv(ra, './Reanalysis/Reanalysis-Data.csv', 
#           row.names = FALSE)
# 

#### Read in the rbinded data and the Snake dataset
#### and the TCI data
ra = read.csv('./Reanalysis/Reanalysis-Data.csv', header = TRUE)
tci = raster('./GIS-Data/TCI_Whole-Range_UTM.tif')
snake = read.csv('./Site-Data/snake_july-2014.csv', 
                 header = TRUE, as.is = 1)
coordinates(snake) = ~x_utm+y_utm
proj4string(snake) = CRS(projection(snake))

#### add another TCI column nto see if recalculating was worth it
# snake$tci2 = extract(tci, snake)
# boxplot(snake$tci, snake$tci2)
# 
# ###### Well worth it! overwrite them
# snake$tci = extract(tci, snake)
# snake$tci2 = NULL
# write.csv(snake, './Site-Data/snake_july-2014.csv', row.names = FALSE)

#### Read in the 2 m sensor data
tmn_orig = read.csv('./Aggregated_Data/m-tmn.csv')
tmx_orig = read.csv('./Aggregated_Data/m-tmx.csv')
tav_orig = read.csv('./Aggregated_Data/m-tav.csv')

## Merge temperature and the reanalysis data
tmn = merge(tmn_orig, ra, by = 'date')
tmx = merge(tmx_orig, ra, by = 'date')
tav = merge(tav_orig, ra, by = 'date')

## Make a data frame for later use to keep track of residuals
out_tmn = data.frame('date' = as.Date(tmn$date), 'index' = 1:length(tmn$date))
out_tmx = data.frame('date' = as.Date(tmx$date), 'index' = 1:length(tmx$date))
out_tav = data.frame('date' = as.Date(tav$date), 'index' = 1:length(tmx$date))

######################################################
# Work on tmn (minimum temperature) first
######################################################
#### Linear model of site's temperature by reanalysis T
## Make a function to do the work
mods_fun = function(i) {
  ys = names(tmn)
  
  temp.df <- data.frame(tmn[ , ys[i]], tmn$tair)
  names(temp.df) = c(ys[i], 'tair')
  
  return(lm(temp.df[ , 1] ~ temp.df$tair, na.action = na.omit))
}

## Apply the function to columns 2:41 and out_tmnput a list
mod_list = lapply(2:41, mods_fun)

names = names(tmn)[c(-1, -42:-48)]
names(mod_list) = names

## Get the summaries
mod_sums = lapply(mod_list, summary)
names(mod_sums) = names

## Get the residuals
mod_resids = lapply(mod_list, resid)
names(mod_resids) = names

#### Make a function to match the residuals with dates
for(i in 1:length(mod_resids)){
  col = which(names(tmn) == names(mod_resids[i]))

  w = which(!is.na(tmn[, col]))
  
  df = data.frame(w, mod_resids[[i]])
  names(df) = c('index', names(mod_resids[i]))
  
  out_tmn = merge(out_tmn, df, all.x = TRUE)
  rm(list = c('df', 'w', 'col'))
}

#### Write out_tmn the out_tmn data.frame
out_tmn = out_tmn[ , -which(names(out_tmn) == 'index')]
write.csv(out_tmn, './MRI/tmn_resids.csv', row.names = FALSE)

#### run the model on each day with tci
df = data.frame('id_snake' = snake$ID, 'id_list' = names(out_tmn)[-1])
df$id_snake == df$id_list
rm(df)

get_tci = function(i){
  v = as.vector(as.matrix(out_tmn[i, -1]))
  
  return(lm(v ~ tci_v))
}

## make a vector of tci to rinse and reuse
tci_v = snake$tci

## make the models and store as lists
tci_mods = lapply(1:nrow(out_tmn), get_tci)
tci_sums = lapply(tci_mods, summary)
rsq_tci = sapply(1:length(tci_mods), function(i) tci_sums[[i]]$r.squared)

#### add month to the out_tmn data.frame and aggreate by month
out_tmn$month = strftime(out_tmn$date, '%m')
out_tmn$year = strftime(out_tmn$date, '%Y')
out_tmn$agg_date = as.Date(paste(out_tmn$year, out_tmn$month, '01', sep = '-'))
out_tmn$tci_rsq = rsq_tci

agg_mean_tmn = aggregate(rsq_tci ~ agg_date, out_tmn, mean)
plot(agg_mean_tmn$agg_date, agg_mean_tmn$rsq_tci, type = 'l',
     ylim = c(0, .5))



#### run the model on each day with elevation
## make a vector of elev to rinse and reuse
elev = snake$elev

get_elev = function(i){
  v = as.vector(as.matrix(out_tmn[i, 2:41]))
  
  return(lm(v ~ elev))
}

## make the models and store as lists
elev_mods = lapply(1:nrow(out_tmn), get_elev)
elev_sums = lapply(elev_mods, summary)
rsq_elev = sapply(1:length(elev_sums), function(i) elev_sums[[i]]$r.squared)

#### add month to the out_tmn data.frame and aggreate by month
out_tmn$tci_rsq = rsq_elev

# agg_mean_tmn = aggregate(cbind(rsq_tci, rsq_elev) ~ month, out_tmn, mean)
# plot(agg_mean_tmn$month, agg_mean_tmn$rsq_elev_tmn, type = 'l')
# lines(agg_mean_tmn$month, agg_mean_tmn$rsq_tci_tmn, col = 2)

#### run the model on each day with elevation
## make a vector of elev to rinse and reuse
hli = snake$hli

get_hli = function(i){
  v = as.vector(as.matrix(out_tmn[i, 2:41]))
  
  return(lm(v ~ hli))
}

## make the models and store as lists
hli_mods = lapply(1:nrow(out_tmn), get_hli)
hli_sums = lapply(hli_mods, summary)
rsq_hli = sapply(1:length(hli_sums), function(i) hli_sums[[i]]$r.squared)

#### add month to the out_tmn data.frame and aggreate by month
out_tmn$tci_rsq = rsq_hli

agg_mean_tmn = aggregate(cbind(rsq_tci, rsq_elev, rsq_hli) ~ agg_date, out_tmn, mean)
names(agg_mean_tmn) = c('agg_date', 'rsq_tci_tmn', 'rsq_elev_tmn', 'rsq_hli_tmn')
plot(agg_mean_tmn$agg_date, agg_mean_tmn$rsq_elev_tmn, type = 'l', 
     ylim = (c(0,1)), xlab = 'R squared', ylab = 'agg_date')
lines(agg_mean_tmn$agg_date, agg_mean_tmn$rsq_tci_tmn, col = 'blue')
lines(agg_mean_tmn$agg_date, agg_mean_tmn$rsq_hli_tmn, col = 'darkgreen')
legend('topleft', legend = c('Elevation', 'TCI', 'HLI'),
       col = c('black', 'blue', 'darkgreen'), lwd = 1.25)

# plot( tmn$tair, tmn[ , 2], pch = 16, cex = 0.5, 
#       xlim = c(-30, 25), ylim = c(-30, 25))
# for(i in 3:41){
#   points(tmn$tair, tmn[ , i], pch = 16, cex = 0.5)
# }

######################################################
# Work on tmx (maximum temperature) second
######################################################
#### Linear model of site's temperature by reanalysis T
## Make a function to do the work
mods_fun = function(i) {
  ys = names(tmx)
  
  temp.df <- data.frame(tmx[ , ys[i]], tmx$tair)
  names(temp.df) = c(ys[i], 'tair')
  
  return(lm(temp.df[ , 1] ~ temp.df$tair, na.action = na.omit))
}

## Apply the function to columns 2:41 and out_tmxput a list
mod_list = lapply(2:41, mods_fun)

names = names(tmx)[c(-1, -42:-48)]
names(mod_list) = names

## Get the summaries
mod_sums = lapply(mod_list, summary)
names(mod_sums) = names

## Get the residuals
mod_resids = lapply(mod_list, resid)
names(mod_resids) = names

#### Make a function to match the residuals with dates
for(i in 1:length(mod_resids)){
  col = which(names(tmx) == names(mod_resids[i]))
  
  w = which(!is.na(tmx[, col]))
  
  df = data.frame(w, mod_resids[[i]])
  names(df) = c('index', names(mod_resids[i]))
  
  out_tmx = merge(out_tmx, df, all.x = TRUE)
  rm(list = c('df', 'w', 'col'))
}

#### Write out_tmx the out_tmx data.frame
out_tmx = out_tmx[ , -which(names(out_tmx) == 'index')]
write.csv(out_tmx, './MRI/tmx_resids.csv', row.names = FALSE)

#### run the model on each day with tci
df = data.frame('id_snake' = snake$ID, 'id_list' = names(out_tmx)[-1])
df$id_snake == df$id_list
rm(df)

get_tci = function(i){
  v = as.vector(as.matrix(out_tmx[i, -1]))
  
  return(lm(v ~ tci_v))
}

## make a vector of tci to rinse and reuse
tci_v = snake$tci

## make the models and store as lists
tci_mods = lapply(1:nrow(out_tmx), get_tci)
tci_sums = lapply(tci_mods, summary)
rsq_tci = sapply(1:length(tci_mods), function(i) tci_sums[[i]]$r.squared)

#### add month to the out_tmx data.frame and aggreate by month
out_tmx$month = strftime(out_tmx$date, '%m')
out_tmx$year = strftime(out_tmx$date, '%Y')
out_tmx$agg_date = as.Date(paste(out_tmx$year, out_tmx$month, '01', sep = '-'))
out_tmx$tci_rsq = rsq_tci

agg_mean_tmx = aggregate(rsq_tci ~ agg_date, out_tmx, mean)
plot(agg_mean_tmx$agg_date, agg_mean_tmx$rsq_tci, type = 'l',
     ylim = c(0, .5))



#### run the model on each day with elevation
## make a vector of elev to rinse and reuse
elev = snake$elev

get_elev = function(i){
  v = as.vector(as.matrix(out_tmx[i, 2:41]))
  
  return(lm(v ~ elev))
}

## make the models and store as lists
elev_mods = lapply(1:nrow(out_tmx), get_elev)
elev_sums = lapply(elev_mods, summary)
rsq_elev = sapply(1:length(elev_sums), function(i) elev_sums[[i]]$r.squared)

#### add month to the out_tmx data.frame and aggreate by month
out_tmx$tci_rsq = rsq_elev

# agg_mean_tmx = aggregate(cbind(rsq_tci, rsq_elev) ~ month, out_tmx, mean)
# plot(agg_mean_tmx$month, agg_mean_tmx$rsq_elev_tmx, type = 'l')
# lines(agg_mean_tmx$month, agg_mean_tmx$rsq_tci_tmx, col = 2)

#### run the model on each day with elevation
## make a vector of elev to rinse and reuse
hli = snake$hli

get_hli = function(i){
  v = as.vector(as.matrix(out_tmx[i, 2:41]))
  
  return(lm(v ~ hli))
}

## make the models and store as lists
hli_mods = lapply(1:nrow(out_tmx), get_hli)
hli_sums = lapply(hli_mods, summary)
rsq_hli = sapply(1:length(hli_sums), function(i) hli_sums[[i]]$r.squared)

#### add month to the out_tmx data.frame and aggreate by month
out_tmx$tci_rsq = rsq_hli

agg_mean_tmx = aggregate(cbind(rsq_tci, rsq_elev, rsq_hli) ~ agg_date, out_tmx, mean)
names(agg_mean_tmx) = c('agg_date', 'rsq_tci_tmx', 'rsq_elev_tmx', 'rsq_hli_tmx')
plot(agg_mean_tmx$agg_date, agg_mean_tmx$rsq_elev_tmx, type = 'l', 
     ylim = (c(0,1)), xlab = 'R squared', ylab = 'agg_date')
lines(agg_mean_tmx$agg_date, agg_mean_tmx$rsq_tci_tmx, col = 'blue')
lines(agg_mean_tmx$agg_date, agg_mean_tmx$rsq_hli_tmx, col = 'darkgreen')
legend('topleft', legend = c('Elevation', 'TCI', 'HLI'),
       col = c('black', 'blue', 'darkgreen'), lwd = 1.25)

# plot( tmx$tair, tmx[ , 2], pch = 16, cex = 0.5, 
#       xlim = c(-30, 25), ylim = c(-30, 25))
# for(i in 3:41){
#   points(tmx$tair, tmx[ , i], pch = 16, cex = 0.5)
# }

######################################################
# Work on tav (average temperature) third
######################################################
#### Linear model of site's temperature by reanalysis T
## Make a function to do the work
mods_fun = function(i) {
  ys = names(tav)
  
  temp.df <- data.frame(tav[ , ys[i]], tav$tair)
  names(temp.df) = c(ys[i], 'tair')
  
  return(lm(temp.df[ , 1] ~ temp.df$tair, na.action = na.omit))
}

## Apply the function to columns 2:41 and out_tavput a list
mod_list = lapply(2:41, mods_fun)

names = names(tav)[c(-1, -42:-48)]
names(mod_list) = names

## Get the summaries
mod_sums = lapply(mod_list, summary)
names(mod_sums) = names

## Get the residuals
mod_resids = lapply(mod_list, resid)
names(mod_resids) = names

#### Make a function to match the residuals with dates
for(i in 1:length(mod_resids)){
  col = which(names(tav) == names(mod_resids[i]))
  
  w = which(!is.na(tav[, col]))
  
  df = data.frame(w, mod_resids[[i]])
  names(df) = c('index', names(mod_resids[i]))
  
  out_tav = merge(out_tav, df, all.x = TRUE)
  rm(list = c('df', 'w', 'col'))
}

#### Write out_tav the out_tav data.frame
out_tav = out_tav[ , -which(names(out_tav) == 'index')]
write.csv(out_tav, './MRI/tav_resids.csv', row.names = FALSE)

#### run the model on each day with tci
df = data.frame('id_snake' = snake$ID, 'id_list' = names(out_tav)[-1])
df$id_snake == df$id_list
rm(df)

get_tci = function(i){
  v = as.vector(as.matrix(out_tav[i, -1]))
  
  return(lm(v ~ tci_v))
}

## make a vector of tci to rinse and reuse
tci_v = snake$tci

## make the models and store as lists
tci_mods = lapply(1:nrow(out_tav), get_tci)
tci_sums = lapply(tci_mods, summary)
rsq_tci = sapply(1:length(tci_mods), function(i) tci_sums[[i]]$r.squared)

#### add month to the out_tav data.frame and aggreate by month
out_tav$month = strftime(out_tav$date, '%m')
out_tav$year = strftime(out_tav$date, '%Y')
out_tav$agg_date = as.Date(paste(out_tav$year, out_tav$month, '01', sep = '-'))
out_tav$tci_rsq = rsq_tci

agg_mean_tav = aggregate(rsq_tci ~ agg_date, out_tav, mean)
plot(agg_mean_tav$agg_date, agg_mean_tav$rsq_tci, type = 'l',
     ylim = c(0, .5))



#### run the model on each day with elevation
## make a vector of elev to rinse and reuse
elev = snake$elev

get_elev = function(i){
  v = as.vector(as.matrix(out_tav[i, 2:41]))
  
  return(lm(v ~ elev))
}

## make the models and store as lists
elev_mods = lapply(1:nrow(out_tav), get_elev)
elev_sums = lapply(elev_mods, summary)
rsq_elev = sapply(1:length(elev_sums), function(i) elev_sums[[i]]$r.squared)

#### add month to the out_tav data.frame and aggreate by month
out_tav$tci_rsq = rsq_elev

# agg_mean_tav = aggregate(cbind(rsq_tci, rsq_elev) ~ month, out_tav, mean)
# plot(agg_mean_tav$month, agg_mean_tav$rsq_elev_tav, type = 'l')
# lines(agg_mean_tav$month, agg_mean_tav$rsq_tci_tav, col = 2)

#### run the model on each day with elevation
## make a vector of elev to rinse and reuse
hli = snake$hli

get_hli = function(i){
  v = as.vector(as.matrix(out_tav[i, 2:41]))
  
  return(lm(v ~ hli))
}

## make the models and store as lists
hli_mods = lapply(1:nrow(out_tav), get_hli)
hli_sums = lapply(hli_mods, summary)
rsq_hli = sapply(1:length(hli_sums), function(i) hli_sums[[i]]$r.squared)

#### add month to the out_tav data.frame and aggreate by month
out_tav$tci_rsq = rsq_hli

agg_mean_tav = aggregate(cbind(rsq_tci, rsq_elev, rsq_hli) ~ agg_date, out_tav, mean)
names(agg_mean_tav) = c('agg_date', 'rsq_tci_tav', 'rsq_elev_tav', 'rsq_hli_tav')
plot(agg_mean_tav$agg_date, agg_mean_tav$rsq_elev_tav, type = 'l', 
     ylim = (c(0,1)), xlab = 'R squared', ylab = 'agg_date')
lines(agg_mean_tav$agg_date, agg_mean_tav$rsq_tci_tav, col = 'blue')
lines(agg_mean_tav$agg_date, agg_mean_tav$rsq_hli_tav, col = 'darkgreen')
legend('topleft', legend = c('Elevation', 'TCI', 'HLI'),
       col = c('black', 'blue', 'darkgreen'), lwd = 1.25)

# plot( tav$tair, tav[ , 2], pch = 16, cex = 0.5, 
#       xlim = c(-30, 25), ylim = c(-30, 25))
# for(i in 3:41){
#   points(tav$tair, tav[ , i], pch = 16, cex = 0.5)
# }

agg_mean = merge(agg_mean_tmn, agg_mean_tmx)
agg_mean = merge(agg_mean, agg_mean_tav)


######## PLOT
#### tmx plot
par(mfrow=c(1,3), mar = c(7, 5, 4, 2) + 0.1)
date_labs = c(strftime(agg_mean$agg_date, '%Y-%m')[1],
              strftime(agg_mean$agg_date, '%m')[2:7],
              strftime(agg_mean$agg_date, '%Y-%m')[8],
              strftime(agg_mean$agg_date, '%m')[9:13])
plot(agg_mean$agg_date, agg_mean$rsq_elev_tmx, pch = 18,
     col = 'black', xlab = '', ylab = 'R Squared', 
     cex = 2, cex.lab = 1.9, cex.axis = 1.7, ylim = c(0,0.8),
     xaxt = 'n', main = 'Maximum Temperature', cex.main = 1.9)
axis(side = 1, at = agg_mean$agg_date, labels = FALSE)
text(agg_mean$agg_date, par("usr")[3] - 0.03,
     labels = date_labs,
     srt = 45, pos = 1, xpd = TRUE, cex = 1.5)
title(xlab = 'Month', cex.lab = 1.9, line = 5)
points(agg_mean$agg_date, agg_mean$rsq_tci_tmx, pch = 18,
       col = 'blue', cex = 2)
points(agg_mean$agg_date, agg_mean$rsq_hli_tmx, pch = 18,
       col = 'red', cex = 2)
legend('topleft', legend = c('Elevation', 'TCI', 'HLI'),
       pch = 18, col = c('black', 'blue', 'red'),
       cex = 2)

######## PLOT
#### tmn plot
plot(agg_mean$agg_date, agg_mean$rsq_elev_tmn, pch = 17,
     col = 'black', xlab = '', ylab = 'R Squared', 
     cex = 1.7, cex.lab = 1.9, cex.axis = 1.7, ylim = c(0,0.8),
     xaxt = 'n', main = 'Minimum Temperature', cex.main = 1.9)
axis(side = 1, at = agg_mean$agg_date, labels = FALSE)
text(agg_mean$agg_date, par("usr")[3] - 0.03,
     labels = date_labs,
     srt = 45, pos = 1, xpd = TRUE, cex = 1.5)
title(xlab = 'Month', cex.lab = 1.9, line = 5)
points(agg_mean$agg_date, agg_mean$rsq_tci_tmn, pch = 17,
       col = 'blue', cex = 1.7)
points(agg_mean$agg_date, agg_mean$rsq_hli_tmn, pch = 17,
       col = 'red', cex = 1.7)
legend('topleft', legend = c('Elevation', 'TCI', 'HLI'),
       pch = 17, col = c('black', 'blue', 'red'),
       cex = 1.75)

######## PLOT
#### tav plot
plot(agg_mean$agg_date, agg_mean$rsq_elev_tav, pch = 15,
     col = 'black', xlab = '', ylab = 'R Squared', 
     cex = 1.7, cex.lab = 1.9, cex.axis = 1.7, ylim = c(0,0.8),
     xaxt = 'n', main = 'Mean Temperature', cex.main = 1.9)
axis(side = 1, at = agg_mean$agg_date, labels = FALSE)
text(agg_mean$agg_date, par("usr")[3] - 0.03,
     labels = date_labs,
     srt = 45, pos = 1, xpd = TRUE, cex = 1.5)
title(xlab = 'Month', cex.lab = 1.9, line = 5)
points(agg_mean$agg_date, agg_mean$rsq_tci_tav, pch = 15,
       col = 'blue', cex = 1.7)
points(agg_mean$agg_date, agg_mean$rsq_hli_tav, pch = 15,
       col = 'red', cex = 1.7)
legend('topleft', legend = c('Elevation', 'TCI', 'HLI'),
       pch = 15, col = c('black', 'blue', 'red'),
       cex = 1.75)





####### plot of original regression ########
#### TMx
par(mfrow=c(1,3), mar = c(5, 5, 4, 2) + 0.1, oma = c(0,0,3,0))
#png('./MRI/Figures/tmx_regression.png', height = 10, width = 30,
#    unit = 'in', res=300)
plot(tmx$tair, tmx$D1P01, xlim = c(-25, 25), ylim = c(-25, 45),
     cex.axis = 1.7, cex.lab = 1.9, xlab = 'Free Air Temperature (°C)', 
     ylab = 'Maximum Temperature (°C)')
for(i in 3:41){
  points(tmx$tair, tmx[,i])
}
max_mod_df = data.frame('tair' = tmx$tair, 'tmx' = tmx$D1P01)
for(i in 3:41){
  max_mod_df = rbind(max_mod_df, data.frame('tair' = tmx$tair, 
                                            'tmx' = tmx[ , i]))
}
tmx_lm = lm(tmx ~ tair, max_mod_df)
abline(tmx_lm, col = 'red', lwd = 2)
abline(0, 1, lty = 2, lwd = 2, col = 'blue')
legend('topleft', legend = c('Linear Regression', '1:1 line'),
       col = c('red', 'blue'), lty = 1:2, lwd = 2, cex = 1.8)



#### tmn
plot(tmn$tair, tmn$D1P01, xlim = c(-31, 25), ylim = c(-31, 25),
     cex.axis = 1.7, cex.lab = 1.9, xlab = 'Free Air Temperature (°C)', 
     ylab = 'Minimum Temperature (°C)')
for(i in 3:41){
  points(tmn$tair, tmn[,i])
}
min_mod_df = data.frame('tair' = tmn$tair, 'tmn' = tmn$D1P01)
for(i in 3:41){
  min_mod_df = rbind(min_mod_df, data.frame('tair' = tmn$tair, 
                                            'tmn' = tmn[ , i]))
}
tmn_lm = lm(tmn ~ tair, min_mod_df)
abline(tmn_lm, col = 'red', lwd = 2)
abline(0, 1, lty = 2, lwd = 2, col = 'blue')
legend('topleft', legend = c('Linear Regression', '1:1 line'),
       col = c('red', 'blue'), lty = 1:2, lwd = 2, cex = 1.8)

mtext('Free Air Temperature vs Site Specific Temperature', cex = 2,
      line = 2)
#### tav
plot(tav$tair, tav$D1P01, xlim = c(-25, 25), ylim = c(-25, 30),
     cex.axis = 1.7, cex.lab = 1.9, xlab = 'Free Air Temperature (°C)', 
     ylab = 'Mean Temperature (°C)')
for(i in 3:41){
  points(tav$tair, tav[,i])
}
av_mod_df = data.frame('tair' = tav$tair, 'tav' = tav$D1P01)
for(i in 3:41){
  av_mod_df = rbind(av_mod_df, data.frame('tair' = tav$tair, 
                                            'tav' = tav[ , i]))
}
tav_lm = lm(tav ~ tair, av_mod_df)
abline(tav_lm, col = 'red', lwd = 2)
abline(0, 1, lty = 2, lwd = 2, col = 'blue')
legend('topleft', legend = c('Linear Regression', '1:1 line'),
       col = c('red', 'blue'), lty = 1:2, lwd = 2, cex = 1.8)


#dev.off()
