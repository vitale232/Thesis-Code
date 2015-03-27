#--------------------------------
# Name:         EOF.R
# Purpose:      Calculate EOFs for the SLP anomoly data
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/10/13
# R:            3.1.1
#--------------------------------

#### you must run this script in the console and open
#### the console with the following command:
#### R --max-ppsize 500000

library(raster)
library(rgdal)
library(fields)
library(ncdf4)
library(spacetime)
library(devtools)
library(xts)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

#### Set up the extent I want to work in
e = extent(-180, -75, 15, 90)

# #### Read in the file list
# slp_files = list.files('./Reanalysis/SLP/', pattern='.nc$', full.names=TRUE)
# 
# #### Correct for the time issues.  Origin of the time files
# #### is '1-1-1 00:00:00', but there is a 48 hour error in R's
# #### interpretation of the times.  This error is due to the origin
# #### occuring prior to the Gregorian Calendar change.
# time = NULL
# for(file in slp_files){
#   time = c(time, ncvar_get(nc_open(file), varid='time'))
# }
# time = (time - 48) * 3600 # convert to seconds for R
# time = as.POSIXct(time, origin='1-1-1 00:00:00', tz='GMT')
# 
# #### Read in the NetCDF files as a raster stack
# slp = stack(slp_files)
# names(slp) = paste0('x', time)
# slp = setZ(slp, time)
# 
# #### Rotate to -180, 180 longitude
# slp = rotate(slp)
# 
# #### Crop to the extent of interest
# slp = crop(slp, e)
# 
# #### Reproject the data
# slp = projectRaster(slp, crs='+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
# e2 = extent(-5016183, -127126.6, -1294725, 3178209)
# slp = crop(slp, e2)
# 
# #### Convert to hPa
# slp = slp/100
# 
# #### Calculate the anomolies using the entire period as baseline
# climo = mean(slp)
# anoms = slp - climo
# anoms = setZ(anoms, time)
# names(anoms) = paste0('x', time)


#-----------------------------------------------------------------------------
#### Load in the anoms that were calculated using the code above
anoms = brick('./Reanalysis/SLP_Anoms/slp_anoms.grd')
anoms = setZ(anoms, as.Date(getZ(anoms)))

dev_mode(on=TRUE)
install_github('edzer/spacetime')
stfdf = as(anoms, 'STFDF')

#### EOF HAS BEEN DEPRECATED BY EDZER ####
# seof_preds_scale = EOF(stfdf, 'spatial', returnPredictions=TRUE, scale.=TRUE)
# seof_scale = EOF(stfdf, 'spatial', returnPredictions=FALSE, scale.=TRUE)

eof = eof(stfdf)
eof_summ = eof(stfdf, returnEOFs=FALSE)
peof = predict(eof_summ)
teof = data.frame('date'=as.Date(row.names(peof)),
                  peof)

# PCs = apply(seof_scale$rotation, 2, function(x) (x - mean(x)) / sd(x))
# EOFs = stack(seof_preds_scale)

# save.image('~/Dropbox/UNR/UNR-Thesis/Data/Modeling_R-Images/2014-10-30_EOF-scaled.RData')

load('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Modeling_R-Images/2014-10-30_EOF-scaled.RData')

source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')
vars = eof_summ$sdev^2
vars = vars/sum(vars)
write.csv(rbind('Stand. Dev.' = eof_summ$sdev, 'Prop. Var.' = vars, 'Cum. Prop.' = cumsum(vars)),
          file='./Validation/EOF-summary.csv')
PCs = with(teof, {
  data.frame(date, PC1, PC2, PC3, PC4, PC5)})
EOFs = stack(eof)

d = sort(unique(m_tmn$date))
w = which(getZ(anoms) %in% d)

df = read.csv('./Aggregated_Data/m_tmn_lapse.csv')
df$date = as.Date(df$date)
df$cuts = cut(df$m_tmn_lapse, 5)
df$cols = as.numeric(df$cuts)

dat = read.csv('./Aggregated_Data/m_tmx_lapse.csv')
dat$date = as.Date(dat$date)
df = merge(df, dat)

x11(height=10.25, width=20)
# png('~/Dropbox/UNR/UNR-Thesis/Figures/EOF_PC-ts.png',
#     height=10.25, width=20, units='in', res=300)
colors = c('red', 'orange', 'green', 'blue', 'violet')
par(mfrow=c(4,1), mar=c(2, 4, 1, 2) + 0.1)
for(i in 2:5){
  plot(d, PCs[w, i], type='l', main=paste0('EOF', i-1),
       ylab=paste(paste0('EOF', i-1), 'weight'))
  points(d, PCs[w, i], col=colors[df$cols], pch=1)
  abline(0, 0, lty=2)
  if(i == 5) {
    legend('bottomleft', legend=gsub('\\]', '', gsub('\\(', '', levels(df$cut))),
           pch=1, col=colors)
  }
}
# dev.off()

states = readOGR(dsn='/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/GIS-Data/states_21basic',
                 layer='states')
canada = readOGR(dsn='/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/GIS-Data/Canada',
                 layer='Canada')

states = spTransform(states, CRS(projection(EOFs)))
canada = spTransform(canada, CRS(projection(EOFs)))

x11(height=9.9, width=10)
# png(filename='~/Dropbox/UNR/UNR-Thesis/Figures/EOF_R-eof-4.png',
#     height=10, width=10.5, res=300, units='in')
colors = colorRampPalette(c('darkblue', 'blue', 'white', 'red', 'darkred'))
par(mfrow=c(2,2), mar= c(5, 2, 4, 6) + 0.1)
for(i in 1:4){
  plot(EOFs, i, col=colors(255), axes=FALSE)
  plot(states, add=TRUE, border='grey48')
  plot(canada, add=TRUE, border='grey48')
  contour(EOFs[[i]], add=TRUE)
}
# dev.off()

d2 = seq(as.Date('2013-06-17'), as.Date('2014-06-24'),
         by='day')
w2 = which(getZ(anoms) %in% d2)

x11(height=10.25, width=20)
# png('~/Dropbox/UNR/UNR-Thesis/Figures/EOF_PC-ts-mine-1.png',
#     height=10.25, width=20, units='in', res=300)
par(mfrow=c(4,1), mar=c(2, 4, 1, 2) + 0.1)
for(i in 1:4){
  plot(d2, PCs[w2, i], type='l', main=paste0('EOF', i),
       ylab=paste(paste0('EOF', i), 'weight'))
  abline(0, 0, lty=2)
}
# dev.off()

df$PC1 = PCs[w,'PC1']
df$PC2 = PCs[w,'PC2']
df$PC3 = PCs[w,'PC3']
df$PC4 = PCs[w,'PC4']
df$PC5 = PCs[w,'PC5']
df$PC6 = PCs[w,'PC6']

write.csv(df, './Reanalysis/EOF_PCs-time-series.csv', row.names=FALSE)

# png('~/Dropbox/UNR/UNR-Thesis/Figures/EOF_tmn-lapse.png', height=7, width=14, units='in', res=300)
colors = c('red', 'orange', 'green', 'blue', 'violet')
plot(df$date, df$PC4, type='l', xlab='Date', ylab='PC4', main='Tmn lapse rates (°C/km) and PC4')
points(df$date, df$PC4, pch=1, col=colors[df$cols])
abline(0,0, lty=2)
legend('topleft', pch=1, col=colors[1:length(unique(df$cols))], 
       legend=levels(df$cuts))
# dev.off()

setEPS(height=7, width=12)
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/lapse-rates_tmn-PC4.eps')
start = as.Date('2013-06-17')
end = as.Date('2014-06-24')
colors = c('red', 'orange', 'green', 'blue', 'violet')
with(df[df$date >= start & df$date <= end, ], {
  plot(date, PC4, type='l', ylim=c(-170, 170),
       main='Tmn lapse rates (°C/kim) and PC4', lwd=1.1)
  points(date, PC4, pch=1, col=colors[cols])
  abline(0, 0, lty=2)
  legend('topleft', pch=1, col=colors[1:length(unique(df$cols))], 
         legend=levels(df$cuts))
})
dev.off()


setEPS(height=7, width=12)
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/lapse-rates_tmx-PC4.eps')
with(df[df$date >= start & df$date <= end, ], {
  plot(date, PC4, type='l', ylim=c(-170, 170),
       main='Tmx lapse rates (°C/kim) and PC4')
  points(date, PC4, pch=1, col=colors[tmx_cols])
  abline(0, 0, lty=2)
  legend('topleft', pch=1, col=colors[1:length(unique(df$tmx_cols))], 
         legend=levels(df$tmx_cuts))
})
dev.off()