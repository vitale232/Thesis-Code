#--------------------------------
# Name:         extract_irradiance.R
# Purpose:      Extract irradiance data for Snake Range sites
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/10/07
# R:            3.1.1
#--------------------------------

library(raster)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

snake = read.csv('./Site-Data/snake3.csv')
coordinates(snake) = ~x_utm+y_utm

m_tmx = read.csv('./Aggregated_Data/m-tmx.csv')
m_tmx$date = as.Date(m_tmx$date)
dates = m_tmx$date
days = as.numeric(strftime(dates, '%j'))

lf = list.files('./GIS-Data/Irradiance/', full.names=TRUE,
                pattern='irradiance_[[:digit:]]{3}.tif')

s = stack(lf)

s2 = s[[days]]

e = extract(s2, snake, sp=TRUE)

df = as.data.frame(e)
df2 = df[ , c(-1:-2, -4:-34)]

m = t(df2[ , -1])
colnames(m) = df2[ , 1]

mdf = as.data.frame(m)

out = data.frame('date'=dates, 'days'=days,
                 mdf)

write.csv(out, './Site-Data/irradiance.csv', row.names=FALSE)
