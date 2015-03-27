#--------------------------------
# Name:         SOM_reproject_multiple.R
# Purpose:      Prep Reanalysis SLP data for SOM, reproject it, and run the SOM
#               with grids of varying sizes.
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/09/30
# R:            3.1.1
#--------------------------------

library(raster)
library(rgdal)
library(fields)
library(ncdf4)
library(kohonen)

plot_the_maps = TRUE

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

anoms = brick('./Reanalysis/SLP_Anoms/slp_anoms.grd')

#### Set up an empty raster from the anoms metadata to capture cell size etc.
r = raster(anoms)

#### Create a matrix of the anomolies
m = t(as.matrix(anoms))

#### Run the SOM on the SLP anomoly data
set.seed(67728)
slp_som_7x5 = som(m, grid=somgrid(xdim=7, ydim=5, topo='rectangular'), 
                  n.hood='square', keep.data=FALSE)

#### Extract the code values and create a raster stack
l = list()
for(i in 1:nrow(slp_som_7x5$codes)){
  l[[i]] = setValues(r, slp_som_7x5$codes[i, ])
}
codes_7x5 = stack(l)

#### If plot_the_maps flag is on, plot the maps using the SOM_Plot-helper script
if(plot_the_maps){
  contours = TRUE
  equidistant_projection = TRUE
  save_as_png = TRUE
  png_filename = '~/Dropbox/UNR/UNR-Thesis/Figures/SOM_7x5_Codes.png'
  layout(matrix(c(1:35, rep(36, 7)), ncol=7, byrow=T))
  layout.show(36)
  brks <- seq(-34, 34, by = 1)
  nb <- length(brks) - 1
  cols <-  colorRampPalette(c('darkblue', 'blue', 'white', 'red', 'darkred'))
  ticks <- seq(-34, 34, by = 2)
  z = c(-34, 34)
  codes = codes_7x5
  source('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/SOM_Plot-helper.R')
}

#### Plot the training process
x11()
plot(slp_som_7x5, type='changes', 'SLP SOM Training Process', 
     cex.axis=1.2, cex.lab=1.2, lwd=1.5)


#### Subset the anoms for the dates of my time series
start_date = as.Date('2013-06-17')
end_date = as.Date('2014-06-24')

dates = as.Date(getZ(anoms))
which_layers = which(dates >= start_date & dates <= end_date)

these_anoms = anoms[[which_layers]]

#### coerce to matrix and map to slp_som
map_m = t(as.matrix(these_anoms))

mapped_7x5 = map.kohonen(slp_som_7x5, map_m)

dates_mapped = data.frame('date'=dates[which_layers],
                          'unit_7x5'=mapped_7x5$unit.classif,
                          'dist_7x5'=mapped_7x5$distances)







slp_som_5x5 = som(m, grid=somgrid(xdim=5, ydim=5, topo='rectangular'), 
                  n.hood='square', keep.data=FALSE)

#### Extract the code values and create a raster stack
l = list()
for(i in 1:nrow(slp_som_5x5$codes)){
  l[[i]] = setValues(r, slp_som_5x5$codes[i, ])
}
codes_5x5 = stack(l)

#### If plot_the_maps flag is on, plot the maps using the SOM_Plot-helper script
if(plot_the_maps){
  contours = TRUE
  equidistant_projection = TRUE
  save_as_png = TRUE
  png_filename = '~/Dropbox/UNR/UNR-Thesis/Figures/SOM_5x5_Codes.png'
  if(save_as_png){
    png(png_filename, width=15, height=10, unit='in', res=300)}else{
      x11(width=15, height=10)
    }
  layout(matrix(c(1:25, rep(26, 5)), ncol=5, byrow=T))
  layout.show(26)
  brks <- seq(-32, 32, by = 1)
  nb <- length(brks) - 1
  cols <-  colorRampPalette(c('darkblue', 'blue', 'white', 'red', 'darkred'))
  ticks <- seq(-32, 32, by = 2)
  z = c(-32, 32)
  codes = codes_5x5
  source('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/SOM_Plot-helper.R')
}

#### Plot the training process
x11()
plot(slp_som_5x5, type='changes', 'SLP SOM Training Process', 
     cex.axis=1.2, cex.lab=1.2, lwd=1.5)

mapped_5x5 = map.kohonen(slp_som_5x5, map_m)

dates_mapped$unit_5x5 = mapped_5x5$unit.classif
dates_mapped$dist_5x5 = mapped_5x5$distances





slp_som_4x4 = som(m, grid=somgrid(xdim=4, ydim=4, topo='rectangular'), 
                  n.hood='square', keep.data=FALSE)

#### Extract the code values and create a raster stack
l = list()
for(i in 1:nrow(slp_som_4x4$codes)){
  l[[i]] = setValues(r, slp_som_4x4$codes[i, ])
}
codes_4x4 = stack(l)

#### If plot_the_maps flag is on, plot the maps using the SOM_Plot-helper script
if(plot_the_maps){
  contours = TRUE
  equidistant_projection = TRUE
  save_as_png = TRUE
  png_filename = '~/Dropbox/UNR/UNR-Thesis/Figures/SOM_4x4_Codes.png'
  if(save_as_png){
    png(png_filename, width=15, height=10, unit='in', res=300)}else{
      x11(width=15, height=10)
    }
  layout(matrix(c(1:16, rep(17, 4)), ncol=4, byrow=T))
  layout.show(17)
  brks <- seq(-30, 30, by = 1)
  nb <- length(brks) - 1
  cols <-  colorRampPalette(c('darkblue', 'blue', 'white', 'red', 'darkred'))
  ticks <- seq(-30, 30, by = 2)
  z = c(-30, 30)
  codes = codes_4x4
  source('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/SOM_Plot-helper.R')
}

#### Plot the training process
x11()
plot(slp_som_4x4, type='changes', 'SLP SOM Training Process', 
     cex.axis=1.2, cex.lab=1.2, lwd=1.5)

mapped_4x4 = map.kohonen(slp_som_4x4, map_m)

dates_mapped$unit_4x4 = mapped_4x4$unit.classif
dates_mapped$dist_4x4 = mapped_4x4$distances

write.csv(dates_mapped, './Reanalysis/SOM-Data.csv', row.names=FALSE)