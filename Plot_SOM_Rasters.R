library(fields)
library(raster)
library(rgdal)

contours = TRUE
equidistant_projection = TRUE
save_as_png = TRUE
png_filename = '~/Google Drive/UNR/UNR-Thesis/Figures/equi-dist_SOM.png'


if(save_as_png){
  png(png_filename, width=15, height=10, unit='in', res=300)}else{
    x11(width=15, height=10)
  }

layout(matrix(c(1:20, rep(20, 5)), ncol=5, byrow=T))
layout.show(26)
###Precip
brks <- seq(-34, 34, by = 1)
nb <- length(brks) - 1
cols <-  colorRampPalette(c('darkblue', 'blue', 'white', 'red', 'darkred'))
ticks <- seq(-34, 34, by = 2)
z = c(-34, 34)

states = readOGR(dsn='/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/GIS-Data/states_21basic',
                 layer='states')
canada = readOGR(dsn='/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/GIS-Data/Canada',
                 layer='Canada')

states = spTransform(states, CRS(projection(codes)))
canada = spTransform(canada, CRS(projection(codes)))

mains = 1:nlayers(codes)
par(mar=c(0, 0, 1, 0) + 0.3)
for(i in 1:nlayers(codes)){
  if(i %in% c(1, 8, 15, 22)){
  
  image(codes[[i]], col = cols(nb), xlab = '', ylab = '', 
          breaks = brks, axes=FALSE, zlim=z, main=mains[i])
  if(equidistant_projection==FALSE) axis(2, cex.axis=1.4)
  plot(states, add=TRUE, border='gray29')
  plot(canada, add=TRUE, border='gray29')
  if(contours) contour(codes[[i]], add=TRUE)
  }
  
  if(i %in% 30:35){
    image(codes[[i]], col = cols(nb), xlab = '', ylab = '',
          breaks = brks, axes=FALSE, zlim=z, main=mains[i])
    if(equidistant_projection==FALSE) axis(1, cex.axis=1.4)
  plot(states, add=TRUE, border='gray29')
  plot(canada, add=TRUE, border='gray29')
  if(contours) contour(codes[[i]], add=TRUE)
  }
  
  if(i %in% c( c(1, 8, 15, 22), 29:35) == FALSE){
    image(codes[[i]], col=cols(nb), xlab='', ylab='',
          breaks=brks, axes=FALSE, zlim=z, main=mains[i])
    plot(states, add=TRUE, border='gray29')
    plot(canada, add=TRUE, border='gray29')
    if(contours) contour(codes[[i]], add=TRUE)
  }  
  
  if(i == 29){
    image(codes[[i]], col=cols(nb), xlab='', ylab='',
          breaks=brks, axes=FALSE, zlim=z, main=mains[i])
    if(equidistant_projection==FALSE) axis(1, cex.axis=1.4)
    if(equidistant_projection==FALSE) axis(2, cex.axis=1.4)
    plot(states, add=TRUE, border='gray29')
    plot(canada, add=TRUE, border='gray29')
    if(contours) contour(codes[[i]], add=TRUE)
  }
}  




par(mar=c(4, 4, 3, 2) + 0.1)
image(x=matrix(brks), y=matrix(c(0,1)), z=matrix(brks), col=cols(nb), zlim=z,
      axes=FALSE, xlab='', ylab='', bigplot=c(0, 1, 0.5, 1))#,

axis(1, at=ticks, labels=ticks, ticks=TRUE, lwd=1.25,
     cex.axis=1.5)
mtext('hPa', side=1, outer=TRUE, line=-1.5, cex=1.3)


if(save_as_png) dev.off()

