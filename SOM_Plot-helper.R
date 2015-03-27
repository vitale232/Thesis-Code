library(fields)
library(raster)
library(rgdal)

states = readOGR(dsn='/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/GIS-Data/states_21basic',
                 layer='states')
canada = readOGR(dsn='/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/GIS-Data/Canada',
                 layer='Canada')

states = spTransform(states, CRS(projection(codes)))
canada = spTransform(canada, CRS(projection(codes)))

mains = 1:nlayers(codes)
par(mar=c(0, 0, 1, 0) + 0.3)
for(i in 1:nlayers(codes)){
  image(codes[[i]], col = cols(nb), xlab = '', ylab = '', 
        breaks = brks, axes=FALSE, zlim=z, main=mains[i])
  plot(states, add=TRUE, border='gray20')
  plot(canada, add=TRUE, border='gray20')
  if(contours) contour(codes[[i]], add=TRUE)
}

par(mar=c(4, 4, 3, 2) + 0.1)
image(x=matrix(brks), y=matrix(c(0,1)), z=matrix(brks), col=cols(nb), zlim=z,
      axes=FALSE, xlab='', ylab='', bigplot=c(0, 1, 0.5, 1))#,

axis(1, at=ticks, labels=ticks, ticks=TRUE, lwd=1.25,
     cex.axis=1.5)
mtext('hPa', side=1, outer=TRUE, line=-1.5, cex=1.3)


if(save_as_png) dev.off()

