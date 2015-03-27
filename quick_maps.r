### Quick maps

library(raster)
library(rgdal)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

tci = raster('./GIS-Data/TCI_Whole-Range_UTM.tif')
hli = raster('./GIS-Data/hli_utm.tif')
snake = read.csv('./Site-Data/snake_july-2014.csv', 
                 header = TRUE, as.is = 1)
coordinates(snake) = ~x_utm+y_utm
proj4string(snake) = CRS(projection(snake))

tci <- crop(tci, hli)

plot(hli)
plot(tci)


par(mar = c(5, 5, 4, 2), mfrow = c(1,1))
plot(tci, main = "Terrain Convergence Index (TCI)", xlab = "Easting",
     ylab = "Northing", cex.lab = 1.9, cex.axis = 1.5,
     cex.main = 2)
plot(snake, add = TRUE, cex = 1.5)
legend('topleft', legend = 'Sensor Locations', cex = 1.5, pch = 3)

plot(hli, main = "Heat Load Index (HLI)", xlab = "Easting",
     ylab = "Northing", cex.lab = 1.9, cex.axis = 1.5,
     cex.main = 2)
plot(snake, add = TRUE)
legend('topleft', legend = 'Sensor Locations', cex = 1.5, pch = 3)