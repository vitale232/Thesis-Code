##############################################################
############### Extract Reanalysis2 Data #####################
##############################################################
## Andrew Vitale
## 2014 March 25

library(tools)
library(rgdal)
library(gstat)
library(raster)
library(ncdf4)
library(spacetime)
library(maps)
library(maptools)

# vetor of min and max dates I want (one day before sensor
# record begins to look at lag)
do <- list(c(as.Date("2013-06-18"), as.Date("2014-01-02")),
           c(as.Date("2014-01-02"), as.Date("2014-06-24")))

file_names = c("Reanalysis-Data_2013.csv", "Reanalysis-Data_2014.csv")

for(index in 1:2){
  
years_to_get = 2013:2014

setwd(paste0("~/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/", years_to_get[index]))

# list the netCDF files
lf <- list.files(pattern = ".nc$")

# open netcdf files as a list
ncl <- lapply(lf, nc_open)
# check to make sure all pressure levels are stored the same way
lapply(1:length(ncl), function(i) ncl[[i]]$dim$level$vals)

# read the netcdf files as a brick, level 4 = 700 hPa level
bl <- lapply(lf, brick, level = 4)

# get the dates from the netcdf files.  One per day
d <- substring(names(bl[[1]]), first = 2, last = 11)
d <- as.Date(d, format = "%Y.%m.%d")

# set the Z slot with the date extracted from layer names
bl <- lapply(1:length(bl), function(i){
  setZ(bl[[i]], d, name = 'date')
} )

# write the netcdf files out as grd files so that the summary
# reads properly
# for(i in 1:length(bl)){
#   writeRaster(bl[[i]], filename = paste0(lf[i], "-temp.grd"), overwrite = TRUE)
# }

# # remove everything and start fresh to get the right summaries
# rm(list=ls())
# 
# ###
# # reread in the raster format grids
# lf <- list.files(pattern = ".grd$")
# 
# # as bricks
# bl <- lapply(lf, brick)
# 
## beefsteak

# rotate to change the coords to -180:180 rather than 0:360
bl <- lapply(bl, rotate)

# which are greater and less than my dates?
w <- which(getZ(bl[[1]]) >= do[[index]][1] & getZ(bl[[1]]) <= do[[index]][2])

# subset the brick to just the ones I want
bl <- lapply(1:length(bl), function(i) {
  return(bl[[i]][[w]])
} )



# Average lat/long for the study site -114.34075   38.89953
# make it a spatial point so I can extract the data
p <- SpatialPoints(coords = matrix(c(-114.34075, 38.89953), ncol = 2),
                   proj4string = CRS(projection(bl[[1]])))

# extract the data
el <- lapply(bl, extract, y = p)

# get the dates again to make the data.frame
d <- substring(names(bl[[1]]), first = 2, last = 11)
d <- as.Date(d, format = "%Y.%m.%d")

df <- data.frame("date" = d, 
                 "tair" =   signif(t(el[[1]]) - 273.15, 4), 
                 "hgt" =    signif(t(el[[2]]), 4),
                 "omega" =  signif(t(el[[3]]), 4),
                 "rh" =     signif(t(el[[4]]), 4),
                 'shum' =   signif(t(el[[5]]), 4),
                 "uwnd" =   signif(t(el[[6]]), 4),
                 "vwnd" =   signif(t(el[[7]]), 4))

grive <- "~/Google Drive/UNR/UNR-Thesis/Data/Reanalysis/"

write.csv(df, file = file_names[index], row.names = FALSE)
write.csv(df, file = file.path(grive, file_names[index]), row.names = FALSE)

# make a map of NCEP temperature for Nevada
state <- map(database = "state",
             regions = c("nevada", "idaho", "utah", "california", "arizona", "oregon"), fill = TRUE)
state <- map2SpatialPolygons(state, IDs = state$names, proj4string = CRS("+proj=longlat +datum=WGS84"))

nv <- map(database = "state", regions = "nevada", fill = TRUE)
nv <- map2SpatialPolygons(nv, IDs = nv$names, proj4string = CRS("+proj=longlat +data=WGS84"))

r.plot <- crop(bl[[1]][[1]], state) - 273.15
x11()
#png(filename = file.path(grive, "Vitale/Reanalysis2-map.png"),
#    width = 800, height = 800, res = 100)
par(mar = c(5, 5, 4, 2), mfrow = c(1,1))
plot(nv, axes = TRUE )
plot(r.plot, add = TRUE)
plot(state, add = TRUE)
plot(p, add = TRUE, cex = 2, col = "red")
title(main = "700 hPa Air Temperature, 2013-06-18", xlab = "Longitude",
      ylab = "Latitude", cex.lab = 1.9, cex.axis = 1.5,
      cex.main = 2)
legend('bottomleft', legend = 'Study Site', cex = 1.5, pch = 3, col = 'red')

# for(i in 1:length(bl)){
# x11()
# plot(bl[[i]], 1)
# }
}
#dev.off()



