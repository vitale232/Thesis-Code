#--------------------------------
# Name:         HLI.R
# Purpose:      Function to calculate McCune et al. (2002) Heat Load Index
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2013/07/19
# R:            3.1.1
#--------------------------------  
# Reference:
# McCune, Bruce and Dylan Keon, 2002. Equations for potential annual direct
# incident radiation and heat load index. Journal of Vegetation Science. 13:603-606.
# available at: http://oregonstate.edu/~mccuneb/radiation.htm 

library(raster)

HLI <- function(DEM){
                slope <- terrain(DEM, opt = 'slope', unit = 'radians', neighbors = 8)
                aspect <- terrain(DEM, opt = 'aspect', unit = 'radians', neighbors = 8)
                
                xy <- SpatialPoints(xyFromCell(DEM, cellFromRowCol(DEM, 1:nrow(DEM), 1)),
                                    proj4string = CRS(projection(DEM)))
                lat <- coordinates(spTransform(xy, CRS("+proj=longlat +datum=WGS84")))[,2]
                latR <- raster(DEM)
                latR[] <- rep(lat, each = ncol(latR))
                latRrad <- calc(latR, fun = function(x) x * 0.0174532925)
                
                cosLatR <- calc(latRrad, fun = function(x) {cos(x)})
                sinLatR <- calc(latRrad, fun = function(x) {sin(x)})
                 
                cosSlope <- calc(slope, fun = function(x){cos(x)})
                sinSlope <- calc(slope, fun = function(x){sin(x)})
                
                foldAsp <- calc(aspect, fun = function(x){pi - abs(x - (5*pi/4))})
                cosFoldAsp <- calc(foldAsp, fun = function(x){cos(x)})
                sinFoldAsp <- calc(foldAsp, fun = function(x){sin(x)})
                
                out <- overlay(cosLatR, sinLatR, foldAsp, cosSlope, sinSlope, cosFoldAsp,
                              sinFoldAsp,
                              fun = 
                              function(cosLatR, sinLatR, foldAsp, cosSlope, sinSlope, cosFoldAsp,
                              sinFoldAsp)
                              {exp(-1.467 + 1.582 * cosLatR * cosSlope - 1.5 * cosFoldAsp
                              * sinSlope * sinLatR - 0.262 * sinLatR * sinSlope + 0.607
                              * sinFoldAsp * sinSlope)})
                
                return(out)
                }
 