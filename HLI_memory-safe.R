HLI <- function(DEM, filename, overwrite=TRUE, ...){
  ## Initialize an empty raster for output
  out = raster(DEM)
  
  ## Determine blocksize using raster package function
  ## and initialize writing
  bs = blockSize(DEM)
  out = writeStart(out, filename, overwrite=overwrite, ...)
  
  ## Extract number of columns to use in repeating
  ## latitude within the for loop (below)
  ncol_out = ncol(out)
  
  ## calculate slope and the sin/cosine of the slope
  ## in radians, as described in McCune 2002
  slope = terrain(DEM, opt = 'slope', unit = 'radians', neighbors = 8)
  cosSlope = calc(slope, fun = function(x){cos(x)})
  sinSlope = calc(slope, fun = function(x){sin(x)})
  
  ## Calculate aspect in radians and calculate folded aspect
  ## and the sin/cos of both
  aspect = terrain(DEM, opt = 'aspect', unit = 'radians', neighbors = 8)
  foldAsp = calc(aspect, fun = function(x){pi - abs(x - (5*pi/4))})
  cosFoldAsp = calc(foldAsp, fun = function(x){cos(x)})
  sinFoldAsp = calc(foldAsp, fun = function(x){sin(x)})
  
  ## Extract the x and y coordinates of the DEM
  xy = SpatialPoints(xyFromCell(DEM, cellFromRowCol(DEM, 1:nrow(DEM), 1)),
                     proj4string = CRS(projection(DEM)))
  ## Transform the xy coordinates to lat/long
  lat = coordinates(spTransform(xy, CRS("+proj=longlat +datum=WGS84")))[,2]
  
  ## loop through the blocks as determined by raster:::blockSize
  for(i in 1:bs$n){
    
    ## Create a vector of indexes to repeat latitude
    ## and make its dimensions match slope/aspect etc
    ## then use rep to create the vector
    these = bs$row[i]:((bs$row[i]+bs$nrows[i])-1)
    l = rep(lat[these], each=ncol_out)
    
    ## transform latitude vector to radianse and calculate
    ## sine/cosine
    l_rad = l * 0.0174532925
    l_cos = cos(l_rad)
    l_sin = sin(l_rad)
    
    ## extract the slope/aspect sine/cosine blocks
    slope_cos = getValues(cosSlope, row=bs$row[i], nrows=bs$nrows[i])
    slope_sin = getValues(sinSlope, row=bs$row[i], nrows=bs$nrows[i])
    
    fold_asp = getValues(foldAsp, row=bs$row[i], nrows=bs$nrows[i])
    cos_fold_asp = getValues(cosFoldAsp, row=bs$row[i], nrows=bs$nrows[i])
    sin_fold_asp = getValues(sinFoldAsp, row=bs$row[i], nrows=bs$nrows[i])
    
    ## apply the final equation to the block
    v = exp(-1.467 + 1.582 * l_cos * slope_cos - 1.5 * cos_fold_asp
            * slope_sin * l_sin - 0.262 * l_sin * slope_sin + 0.607
            * sin_fold_asp * slope_sin)
    
    ## write the block to the raster out
    out = writeValues(out, v, bs$row[i])
  }       
  ## stop writing and return the raster out
  out = writeStop(out)
  return(out)                    
}
# 
# nevada_dem = raster('/media/george/Student_Folders/Common_Data/Nevada_DEMs/nv_dem.img')
# 
# hli = HLI(nevada_dem, filename='/media/george/Student_Folders/Common_Data/Nevada_DEMs/nv_hli.tif',
#           overwrite=TRUE, format='GTiff')
#           