#--------------------------------
# Name:         Data-Manipulation_Initial.R
# Purpose:      Reads in all of the LogTag data from the Snake Range 2013-2014 and creates 2 tables
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/06/30
# R:            3.1.0 & 3.1.1
#data.table:    1.9.2
#--------------------------------

#===================================================================================================
## Work with the sensor data
library(data.table)

## Must set the system timezone, as R automatically
# tries to compensate for daylight savings
Sys.setenv( TZ="GMT+8" )

setwd("~/Dropbox/UNR/UNR-Thesis/Data/")

## read in the site specific data
snake <- read.csv('./Site-Data/snake2.csv', header = TRUE)
snake$ID <- as.character(snake$ID)


###################################################################################
#### Work on the 2013 field work data

# list all files, 2m files, 0m files
f13 <- list.files(path = "./Sensor-Data_Raw/2013_Sensor-Data/")
m13 <- f13[grep(x = f13, pattern = '_2')] #2 meter readings
g13 <- f13[grep(x = f13, pattern = '_0')] #0 meter readings

# Make a list of data frames from the csv files
# broken into three steps to assure locations match up in the same
# list order
ml13 <- lapply(1:29, function(i) {
  read.csv(file.path("./Sensor-Data_Raw/2013_Sensor-Data/", m13[i]), header = TRUE)
  })

ml13[[30]] <- NULL

for(i in 31:40){
  ml13[[i]] <- read.csv(file.path("./Sensor-Data_Raw/2013_Sensor-Data/", m13[i-1]), header = TRUE)
}

## Read in the ground level data as list of dataframes
gl13 <- lapply(1:length(g13), function(i) {
  read.csv(file.path("./Sensor-Data_Raw/2013_Sensor-Data/", g13[i]), header = TRUE)
  })

### create a DateTime column in base R
## for the 2m data
# broken up to account for missing data
for(i in 1:29) { 
  ml13[[i]]$DateTime <- as.POSIXct(paste(ml13[[i]]$Time, ml13[[i]]$Date, sep = ' '),
                                   format = '%I:%M:%S %p %m/%d/%Y')
  
  out <- data.frame('DateTime' = ml13[[i]]$DateTime, 'T' = ml13[[i]]$Readings...C.)
  id <- substr(m13[grep(m13, pattern = '_2.csv')][i], 1, 5)
  names(out) <- c('DateTime', id)
  ml13[[i]] <- out
  rm(out)
}

for(i in 31:40) { 
    ml13[[i]]$DateTime <- as.POSIXct(paste(ml13[[i]]$Time, ml13[[i]]$Date, sep = ' '),
                                     format = '%I:%M:%S %p %m/%d/%Y')
    
    out <- data.frame('DateTime' = ml13[[i]]$DateTime, 'T' = ml13[[i]]$Readings...C.)
    id <- substr(m13[grep(m13, pattern = '_2.csv')][i-1], 1, 5)
    names(out) <- c('DateTime', id)
    ml13[[i]] <- out
    rm(out)
}


## for the 0m data
gl13 <- lapply(1:length(gl13), function(i){ 
  gl13[[i]]$DateTime <- as.POSIXct(paste(gl13[[i]]$Time, gl13[[i]]$Date, sep = ' '),
                                 format = '%I:%M:%S %p %m/%d/%Y')
  
  out <- data.frame('DateTime' = gl13[[i]]$DateTime, 'T' = gl13[[i]]$Readings...C.)
  id <- substr(g13[grep(g13, pattern = '_0.csv')][i], 1, 5)
  names(out) <- c('DateTime', id)
  return(out)
})

###################################################################################
#### Work on the 2014 field work data

# list all files, 2m files, 0m files
f14 <- list.files(path = "./Sensor-Data_Raw/2014_Sensor-Data/")
m14 <- f14[grep(x = f14, pattern = '_2')] #2 meter readings
g14 <- f14[grep(x = f14, pattern = '_0')] #0 meter readings

# Make a list of data frames from the csv files
ml14 <- lapply(1:31, function(i) {
  out <- read.csv(file.path("./Sensor-Data_Raw/2014_Sensor-Data/", m14[i]), header = FALSE,
                  sep = '\t', skip = 1, as.is = 2:3)
  names(out) <- c('index', 'date', 'time', 'temp', 'na')
  return(out)
})
ml14[[32]] <- NULL
for(i in 33:40){
  ml14[[i]] <- read.csv(file.path("./Sensor-Data_Raw/2014_Sensor-Data/", m14[i-1]), header = FALSE,
                        sep = '\t', skip = 1, as.is = 2:3)
  names(ml14[[i]]) <- c('index', 'date', 'time', 'temp', 'na')
}

### Do it for 0 m data
gl14 <- lapply(1:length(g14), function(i) {
  out <- read.table(file.path("./Sensor-Data_Raw/2014_Sensor-Data/", g14[i]), header = FALSE,
                    sep = '\t', skip = 1, as.is = 2:3)
  names(out) <- c('index', 'date', 'time', 'temp', 'na')
  return(out)
})

### create a DateTime column in base R
# for the 2m data
for(i in 1:31) { 
  ml14[[i]]$DateTime <- as.POSIXct(paste(ml14[[i]]$time, ml14[[i]]$date, sep = ' '),
                                   format = '%I:%M:%S %p %m/%d/%Y')
  
  out <- data.frame('DateTime' = ml14[[i]]$DateTime, 'T' = ml14[[i]]$temp)
  id <- substr(m14[grep(m14, pattern = '_2.csv')][i], 1, 5)
  names(out) <- c('DateTime', id)
  ml14[[i]] <- out
  rm(out)
}

for(i in 33:40) { 
  ml14[[i]]$DateTime <- as.POSIXct(paste(ml14[[i]]$time, ml14[[i]]$date, sep = ' '),
                                   format = '%I:%M:%S %p %m/%d/%Y')
  
  out <- data.frame('DateTime' = ml14[[i]]$DateTime, 'T' = ml14[[i]]$temp)
  id <- substr(m14[grep(m14, pattern = '_2.csv')][i-1], 1, 5)
  names(out) <- c('DateTime', id)
  ml14[[i]] <- out
  rm(out)
}

##########
## The following two lines account for silly field mistakes.  Site D4P09 is 
## off sync by 120 sec and D3P03 is off by 540 sec at the 2 m sensors
ml14[[39]]$DateTime = ml14[[39]]$DateTime - 120
ml14[[23]]$DateTime = ml14[[23]]$DateTime + 540


## for the 0m data
gl14 <- lapply(1:length(gl14), function(i){ 
  gl14[[i]]$DateTime <- paste(gl14[[i]]$date, gl14[[i]]$time)
  gl14[[i]]$DateTime <- as.POSIXct(gl14[[i]]$DateTime, format = '%m/%d/%Y %I:%M:%S %p')  
  
  out <- data.frame('DateTime' = gl14[[i]]$DateTime, 'T' = gl14[[i]]$temp)
  id <- substr(g14[grep(g14, pattern = '_0.csv')][i], 1, 5)
  names(out) <- c('DateTime', id)
  return(out)
})


### combine the 2013 and 2014 field data into one list
gl <- lapply(1:length(gl13), function(i){
  return(rbind(gl13[[i]], gl14[[i]]))
})

ml <- lapply(1:length(ml13), function(i){
  return(rbind(ml13[[i]], ml14[[i]]))
})

rm(list = c('ml14', 'ml13', 'gl14', 'gl13'))


#### Save out the combined 2013 and 2014 data
# 0 m
sapply(1:length(gl), function(i){
  write.csv(gl[[i]], paste0('./Ground-Level/', g14[i]))
  print(paste('WROTE:', g14[i]))
})

# 2 m
column_names = substr(g14[grep(g14, pattern = '_0.csv')], 1, 5)
m_out = paste0(column_names, '_2.csv')

sapply(1:length(ml), function(i){
  write.csv(ml[[i]], paste0('./Two-Meter/', m_out[i]))
  print(paste('WROTE:', m_out[i]))
})


#######################################################################
##### Start combining everything to one dataset
## Create a date sequence to merge

### Dates and times of interest
start = as.POSIXct("2013-06-17 00:00:00")
finish = as.POSIXct("2014-06-24 00:00:00")
date_range = seq(start, finish,
                 by = start + 30*60 - start)

# make a data table of the dates of interest
g_out = data.table('DateTime' = date_range, key = 'DateTime')
m_out = data.table('DateTime' = date_range, key = 'DateTime')

# make the lists into data.table objects for fast merging
gl <- lapply(gl, data.table, key = 'DateTime')
ml <- lapply(ml, data.table, key = 'DateTime')

#### merge g_out with the gl list and append the merged data to g_out
## this step basically ensures the entire time series remains
## and that NA values will be filled in where there are no measurements
column_names = substr(g14[grep(g14, pattern = '_0.csv')], 1, 5)

for(i in 1:length(gl)){
  g_out[ , column_names[i]] <- merge(g_out, gl[[i]], by = 'DateTime', 
                                   all.x = TRUE)[ , column_names[i], with = FALSE]
}

# write the restult to disk
write.csv(g_out, './Ground-Level/Ground-Level_All-Sensors.csv',
          row.names = FALSE)
print(paste("WROTE:", './Ground-Level/Ground-Level_All-Sensors.csv'))

# same file, different spot
write.csv(g_out, './Sensor-Data_Raw/Aggregated_Raw-Data/Ground-Level_All-Sensors.csv',
          row.names = FALSE)
print(paste("WROTE:", './Sensor-Data_Raw/Aggregated_Raw-Data/Ground-Level_All-Sensors.csv'))

##### do the merging for 2 m
for(i in 1:length(ml)){
  m_out[ , column_names[i]] <- merge(m_out, ml[[i]], by = 'DateTime', 
                                     all.x = TRUE)[ , column_names[i], with = FALSE]
}

# write the result
write.csv(m_out, './Two-Meter/Two-Meter_All-Sensors.csv',
          row.names = FALSE)
print(paste("WROTE:", './Two-Meter/Two-Meter_All-Sensors.csv'))

write.csv(m_out, './Sensor-Data_Raw/Aggregated_Raw-Data/Two-Meter_All-Sensors.csv',
          row.names = FALSE)
print(paste("WROTE:", './Sensor-Data_Raw/Aggregated_Raw-Data/Two-Meter_All-Sensors.csv'))


# # calculate T mean, min, max for 2m
m_out$date <- as.Date(format(m_out$DateTime, '%Y-%m-%d', tz = 'GMT+8'))
setkey(m_out, date)

sdcols <- 2:41

m.tav <- m_out[ , lapply(.SD, mean), by = 'date', .SDcols = sdcols]
m.tmn <- m_out[ , lapply(.SD, min), by = 'date', .SDcols = sdcols]
m.tmx <- m_out[ , lapply(.SD, max), by = 'date', .SDcols = sdcols]
m.tmd <- m_out[ , lapply(.SD, median), by = 'date', .SDcols = sdcols]

# calculate T mean, min, max for 0m
g_out$date <- as.Date(format(g_out$DateTime, '%Y-%m-%d', tz = 'GMT+8'))
setkey(g_out, date)

g.tav <- g_out[ , lapply(.SD, mean), by = 'date', .SDcols = sdcols]
g.tmn <- g_out[ , lapply(.SD, min), by = 'date', .SDcols = sdcols]
g.tmx <- g_out[ , lapply(.SD, max), by = 'date', .SDcols = sdcols]
g.tmd <- g_out[ , lapply(.SD, median), by = 'date', .SDcols = sdcols]

fnl <- c("m-tav", "m-tmn", "m-tmx", "m-tmd", "g-tav", "g-tmn", "g-tmx", "g-tmd")
fnl <- paste0(fnl, ".csv")

fl <- list(m.tav, m.tmn, m.tmx, m.tmd, g.tav, g.tmn, g.tmx, g.tmd)

fl <- lapply(1:length(fl), function(i) {
  out <- as.data.frame(fl[[i]])
  out[,-1] <- signif(out[,-1], 3)
  return(out)
})

info = lapply(1:length(fl), function(i) {
  write.csv(fl[[i]], file = file.path("./Aggregated_Data/", fnl[i]), row.names = FALSE)
  print(paste('WROTE: ', file.path("./Aggregated_Data/", fnl[i])))
})

########################################
# Make tdif files for tav, tmn tmx
########################################
# tdif wil arbitrarily be the ground level minus the two meter

## tav
d.tav = data.frame(m.tav$date)
d.tmx = data.frame(m.tav$date)
d.tmn = data.frame(m.tav$date)

g.tav = data.frame(g.tav)
g.tmx = data.frame(g.tmx)
g.tmn = data.frame(g.tmn)

m.tav = data.frame(m.tav)
m.tmx = data.frame(m.tmx)
m.tmn = data.frame(m.tmn)

d.tav = cbind(d.tav, g.tav[ , -1] - m.tav[ , -1])
d.tmn = cbind(d.tmn, g.tmn[ , -1] - m.tmn[ , -1])
d.tmx = cbind(d.tmx, g.tmx[ , -1] - m.tmx[ , -1])




file_names = c('d-tav', 'd-tmn', 'd-tmx')
file_names = paste0(file_names, '.csv')
d_list = list(d.tav, d.tmn, d.tmx)

lapply(1:length(d_list), function(i){
  write.csv(d_list[[i]], file=file.path('./Aggregated_Data/', file_names[i]), row.names=FALSE)
  print(paste('WROTE: ', file.path('./Aggregated_Data/', file_names[i])))
})
