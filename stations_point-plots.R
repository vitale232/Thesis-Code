nevcan = read.csv('~/Dropbox/UNR/UNR-Thesis/Data/Validation/validation_nevcan-daily.csv')

nevcan$date = as.Date(nevcan$X)
nevcan$month = format(nevcan$date, '%b')

stations[
  which(stations$station == 'sagebrush_west'), 'Tmn'] = mean(
    nevcan[which(nevcan$month == 'Dec'), 'sage_west_tmn']) 

stations[
  which(stations$station == 'pj_west'), 'Tmn'] = mean(
    nevcan[which(nevcan$month == 'Dec'), 'pj_west_tmn']) 

stations[
  which(stations$station == 'montane_west'), 'Tmn'] = mean(
    nevcan[which(nevcan$month == 'Dec'), 'montane_west_tmn']) 

stations[
  which(stations$station == 'subalpine_west'), 'Tmn'] = mean(
    nevcan[which(nevcan$month == 'Dec'), 'subalpine_west_tmn']) 

stations = as.data.frame(stations)[1:4, ]




stations[
  which(stations$station == 'sagebrush_west'), 'Tmx'] = mean(
    nevcan[which(nevcan$month == 'Jul'), 'sage_west_tmx']) 

stations[
  which(stations$station == 'pj_west'), 'Tmx'] = mean(
    nevcan[which(nevcan$month == 'Jul'), 'pj_west_tmx']) 

stations[
  which(stations$station == 'montane_west'), 'Tmx'] = mean(
    nevcan[which(nevcan$month == 'Jul'), 'montane_west_tmx']) 

stations[
  which(stations$station == 'subalpine_west'), 'Tmx'] = mean(
    nevcan[which(nevcan$month == 'Jul'), 'subalpine_west_tmx']) 

stations = as.data.frame(stations)[1:4, ]

stations$lab = c('Sagebrush', 'PJ', 'Montane', 'Subalpine')
# 
# stations = rbind(stations, stations)
