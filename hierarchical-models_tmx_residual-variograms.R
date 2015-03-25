#--------------------------------
# Name:         hierarchical-models_tmx_residual-variograms.R
# Purpose:      Fit variograms to model residuals for tmx
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/11/12
# R:            3.1.2
#--------------------------------

library(spacetime)
library(gstat)

setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

source('./Thesis-Code/hierarchical-models_tmx.R')
source('./Thesis-Code/moran_mc.R')


#### RERUN THE MODEL WITH NA.EXCLUDE
tmx_lmer3 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci+tri+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.exclude)
summary(tmx_lmer1)

#### TEST FOR SPATIAL AUTO-CORRELATION USING MORAN'S I PERMUTATION TEST ####
m_tmx$resid = residuals(tmx_lmer3, type='pearson', scaled=TRUE) # obs - pred
test = m_tmx[m_tmx$date == as.Date('2013-07-12'), ]
coordinates(test) = ~x_utm+y_utm
proj4string(test) = CRS('+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs')
bubble(test[!is.na(test$resid), ], 'resid')

list_out = list()
dates = unique(m_tmx$date)

for(i in 1:length(dates)){
  print(paste('starting', dates[i]))
  tmp = m_tmx[m_tmx$date == dates[i], c('x_utm', 'y_utm', 'resid')]
  sp = tmp
  coordinates(sp) = ~x_utm+y_utm
  png(paste0('./Tmx_Residual-Bubbleplots/', as.character(dates[i]), '.png'), height=5, width=8,
      res=300, units='in')
  print(bubble(sp[!is.na(sp$resid), 'resid'], main=as.character(dates[i])))
  dev.off()
  l = list(1000, 300, tmp)
  list_out[[i]] = morantable.mc(l)
  print(paste('  finished', dates[i]))
}
names(list_out) = dates

which(sapply(1:length(list_out), function(i) list_out[[i]]$P[1] < 0.05))



m_tmx$resid = residuals(tmx_lmer3, type='pearson', scaled=TRUE) # obs - pred
# m_tmx$resid = residuals(the_mod_3, level=2)
resid_df = reshape2:::dcast(m_tmx[ , c('site', 'date', 'resid')], date ~ site)
resid_df$date = as.Date(resid_df$date)
so = snake[order(snake$elev), ]
row.names(so) = so$ID
id = so$ID
space = list(values=so$ID)

# m_tmx_st = stConstruct(x=m_tmx[ , id], space=space, time=m_tmx$date, SpatialObj=so)
cols = colorRampPalette(c('red', 'deepskyblue'))(255)
resid_st = stConstruct(x=resid_df[ , id], space=space, time=resid_df$date, SpatialObj=so)
stplot(resid_st, mode='xt', col.regions=cols, main = '2m tmx Residuals')

# l = list(1:4, 5:8, 9:12, 13:16, 17:20, 21:24, 25:28, 29:32, 33:36, 37:40)
# for(i in 1:10){
#   rn = row.names(resid_st@sp)[l[[i]]]
#   x11(height=8, width=8)
#   par(mfrow=c(2,2))
#   for(i in rn){
#     acf(na.omit(resid_st[i, ]), main=i)
#   }
# }

timeDim = 1:dim(resid_st)[2]
lst = lapply(timeDim, function(i){
  x = resid_st[,i]
  x$ti = i
  x
})
pooledData = do.call(rbind, lst)
names(pooledData) = c('resid', 'ti')

# vf = variogram(resid~factor(ti), pooledData[!is.na(pooledData$resid), ], dX=0)
# plot(vf)
v = variogram(resid~ti, pooledData[!is.na(pooledData$resid), ], dX=0)
plot(v)
vmod = fit.variogram(v, vgm(psill=1, model='Exp', range=1500, nugget=1))
plot(v, vmod, main='Pooled Variogram, tmx Residuals, cross13, type="pearson", scaled=TRUE')
vmod
# 
# plot(variogram(resid~ti, pooledData[!is.na(pooledData$resid), ], dX=0,
#                alpha=c(0, 45, 90, 135, 180, 225, 270)))
# 
# 
# vv = variogram(values ~ 1, resid_st, tlags=0:7)
# plot(vv)
# plot(vv, map=FALSE)
# 
# 
# metricVgm = vgmST('metric', joint=vgm(50, 'Exp', 100, 0), stAni=50)
# metricVgm = fit.StVariogram(vv, metricVgm)
# attr(metricVgm, 'optim')$value
# plot(vv, metricVgm)
# 
# 
# sepVgm = vgmST('separable', space=vgm(0.9, 'Exp', 123, 0.1),
#                time=vgm(0.9, 'Exp', 2.9, 0.1), sill=100)
# sepVgm = fit.StVariogram(vv, sepVgm, method="L-BFGS-B", lower=c(10,0,0.01,0,1), upper=c(500,1, 20, 1,200))
# attr(sepVgm, 'optim')$value
# 
# plot(vv, list(sepVgm, metricVgm))
# 
# plot(vv, all=T, wireframe=T, zlim=c(0, 0.85), 
#      zlab=NULL, 
#      xlab=list('dist (m)', rot=30),
#      ylab=list('time lag (days)', rot=-35), 
#      scales=list(arrows=F, z=list(distance=5)))
