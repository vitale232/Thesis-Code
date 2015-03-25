library(raster)
library(nlme)
library(reshape2)
library(ggplot2)
library(plyr)

source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

m_tmn$month = factor(format(m_tmn$date, '%b'),
                     levels=c('Jan', 'Feb' ,'Mar',
                              'Apr', 'May', 'Jun',
                              'Jul', 'Aug', 'Sep',
                              'Oct', 'Nov', 'Dec'))
df = m_tmn
df$site = as.character(df$site)
tmn_month = ddply(df, .(site, month),
                  summarize,
                  tmn=round(mean(tmn, na.rm=TRUE), 2),
                  sd=round(sd(tmn, na.rm=TRUE), 2))

tmn_month = merge(tmn_month, snake[, c('ID', 'elev', 'tci', 'cc_nlcd')],
                  by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)
pp = qplot(elev, tmn, data=tmn_month)
pp + facet_wrap( ~ month, nrow=3)

mod = lme(tmn ~ elev, data=m_tmn,
          random=~1|month, na.action=na.exclude)
df$resid = resid(mod)

resid_month = ddply(df, .(site, month),
                  summarize,
                  resid=round(mean(resid, na.rm=TRUE), 2),
                  sd=round(sd(resid, na.rm=TRUE), 2))

resid_month = merge(resid_month, snake[, c('ID', 'elev', 'tci', 'cc_nlcd', 'gis.slope')],
                  by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)

pp = ggplot(resid_month) + geom_point(aes(tci, resid), color='dodgerblue2')
pp = pp + facet_wrap( ~ month, nrow=3) + 
  stat_smooth(aes(tci, resid), data=resid_month[!(resid_month$month %in% c('Jan', 'Nov', 'Dec')) ,],
              method='lm', se=FALSE, color='darkgray', size=1.2) +
  stat_smooth(aes(tci, resid), data=resid_month[resid_month$month %in% c('Jan', 'Nov', 'Dec') ,],
              method='lm', se=FALSE, color='red', size=1.2) + 
  stat_smooth(aes(tci, resid), data=resid_month[resid_month$month == 'Jun',],
              method='lm', se=FALSE, color='orange', size=1.2)
pp = pp + theme_gray(base_size=20) + 
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        strip.text=element_text(size=22)) + 
  xlab('Terrain Convergence Index (TCI)') +
  ylab(expression(paste("Residual Minimum Temperature (", degree, "C)")))
print(pp)

ggsave('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure05_tci-elev-resid.pdf')
# pp
# dev.off()

# qplot(gis.slope, resid, data=resid_month, facets=~month)
ppp = ggplot(resid_month) + geom_point(aes(gis.slope, resid), color='blue')
ppp = ppp + facet_wrap( ~ month, nrow=3) + 
  stat_smooth(aes(gis.slope, resid), method='lm', se=FALSE, color='red', size=1.2)
ppp = ppp + theme_gray(base_size=20) + 
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        strip.text=element_text(size=22)) + 
  xlab('Slope (◦)') +
  ylab('Residual Minimum Temperature (◦C)') + 
  scale_x_discrete(breaks=c(0, 10, 20, 30, 40, 50),
                   labels=c('', '10', '20', '30', '40', '50'))
print(ppp)

AIC(lm(resid ~ tci, resid_month),
    lm(resid ~ gis.slope, resid_month),
    lm(resid ~ cc_nlcd, resid_month))





#### TMX ####
# s = stack(list.files('~/Google Drive/UNR/UNR-Thesis/Data/GIS-Data/Irradiance/new_name/', full.names=T))
# r = raster('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/GIS-Data/NLCD/nlcd_utm.tif')
# s = crop(s, r)
# rad = sum(s)
# writeRaster(rad, './GIS-Data/Irradiance/annual_rad.grd')
rad = raster('./GIS-Data/Irradiance/annual_rad.grd')
snake$rad = extract(rad, snake)
m_tmx$month = factor(format(m_tmn$date, '%b'),
                     levels=c('Jan', 'Feb' ,'Mar',
                              'Apr', 'May', 'Jun',
                              'Jul', 'Aug', 'Sep',
                              'Oct', 'Nov', 'Dec'))
df2 = m_tmx

df2$site = as.character(df2$site)
tmx_month = ddply(df2, .(site, month),
                  summarize,
                  tmx=round(mean(tmx, na.rm=TRUE), 2),
                  sd=round(sd(tmx, na.rm=TRUE), 2),
                  irrad=round(mean(irrad, na.rm=TRUE), 2))

tmx_month = merge(tmx_month, snake[, c('ID', 'elev', 'tci', 'cc_nlcd', 'rad')],
                  by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)
pp = qplot(elev, tmx, data=tmx_month)
pp + facet_wrap( ~ month, nrow=3)

mod2 = lme(tmx ~ elev, data=m_tmx,
          random=~1|month, na.action=na.exclude)
df2$resid = resid(mod)

resid_month_tmx = ddply(df2, .(site, month),
                    summarize,
                    resid=round(mean(resid, na.rm=TRUE), 2),
                    sd=round(sd(resid, na.rm=TRUE), 2),
                    irrad=round(mean(irrad, na.rm=TRUE), 2))

resid_month_tmx = merge(resid_month_tmx, snake[, c('ID', 'elev', 'tci', 'cc_nlcd', 'gis.slope', 'rad')],
                    by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)


fig6 = ggplot(resid_month_tmx) + geom_point(aes(rad, resid), color='dodgerblue2')
fig6 = fig6 + facet_wrap( ~ month, nrow=3) + 
  stat_smooth(aes(rad, resid), data=resid_month_tmx,
              method='lm', se=FALSE, color='darkgray', size=1.2) 
print(fig6)


AIC(lm(resid ~ rad, resid_month_tmx),
    lm(resid ~ gis.slope, resid_month_tmx),
    lm(resid ~ cc_nlcd, resid_month_tmx),
    lm(resid ~ tci, resid_month_tmx))

qplot(gis.slope, resid, data=resid_month_tmx, facets=~month)

#### Look at model summaries
# months = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
#            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# for(month in months){
#   print(month)
#   print(coef(summary(lm(resid ~ gis.slope, 
#                    data=resid_month_tmx[resid_month_tmx$month == month, ]))))
# }

qplot(cc_nlcd, resid, data=resid_month_tmx, facets=~month)
qplot(rad, resid, data=resid_month_tmx, facets=~month)
qplot(tci, resid, data=resid_month_tmx, facets=~month)
