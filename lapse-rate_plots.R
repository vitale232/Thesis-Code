#--------------------------------
# Name:         lapse-rate_plots.R
# Purpose:      Plot lapse rates for tmx and tmn by day
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/11/18
# R:            3.1.2
#--------------------------------

library(nlme)
library(lme4)
library(ggplot2)
library(visreg)

setwd('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/')
melt_them = TRUE
merge_them = TRUE
load_som = TRUE
load_ra = TRUE
load_eof = TRUE
source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

m_tmn$jday = as.numeric(format(m_tmn$date, '%j'))
m_tmn$jday_cos = cos(2*pi/365 * m_tmn$jday)
m_tmn$jday_sin = sin(2*pi/365 * m_tmn$jday)
m_tmn$elev = m_tmn$elev/1000 # convert to km
m_tmn$irrad = m_tmn$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmn$zone = as.factor(sapply(strsplit(as.character(m_tmn$site), split='P'), function(x) x[1]))

m_tmx$jday = as.numeric(format(m_tmx$date, '%j'))
m_tmx$jday_cos = cos(2*pi/365 * m_tmx$jday)
m_tmx$jday_sin = sin(2*pi/365 * m_tmx$jday)
m_tmx$elev = m_tmx$elev/1000 # convert to km
m_tmx$irrad = m_tmx$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmx$zone = as.factor(sapply(strsplit(as.character(m_tmx$site), split='P'), function(x) x[1]))

# x11(height=11, width=17)
png(filename='~/Dropbox/UNR/UNR-Thesis/Figures/lapse-rates_tmn.png',
    height=11, width=17, res=300, units='in')
m_tmn$jday_lab = paste(strftime(m_tmn$date, '%y'), strftime(m_tmn$date, '%j'), sep='-')
brks = c(2, 2.5, 3, 3.5)
labs = c(2, '', 3, '')
qplot(elev, tmn, data=m_tmn) + facet_wrap(~jday_lab, nrow=10) + geom_smooth(method='lm') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks=brks, labels=labs) + labs(title='Tmn Lapse Rates, West Slope of Snake Range, NV', 
                                                      x='Elevation (km)', y='Minimum Temperature (°C)')
dev.off()


# x11(height=11, width=17)
png(filename='~/Dropbox/UNR/UNR-Thesis/Figures/lapse-rates_tmx.png',
    height=11, width=17, res=300, units='in')
m_tmx$jday_lab = paste(strftime(m_tmx$date, '%y'), strftime(m_tmx$date, '%j'), sep='-')
brks = c(2, 2.5, 3, 3.5)
labs = c(2, '', 3, '')
qplot(elev, tmx, data=m_tmx) + facet_wrap(~jday_lab, nrow=10) + geom_smooth(method='lm') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks=brks, labels=labs) + labs(title='Tmx Lapse Rates, West Slope of Snake Range, NV', 
                                                      x='Elevation (km)', y='Maximum Temperature (°C)')
dev.off()



png(filename='~/Dropbox/UNR/UNR-Thesis/Figures/lapse-rates_tmn-pj-elev.png',
    height=11, width=17, res=300, units='in')
m_tmn$jday_lab = paste(strftime(m_tmn$date, '%y'), strftime(m_tmn$date, '%j'), sep='-')
brks = c(2, 2.5, 3, 3.5)
labs = c(2, '', 3, '')
qplot(elev, tmn, data=m_tmn[m_tmn$date<as.Date('2013-11-01'), ]) + facet_wrap(~jday_lab, nrow=10) + geom_smooth(method='lm') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks=brks, labels=labs) + labs(title='Tmn Lapse Rates, West Slope of Snake Range, NV', 
                                                      x='Elevation (km)', y='Minimum Temperature (°C)') +
  geom_vline(xintercept=2.2)
dev.off()

png(filename='~/Dropbox/UNR/UNR-Thesis/Figures/lapse-rates_tmx-pj-elev.png',
    height=11, width=17, res=300, units='in')
m_tmx$jday_lab = paste(strftime(m_tmx$date, '%y'), strftime(m_tmx$date, '%j'), sep='-')
brks = c(2, 2.5, 3, 3.5)
labs = c(2, '', 3, '')
qplot(elev, tmx, data=m_tmx[m_tmx$date<as.Date('2013-11-01'), ]) + facet_wrap(~jday_lab, nrow=10) + geom_smooth(method='lm') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks=brks, labels=labs) + labs(title='Tmx Lapse Rates, West Slope of Snake Range, NV', 
                                                      x='Elevation (km)', y='Maximum Temperature (°C)') +
  geom_vline(xintercept=2.2)
dev.off()
