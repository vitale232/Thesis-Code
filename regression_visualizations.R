#--------------------------------
# Name:         regression_visualizations.R
# Purpose:      Visualize some of the regression coefficients for talk
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/04/06
# R:            3.1.2
#--------------------------------

library(raster)
library(nlme)
library(lme4)
library(ggplot2)
library(visreg)
library(psych)
library(spdep)
library(devtools)

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

#### TMN MODEL ####
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

#### Fit the model ####
## tmn_mod is the final model, as displayed in the hierarchical-models_tmn.R script
## It includes crossed random effects terms for date and site nested within zone, both random intercepts.
## The model also includes a random slope for elevation and its quadratic,
## which is justified by the persistant cold air drainage at the study site.

tmn_mod = lmer(formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+
                         cos(2*pi/365*jday)+sin(2*pi/365*jday)+(1|site)+((elev+I(elev^2))|date)),
               data=m_tmn, REML=FALSE, na.action=na.omit)

#------------------------------------------------------------------#
#### TMX MODELS ####
source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

m_tmx$jday = as.numeric(format(m_tmx$date, '%j'))
m_tmx$jday_cos = cos(2*pi/365 * m_tmx$jday)
m_tmx$jday_sin = sin(2*pi/365 * m_tmx$jday)
m_tmx$elev = m_tmx$elev/1000 # convert to km
m_tmx$irrad = m_tmx$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmx$zone = as.factor(sapply(strsplit(as.character(m_tmx$site), split='P'), function(x) x[1]))

#### Fit the model ####
tmx_mod = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                 cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                 ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_mod)


#------------------------------------------------------------------#
#### START PLOTTING TMN ####

## Topo tmn
par(mar = c(5, 6, 4, 2) + 0.1)
visreg(tmn_mod, 'elev', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))
visreg(tmn_mod, 'tci', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))
visreg(tmn_mod, 'gis.slope', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))

## Interactions tmn
visreg(tmn_mod, 'elev', cex.lab=3.25, axes=FALSE, by='PC4',
       line=list(lwd=7), points=list(cex=1.1))
visreg(tmn_mod, 'elev', cex.lab=3.25, axes=FALSE, by='cc_nlcd',
       line=list(lwd=7), points=list(cex=1.1))

## Synoptic tmn
visreg(tmn_mod, 'tair', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))
visreg(tmn_mod, 'PC4', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))

### Plot sin cosine curves
df = data.frame(date=seq(as.Date('2013-01-01'), as.Date('2013-12-31'), by='day'),
                sin=sin(((2*pi)/365 * 1:365)),
                cos=cos(((2*pi)/365 * 1:365)))
df = melt(df, measure.vars=c('sin', 'cos'))

sincos = ggplot(df) +
  geom_point(aes(date, value, color=variable), size=2) +
  ggtitle('Seasonal Sine and Cosine Curves') +
  xlab('Date') +
  ylab('Value') +
  theme_gray(base_size=24) +
  theme(legend.position='bottom',
        legend.title=element_blank(),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))
print(sincos)

#### START PLOTTING TMX ####

## Topo tmx
visreg(tmx_mod, 'elev', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))
visreg(tmx_mod, 'irrad', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))

## Synoptic tmx
visreg(tmx_mod, 'tair', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))
visreg(tmx_mod, 'PC4', cex.lab=3.25, axes=FALSE, 
       line=list(lwd=7), points=list(cex=1.1))

## interaction tmx
visreg(tmx_mod, 'tci', cex.lab=3.25, axes=FALSE, by='jday',
       line=list(lwd=7), points=list(cex=1.1),
       breaks=c(1, 79, 171, 263))
visreg(tmx_mod, 'elev', cex.lab=3.25, axes=FALSE, by='jday',
       line=list(lwd=7), points=list(cex=1.1),
       breaks=c(1, 79, 171, 263))
