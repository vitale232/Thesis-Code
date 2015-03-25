#--------------------------------
# Name:         hierarchical-models_tmx.R
# Purpose:      Fit 2 level hierarchical models with random effects
#               to the data.
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/10/31
# R:            3.1.1
#--------------------------------

library(nlme)
library(lme4)
library(ggplot2)
library(visreg)
library(psych)
library(spdep)
library(devtools)
# install_github("pbreheny/visreg")

setwd('~/Google Drive/UNR/UNR-Thesis/Data/')

melt_them = TRUE
merge_them = TRUE
load_som = TRUE
load_ra = TRUE
load_eof = TRUE
load_wind = TRUE
source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

#### Calculate Tmx lapse rates
source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/lapse_function.R')
elev_for_lapse = snake$elev/1000
tmx_for_lapse = read.csv('./Aggregated_Data/m-tmx.csv')
m_tmx_lapse = lapse(tmx_for_lapse, elev_for_lapse)
df = data.frame(date=ra$date, m_tmx_lapse)
df$date = as.Date(df$date)
df$tmx_cuts = cut(df$m_tmx_lapse, 5)
df$tmx_cols = as.numeric(df$tmx_cuts)
m_tmx = merge(m_tmx, df, all.x=TRUE, by='date')
cols = c('red', 'brown', 'green', 'blue', 'violet')
# with(m_tmx[m_tmx$site=='D1P01', ], {
#   plot(date, PC4, type='l', cex.axis=1.25, cex.lab=1.25)
#   points(date, PC4, col=cols[tmx_cols])
#   legend('bottomleft', legend=levels(tmx_cuts),
#          pch=1, col=cols)
#   })

m_tmx$jday = as.numeric(format(m_tmx$date, '%j'))
m_tmx$jday_cos = cos(2*pi/365 * m_tmx$jday)
m_tmx$jday_sin = sin(2*pi/365 * m_tmx$jday)
m_tmx$elev = m_tmx$elev/1000 # convert to km
m_tmx$irrad = m_tmx$irrad/1000 # convert from Wh.m-2.day-1 to MegaWh.m-2.day-1
m_tmx$zone = as.factor(sapply(strsplit(as.character(m_tmx$site), split='P'), function(x) x[1]))


#### prelim model
tmx_lm = lm(tmx ~ irrad + elev + tci + gis.slope*gis.aspect + cc_nlcd + tair + PC4, data=m_tmx)
summary(tmx_lm) 
par(mfrow=c(1,1));plot(resid(tmx_lm))
par(mfrow=c(2,2)); plot(tmx_lm)
par(mfrow=c(1,1));hist(resid(tmx_lm))


#### Fit models
tmx_gls = gls(tmx ~ tair + irrad + PC4*elev + tci + tri + tpi + cc_nlcd + cos(2*pi/365 * jday) + sin(2*pi/365 * jday),
              data=m_tmx, na.action=na.exclude)
summary(tmx_gls)
plot(m_tmx$date, residuals(tmx_gls))

tmx_lme1 = lme(tmx ~ tair + irrad + PC4*elev + tci + tri + cc_nlcd + cos(2*pi/365 * jday) + sin(2*pi/365 * jday),
               data=m_tmx, random=~1|date/site, method='ML', na.action=na.exclude)
summary(tmx_lme1)
plot(m_tmx$date, residuals(tmx_lme1))
acf(na.omit(residuals(tmx_lme1)))

tmx_lme2 = lme(tmx ~ tair + irrad + PC4*(elev+I(elev^2)) + tci + tri + cc_nlcd + cos(2*pi/365 * jday) + sin(2*pi/365 * jday),
               data=m_tmx, random=~1|date/site, method='ML', na.action=na.omit)
summary(tmx_lme2)
visreg(tmx_lme2, 'elev', by='PC4')

tmx_lme3 = lme(tmx ~ tair + irrad + PC4*(elev+I(elev^2)) + tci + tri + cc_nlcd + cos(2*pi/365 * jday)*(elev+I(elev^2)) + sin(2*pi/365 * jday)*(elev+I(elev^2)),
               data=m_tmx, random=~1|date/site, method='ML', na.action=na.omit)
summary(tmx_lme3)
visreg(tmx_lme3, 'elev', by='PC4')
anova(tmx_lme2, tmx_lme3)

tmx_lme3 = lme(tmx ~ tair + irrad + PC4 + (elev+I(elev^2)) + tci + tri + cc_nlcd + cos(2*pi/365 * jday)*(elev+I(elev^2)) + sin(2*pi/365 * jday)*(elev+I(elev^2)),
               data=m_tmx, random=~1|date/site, method='ML', na.action=na.omit)
summary(tmx_lme3)
visreg(tmx_lme3, 'elev', by='PC4')
anova(tmx_lme2, tmx_lme3)
acf(residuals(tmx_lme3))

tmx_lme4 = lme(tmx ~ tair + irrad + PC4 + (elev+I(elev^2)) + tci + tri + cc_nlcd + 
                 cos(2*pi/365 * jday)*(elev+I(elev^2)) + sin(2*pi/365 * jday)*(elev+I(elev^2)),
               data=m_tmx, random=~1|date/site, method='ML', na.action=na.omit, cor=corAR1(form=~date))
summary(tmx_lme4)
anova(tmx_lme3, tmx_lme4)


tmx_lme5 = lme(tmx ~ tair + irrad + PC4 + (elev+I(elev^2)) + tci + tri + cc_nlcd + 
                 cos(2*pi/365 * jday)*(elev+I(elev^2)) + sin(2*pi/365 * jday)*(elev+I(elev^2)),
               data=m_tmx, random=list(~1|date, ~1|site, ~1|zone), method='ML', na.action=na.omit)
summary(tmx_lme5)
anova(tmx_lme5, tmx_lme3)

#### Start fitting models with crossed random effects in lme4
tmx_lmer1 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci+tri+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                   (1|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer1)
# anova(tmx_lme3, tmx_lmer1)

tmx_lmer2 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci+tri+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                   (1|date)+(1|site/zone), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer2)
anova(tmx_lmer2, tmx_lmer1)

tmx_lmer3 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci+tri+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer1)
anova(tmx_lmer3, tmx_lmer1)
acf(residuals(tmx_lmer3))

tmx_lmer4 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci+gis.slope+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)

tmx_lmer5 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*PC4+gis.slope+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
anova(tmx_lmer4, tmx_lmer5)

tmx_lmer6 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                  cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                  ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
anova(tmx_lmer4, tmx_lmer6)

tmx_lmer7 = lmer(tmx~tair+irrad+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+PC4*cos(2*pi/365*jday)+PC4*sin(2*pi/365*jday)+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)

tmx_lmer8 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                   cos(2*pi/365*jdayg)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+uwnd*vwnd+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer8)
anova(tmx_lmer6, tmx_lmer8)

tmx_lmer9 = lmer(tmx~tair+irrad+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+uwnd*vwnd*PC4+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer9)
anova(tmx_lmer9, tmx_lmer8)

tmx_lmer10 = lmer(tmx~tair+irrad+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+uwnd*PC4+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer10)
anova(tmx_lmer9, tmx_lmer10)

tmx_lmer11 = lmer(tmx~tair+irrad+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                    cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+PC4+
                    sage_res*pj_res*montane_res*subalpine_res+
                    ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer11)
anova(tmx_lmer6, tmx_lmer11)

tmx_lmer12 = lmer(tmx~tair+irrad+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                    cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+PC4+
                    site_wind+
                    ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_lmer12)
anova(tmx_lmer11, tmx_lmer12)


tmx_lmer13 = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                   cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+site_wind+
                   ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
anova(tmx_lmer13, tmx_lmer6)
summary(tmx_lmer13)


#### Fit the model ####
tmx_mod = lmer(tmx~tair+irrad+PC4+(elev+I(elev^2))+tci*cos(2*pi/365*jday)+tci*sin(2*pi/365*jday)+gis.slope+cc_nlcd+
                 cos(2*pi/365*jday)*(elev+I(elev^2))+sin(2*pi/365*jday)*(elev+I(elev^2))+
                 ((elev+I(elev^2))|date)+(1|site), data=m_tmx, REML=FALSE, na.action=na.omit)
summary(tmx_mod)


# print('Best model: tmx_lme6')
write.csv(summary(tmx_mod)$coefficients,
          './Tables/Tmx/tmx_mod-fixed-effects.csv')
