#--------------------------------
# Name:         hierarchical-models_tmn.R
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

setwd('~/Dropbox/UNR/UNR-Thesis/Data/')

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



#### Start model fitting
tmn_lm = lm(tmn ~ irrad + elev + tci + gis.slope*gis.aspect + cc_nlcd + tair + PC4, data=m_tmn)
summary(tmn_lm) 
par(mfrow=c(1,1));plot(resid(tmn_lm))
par(mfrow=c(2,2)); plot(tmn_lm)
par(mfrow=c(1,1));hist(resid(tmn_lm))

# not everything is significant, let's rethink this model
form = formula(tmn ~ irrad + elev + tci + gis.slope*gis.aspect + cc_nlcd + tair + PC1*PC4 + jday_cos + jday_sin)
tmn_gls = gls(form, data=m_tmn, method='ML', na.action=na.exclude)
summary(tmn_gls)

# second attempt at gls.  This model seems reasonable
form2 = formula(tmn ~ irrad + tair + elev + tci + cc_nlcd + PC4 + jday_cos + jday_sin)
form2a = formula(tmn ~ irrad + tair + elev + tci + cc_nlcd + PC4 + cos(2*pi/365 * jday) + sin(2*pi/365 * jday))
form2b = formula(tmn ~ irrad + tair + elev + tci + cc_nlcd + PC4)
tmn_gls2 = gls(form2, data=m_tmn, method='ML', na.action=na.exclude)
summary(tmn_gls2)
anova(tmn_gls, tmn_gls2)
plot(hist(resid(tmn_gls2)))
plot(tmn_gls2)
plot(resid(tmn_gls2))

m_tmn_center = m_tmn
m_tmn_center[,c('tair','irrad','tci','cc_nlcd','elev','PC4','jday_cos','jday_sin')]=
  scale(m_tmn_center[,c('tair','irrad','tci','cc_nlcd','elev','PC4','jday_cos','jday_sin')],sc=F)
summary(m_tmn_center)

# m_tmn_center = apply(m_tmn, 2, function(x)if(is.numeric(x))scale(x, scale=FALSE))
# tmn_lme4 = lme(form2, data=m_tmn, random=~1|date/site, method='ML', na.action=na.exclude)
# tmn_lme1 = lme(form2, data=m_tmn, random=list(~1|date,~1|site), method='ML', na.action=na.exclude)
# library(lme4);tmn_lmer1 = lmer(tmn ~ scale(irrad,sc=F) + tair + elev + tci + cc_nlcd + PC4 + jday_cos + jday_sin + (1|date)+(1|site), data=m_tmn_center,  REML=F)
# library(lme4);tmn_lmer1 = lmer(tmn ~ scale(irrad,sc=F) + tair + elev + tci + cc_nlcd + PC4 + jday_cos + jday_sin + (1|date/site), data=m_tmn_center,  REML=F)

tmn_lme1 = lme(form2, data=m_tmn, random=~1|date/site, method='ML', na.action=na.omit)
tmn_lme1a = lme(form2a, data=m_tmn, random=~1|date/site, method='ML', na.action=na.omit)
visreg(tmn_lme1a,'cc_nlcd')
tmn_lme1b = lme(form2b, data=m_tmn, random=~1|date/site, method='ML', na.action=na.omit)
summary(tmn_lme1b)

summary(tmn_lme1)
summary(tmn_lme1a)
anova(tmn_gls2, tmn_lme1) # random effect acconts for lots of var, but some predictors are no longer sig
plot(hist(resid(tmn_lme1)))
plot(tmn_lme1)
plot(resid(tmn_lme1))
acf(na.omit(resid(tmn_lme1)))

form3 = formula(tmn ~ irrad + tair + elev + tci + cc_nlcd + PC4 + jday_cos)
tmn_lme2 = update(tmn_lme1, form3)
summary(tmn_lme2) # all that hard work on the EOFs looks waste, not sig.
anova(tmn_lme1, tmn_lme2)

form4 = formula(tmn ~ irrad + tair + elev + tci + cc_nlcd + jday_cos)
tmn_lme3 = update(tmn_lme2, form4)
summary(tmn_lme3)
anova(tmn_lme1, tmn_lme2, tmn_lme3)

test = lme(form3, data=m_tmn, random=~1|date, method='ML', na.action=na.exclude)
summary(test)


form5 = formula(tmn ~ irrad + tair + elev + tci + cc_nlcd + PC4*cos(2*pi/365 * jday) +PC4* sin(2*pi/365 * jday))
tmn_lme1b = lme(form5, data=m_tmn, random=~1|date/site, method='ML', na.action=na.omit)
visreg(tmn_lme1b, 'jday', by='PC4')

form6 = formula(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd +cos(2*pi/365 * jday) +sin(2*pi/365 * jday))
the_mod = lme(form6, data=m_tmn, random=~1|date/site, method='ML', na.action=na.exclude)
summary(the_mod)
plot(resid(the_mod))
acf(na.omit(resid(the_mod)))
visreg(update(the_mod, na.action=na.omit), 'elev', by='PC4')

form7 = formula(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd + gis.slope + cos(2*pi/365 * jday) +sin(2*pi/365 * jday))
the_mod_2 =  lme(form7, data=m_tmn, random=~1|date/site, method='ML', na.action=na.exclude)
summary(the_mod_2)
anova(the_mod, the_mod_2)

form8 = formula(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd + gis.slope + gis.aspect + cos(2*pi/365 * jday) +sin(2*pi/365 * jday))
the_mod_3 =  lme(form8, data=m_tmn, random=~1|date/site, method='ML', na.action=na.exclude)
summary(the_mod_3)
anova(the_mod_2, the_mod_3)

form9 = formula(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd + gis.slope + gis.aspect + cos(2*pi/365 * jday) +sin(2*pi/365 * jday) + (1|site) + (1|date))
cross1 = lmer(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd + gis.slope + cos(2*pi/365 * jday) +sin(2*pi/365 * jday) + (1|site) + (1|date),
              data=m_tmn, REML=FALSE, na.action=na.omit)
cross2 = lmer(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd + gis.slope + cos(2*pi/365 * jday) +sin(2*pi/365 * jday) + (1|site) + (elev|date),
              data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross2)

cross3 = lmer(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd + gis.slope + cos(2*pi/365 * jday) +sin(2*pi/365 * jday) + (1|site)+(elev|date),
             data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross3)

cross4 = lmer(tmn~elev*PC4+irrad+tair+tci+cc_nlcd+gis.slope+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|site)+(elev|date),
              data=m_tmn, REML=FALSE, na.action=na.exclude)
anova(cross3, cross4)

cross5 = lmer(tmn~elev*PC4+irrad+tair+tci+cc_nlcd*elev+gis.slope+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|site)+(elev|date),
              data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross5)
anova(cross5, cross4)

m_tmn$PC4100 = m_tmn$PC4/100
cross6 = lmer(tmn~elev*PC4100+irrad+tair+tci+cc_nlcd*elev+gis.slope+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|site)+(PC4100*elev|date),
              data=m_tmn, REML=FALSE, na.action=na.exclude)

cross7 = lmer(tmn~elev*PC4+irrad+tair+tci+cc_nlcd*elev+gis.slope+ccm.pct+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|site)+(elev|date),
              data=m_tmn, REML=FALSE, na.action=na.omit)
summary(cross7)

cross8 = lmer(tmn~(elev+I(elev^2))*PC4+irrad+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|site)+((elev+I(elev^2))|date),
              data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross8)

formy = formula(tmn~(elev+I(elev^2))*PC4+irrad+(elev+I(elev^2))*tair+tci+
                 cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+
                 (1|site)+((elev+I(elev^2))|date))
cross9 = lmer(formy, data=m_tmn, REML=FALSE, na.action=na.exclude)

formy10 = formula(tmn~(elev+I(elev^2))*PC4+irrad+(elev+I(elev^2))*tair+I(log(tci))+
                    cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+
                    (1|site)+((elev+I(elev^2))|date))
cross10 = lmer(formy10, data=m_tmn, REML=FALSE, na.action=na.omit)
summary(cross9)
m_tmn$dumb1 = 1
m_tmn$dumb2 = 1

cross4 = lme(tmn ~ elev * PC4 + irrad + tair + tci + cc_nlcd + gis.slope + cos(2*pi/365 * jday) +sin(2*pi/365 * jday),
             random=list(dumb1=pdDiag(~date-1), dumb2=pdDiag(~site-1)), data=m_tmn, method='ML', na.action=na.exclude)

cross11 = lmer(tmn~(elev+I(elev^2))*PC4+irrad+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|site)+(elev|date),
              data=m_tmn, REML=FALSE, na.action=na.omit)
summary(cros11)

cross12 = lmer(tmn~(elev+I(elev^2))*PC4+irrad+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+tpi+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|site)+((elev+I(elev^2))|date),
               data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross12)
anova(cross8, cross12)

cross13 = lmer(tmn~(elev+I(elev^2))*PC4+irrad+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+tpi+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|zone/site)+((elev+I(elev^2))|date),
               data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross13)
anova(cross12, cross13)

cross14 = lmer(tmn~(elev+I(elev^2))*PC4+irrad+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+roughness+cos(2*pi/365 * jday)+sin(2*pi/365 * jday)+(1|zone/site)+((elev+I(elev^2))|date),
               data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross14)
anova(cross14, cross13)

cross15 = lmer(tmn~(elev+I(elev^2))*PC4+irrad+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+tri+cos(2*pi/365*jday)+sin(2*pi/365*jday)+(1|zone/site)+((elev+I(elev^2))|date),
               data=m_tmn, REML=FALSE, na.action=na.exclude)
summary(cross15)
anova(cross15, cross13)

formy = formula(tmn~(elev+I(elev^2))*PC4+irrad+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+sin(0.01721421*jday)+cos(0.01721421*jday)+(1|zone/site)+((elev+I(elev^2))|date))
cross16 = lmer(formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365*jday)+sin(2*pi/365*jday)+(1|zone/site)+((elev+I(elev^2))|date)),
               data=m_tmn, REML=FALSE, na.action=na.omit)

cross17 = lmer(formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365*jday)+sin(2*pi/365*jday)+(1|site)+((elev+I(elev^2))|date)),
               data=m_tmn, REML=FALSE, na.action=na.omit)

formFitting = formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+PC4*tci+
                        cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365*jday)+
                        sin(2*pi/365*jday)+(1|zone/site)+((elev+I(elev^2))|date))
cross_tmn1 = lmer(formFitting, data=m_tmn, REML=FALSE, na.action=na.omit)
anova(cross16, cross_tmn1)
cross_tmn2_exclude = lmer(formFitting, data=m_tmn, REML=FALSE, na.action=na.exclude)

formFitting = formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+PC4*(tci+I(tci^2))+
                        cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365*jday)+
                        sin(2*pi/365*jday)+(1|zone/site)+((elev+I(elev^2))|date))
cross_tmn2 = lmer(formFitting, data=m_tmn, REML=FALSE, na.action=na.omit)



formFitting = formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+PC4*tci+
                        cc_nlcd*(elev+I(elev^2))+gis.slope+cos(2*pi/365*jday)+ site_wind+
                        sin(2*pi/365*jday)+(1|zone/site)+((elev+I(elev^2))|date))
cross_tmn3 = lmer(formFitting, data=m_tmn, REML=FALSE, na.action=na.omit)
anova(cross_tmn1, cross_tmn3)

#### Test out nlme with nested effects
form = formula(tmn ~ irrad + (elev+I(elev^2))*PC4 + tair + PC4*tci +
                 cc_nlcd*(elev+I(elev^2)) + gis.slope + cos(2*pi/365*jday) +
                 sin(2*pi/365*jday))

test = lme(form, random=~ 1 + (elev+I(elev^2))|site/date,
           method='ML', data=m_tmn, na.action=na.exclude)

test2 = lme(form, random=~ 1 + (elev+I(elev^2)) | date/site,
            method='ML', data=m_tmn, na.action=na.exclude)

test3 = lme(form, random=~ (elev+I(elev^2)) | date/site,
            method='ML', data=m_tmn, na.action=na.exclude)

test4 = lme(form, random=~ 1 | date/site,
            method='ML', data=m_tmn, na.action=na.exclude)

test5 = lme(form, random=~1 + (elev*I(elev^2))|date,
            method='ML', data=m_tmn, na.action=na.exclude)

#################################################################################################

# #### TEST FOR TEMPORAL AUTO-CORRELATION
# the_mod_corr = update(the_mod_3, correlation=corAR1())
# summary(the_mod_corr)
# anova(the_mod_3, the_mod_corr)
# 
# acf(na.omit(resid(the_mod_3)))
# acf(na.omit(resid(the_mod_corr)))
# plot(the_mod_3)
# plot(the_mod_corr)


#### TEST FOR SPATIAL AUTO-CORRELATION ####
m_tmn$resid = residuals(cross_tmn2_exclude, type='pearson', scaled=TRUE) # obs - pred
test = m_tmn[m_tmn$date == as.Date('2013-07-12'), ]
coordinates(test) = ~x_utm+y_utm
proj4string(test) = CRS('+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs')
bubble(test[!is.na(test$resid), ], 'resid')

test2 = m_tmn[, c('x_utm', 'y_utm', 'resid')]

list_out = list()
dates = unique(m_tmn$date)
source('./Thesis-Code/moran_mc.R')
for(i in 1:length(dates)){
  print(paste('starting', dates[i]))
  tmp = m_tmn[m_tmn$date == dates[i], c('x_utm', 'y_utm', 'resid')]
  sp = tmp
  coordinates(sp) = ~x_utm+y_utm
  png(paste0('~/Desktop/cross_tmn1/', as.character(dates[i]), '.png'), height=5, width=8,
      res=300, units='in')
    print(bubble(sp[!is.na(sp$resid), 'resid'], main=as.character(dates[i])))
  dev.off()
  l = list(1000, 300, tmp)
  list_out[[i]] = morantable.mc(l)
  print(paste('  finished', dates[i]))
}
names(list_out) = dates

which(sapply(1:length(list_out), function(i) list_out[[i]]$P[1] < 0.05))

sites = unique(m_tmn$site)
for(i in 1:length(sites)){
  fname = file.path('/home', 'vitale232', 'Desktop', 'acfs', paste0(sites[[i]], '.png'))
  png(fname, height=7, width=7, res=200, units='in')
  acf(na.omit(m_tmn[m_tmn$site == sites[[i]], ]$resid), main=sites[[i]])
  dev.off()
}

# inc = 500
# 
# xymat = coordinates(test)
# dist = as.matrix(dist(xymat))
# inv.dist = 1/dist
# diag(inv.dist) = 0
# 
# maxprop=0.5
# md = max(dist)*maxprop
# 
# upper = c(seq(inc, (md-inc), inc), md)
# binvect = c(0, upper)
# numbins = length(binvect) - 1
# 
# binpairs = numeric(numbins) #empty pvalue vector
# binmoran.obs = numeric(numbins) #empty moran's I vector
# binmoran.exp = numeric(numbins) #empty moran's I sd vector
# binmoran.sd = numeric(numbins) #empty moran's I expected vector
# binmoran.rank = numeric(numbins) #empty moran's I expected vector
# binmoran.pval = numeric(numbins) #empty pvalue vector
# 
# for(j in 1:numbins){
#   dist.binary=(dist > binvect[j] & dist <= binvect[j+1])
# }
# 
# 


# 
# ##### FOLLOWING FRIDLEY, 2009 SCRIPT ####
# #### Step 1: Fit random effects only, examine variance components
# mn1 = lme(tmn~1, data=m_tmn, random=~1|site, method='ML', na.action=na.exclude)
# summary(mn1)
# VarCorr(mn1)
# 
# #### Step 2: Specify level 1 of the model
# mn2 = lme(tmn~tair, data=m_tmn, random=~1|site, method='ML', na.action=na.exclude)
# summary(mn2)
# VarCorr(mn2) # level 1 variance down to 6.4 from 70
# 
# ## add daily radiation 
# mn3 = lme(tmn~tair+irrad, data=m_tmn, random=~1|site, method='ML', na.action=na.exclude)
# VarCorr(mn3)
# anova(mn2, mn3) # irrad is a significant term for tmn
# 
# ## does reanalysis air temp interact with daily radiation load?
# mn4 = lme(tmn~tair*irrad, data=m_tmn, random=~1|site, method='ML', na.action=na.exclude)
# summary(mn4)
# anova(mn3, mn4) # No, this term is insignificant
# 
# ## add seasonal term independnt of synoptics
# mn5 = lme(tmn~tair+irrad+cos(0.0172*jday)+sin(0.0172*jday), random=~1|site, data=m_tmn,
#           method='ML', na.action=na.exclude)
# summary(mn5)
# anova(mn3, mn5) # these terms are significant, and mn5 is the better model
# 
# ## do synoptic pressure levels contribute?
# mn6 = lme(tmn~tair+irrad+PC4+cos(0.0172*jday)+sin(0.0172*jday), random=~1|site,
#           data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn6)
# anova(mn5, mn6) # PC4 is a keeper
# 
# ## Does pressure interact with tair?
# mn7 = lme(tmn~tair*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday), random=~1|site,
#           data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn7)
# anova(mn6, mn7) # No, mn6 is the better model
# 
# #### Step 3: Specify fixed effects for level 2 of the model
# mn8 = lme(tmn~tair+PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+elev, random=~1|site,
#           data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn8)
# anova(mn6, mn8)
# 
# ## Since PC4 is so correlated with lapse rates, does 
# ## allowing PC4 to interact with elevation better explain
# ## variance in minimum temperature?
# mn9 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday), random=~1|site,
#                 data=m_tmn, method='ML', na.action=na.omit)
# summary(mn9)
# anova(mn8, mn9) # yes, this term is very significant (t-value of -28.5!)
# ## let's look at the elev*PC4 interaction
# visreg(mn9, 'elev', by='PC4')
# 
# ## does canopy cover add anything?
# mn10 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd, random=~1|site,
#            data=m_tmn, method='ML', na.action=na.omit)
# summary(mn10) # a little, p value of 0.03
# anova(mn9, mn10) # significantly better fit and very interpretable, let's do it.
# 
# ## How does TCI improve the fit?
# mn11 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+tci, random=~1|site,
#            data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn11)
# anova(mn10, mn11)
# ## Is log TCI better?
# mn12 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+I(log(tci)), random=~1|site,
#            data=m_tmn, method='ML', na.action=na.omit)
# summary(mn12)
# anova(mn11, mn12) # it's a toss up, let's keep it linear and stick with mn11
# 
# mn13 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+tci, random=~1|site,
#            data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn13)
# anova(mn12, mn13)
# 
# mn14 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+tci,
#            data=m_tmn, random=~1|date/site, method='ML', na.action=na.exclude)
# summary(mn14)
# anova(mn14, mn13)
# 
# mn15 = lme(tmn~tair+elev*irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+tci, random=list(~1|date, ~1|site),
#            data=m_tmn, method='ML', na.action=na.omit)
# summary(mn15)
# anova(mn14, mn15)
# 
# mn16 = lme(tmn~tair+elev*irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+tci+x_utm, random=list(~1|date, ~1|site),
#     data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn16)
# anova(mn16, mn15)
# 
# mn17 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd*uwnd+tci,
#            random=~1|site, data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn17)
# anova(mn16, mn17)
# 
# mn18 = lme(tmn~tair+elev*PC4+irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+uwnd+tci,
#            random=~1|site, data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn18)
# anova(mn16, mn18)
# 
# m_tmn$resid = residuals(17, level=1)
# test = na.omit(m_tmn[m_tmn$date == as.Date('2014-03-30'), ]); coordinates(test) = ~x_utm+y_utm
# bubble(test, 'resid')
# 
# mn19 = lme(tmn~tair+elev*irrad+cos(0.0172*jday)+sin(0.0172*jday)+cc_nlcd+tci, random=list(~1|date, ~1|site),
#            data=m_tmn, method='ML', na.action=na.exclude)
# summary(mn19)
# anova(mn19, mn15)
# Variogram(mn19)
# 
# mn20 = lme(tmn~tair+elev*PC4+irrad+cc_nlcd+tci+cos(0.0172*jday)+sin(0.0172*jday), data=m_tmn,
#            random=~1|date/site, method='ML', na.action=na.exclude)
# summary(mn20)
print('Best model: tmn_mod')


tmn_mod = lmer(formula(tmn~irrad+(elev+I(elev^2))*PC4+tair+tci+cc_nlcd*(elev+I(elev^2))+gis.slope+
                         cos(2*pi/365*jday)+sin(2*pi/365*jday)+(1|site)+((elev+I(elev^2))|date)),
               data=m_tmn, REML=FALSE, na.action=na.omit)

tmn_tab = data.frame(summary(tmn_mod)$coefficients)
write.csv(round(tmn_tab, 4), './Tables/Tmn/tmn_mod_fixed-effects.csv')
