#--------------------------------
# Name:         hierarchical_models.R
# Purpose:      Fit 2 level hierarchical models with random effects
#               to the data.
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/09/30
# R:            3.1.1
#--------------------------------

library(nlme)
library(lme4)
library(ggplot2)
library(visreg)

melt_them = TRUE
merge_them = TRUE
load_som = TRUE
load_ra = TRUE
load_eof = TRUE
source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

## Fit a linear mixed effect model of intercept allowing intercept
## to vary by site.
tmn_lme1 = lme(fixed=tmn ~ 1, random=~1|site, method='ML',
               na.action=na.exclude, data=m_tmn)
summary(tmn_lme1)

## Look at groupedData plots
gd = groupedData(tmn ~ 1|site, data=m_tmn)
plot(gd, main='Site level variation of tmn')

gd = groupedData(tmn ~ elev|site, data=m_tmn)
x11(width=10)
plot(gd, main='Site level variation modeled with elev')

gd = groupedData(tmn ~ as.numeric(date)|site, data=m_tmn)
plot(gd)

tmn_lme2 = update(tmn_lme1, fixed=tmn ~ elev)
# how are we doing by adding elev?
anova(tmn_lme1, tmn_lme2)
# does the inclusion of the random effect add anything?
anova(tmn_lme2, lm(tmn ~ elev, m_tmn))

tmn_lme3 = update(tmn_lme1, fixed=tmn ~ tci)
tmn_lme4 = update(tmn_lme1, fixed=tmn ~ elev + tci)
tmn_lme5 = update(tmn_lme1, fixed=tmn ~ elev*tci)
tmn_lme6 = update(tmn_lme1, fixed=tmn ~ elev + I(log(tci)))

anova(tmn_lme1, tmn_lme2, tmn_lme3, tmn_lme4, tmn_lme5, tmn_lme6)

tmn_lme7 = update(tmn_lme1, fixed=tmn ~ elev + as.factor(unit_7x5))
tmn_lme8 = update(tmn_lme1, fixed=tmn ~ elev + as.factor(unit_5x5))
tmn_lme9 = update(tmn_lme1, fixed=tmn ~ elev + as.factor(unit_4x4))

anova(tmn_lme2, tmn_lme7)
anova(tmn_lme2, tmn_lme8)
anova(tmn_lme2, tmn_lme9)

tmn_lme10 = update(tmn_lme1, fixed=tmn ~ elev + tair)
anova(tmn_lme2, tmn_lme10)

tmn_lme11 = update(tmn_lme1, fixed=tmn ~ elev + tair + as.factor(unit_7x5))
anova(tmn_lme10, tmn_lme11)

tmn_lme12 = update(tmn_lme1, fixed=tmn ~ elev + tair:as.factor(unit_7x5))
anova(tmn_lme11, tmn_lme12)

summary(tmn_lme11)

tmn_lme13 = update(tmn_lme11, correlation=corAR1())
anova(tmn_lme11, tmn_lme13)


##### START LOOKING AT DATE EFFECTS
gd = groupedData(tmn ~ 1|unit_7x5, data=m_tmn)
plot(gd)

gd = groupedData(tmn ~ 1|unit_5x5, data=m_tmn)
plot(gd)

mod1 = gls(tmn ~ date, data=m_tmn, method='ML', na.action=na.exclude)
summary(mod1)
plot(m_tmn$date, resid(mod1))
acf(na.omit(residuals(mod1)))

# mod2 = gls(tmn ~ date, data=m_tmn, method='ML', 
#            correlation=corAR1())
# summary(mod2)
# acf(residuals(mod2))

# arma = corARMA(c(0.2, 0.2), p=2, q=0)
# mod3 = gls(tmn ~ date, data=m_tmn, method='ML',
#            correlation=arma)
# summary(mod3)
# acf(residuals(mod3))
# save.image(file=file.path(getwd(), 'Modeling_R-Images', 
#                           sub(' ', '_', as.character(Sys.time()))))

# anova(mod2, mod3)

mod4 = lme(tmn ~ factor(unit_7x5)*elev, random=~1|site/date, 
           correlation=corAR1(form=~date), data=m_tmn,
           na.action=na.exclude, method='ML')
save.image(file=file.path(getwd(), 'Modeling_R-Images', 
                          sub(' ', '_', as.character(Sys.time()))))


# x11(width=17, height=10.3)
png(filename='/home/vitale232/Dropbox/UNR/UNR-Thesis/Figures/lapse-rates_tmn.png',
    width=17.5, height=10.3, units='in', res=200)
m_tmn$jday = paste(strftime(m_tmn$date, '%y'), strftime(m_tmn$date, '%j'), sep='-')
brks = c(2000, 2500, 3000, 3500)
labs = c(2000, '', 3000, '')
qplot(elev, tmn, data=m_tmn) + facet_wrap(~jday, nrow=10) + geom_smooth(method='lm') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks=brks, labels=labs) + labs(title='Lapse Rates, West Slope of Snake Range, NV', 
                                                      x='Elevation (m)', y='Minimum Temperature (°C)')

dev.off()

mod5 = lme(tmn ~ elev + irrad + cc_nlcd + tci, 
           random=~1|site/date, data=m_tmn,
           correlation=corAR1(form=~date),
           na.action=na.exclude, method='ML')
summary(mod5)

mod6 = lme(tmn ~ elev + irrad + cc_nlcd + tci, 
           random=~1|site/unit_7x5, data=m_tmn,
           correlation=corAR1(form=~date),
           na.action=na.exclude, method='ML')
summary(mod6)

mod7 = lme(tmn ~ elev + irrad + cc_nlcd + tci + tair, 
           random=~1|unit_7x5/site, data=m_tmn,
           correlation=corAR1(form=~date),
           na.action=na.exclude, method='ML')
summary(mod7)

mod8 = lme(tmn ~ tair + elev,
           random=list(~1|site, ~elev|date),
           data=m_tmn, 
           method='ML', na.action=na.exclude)
summary(mod8)



mod9 = lmer(tmn ~ tair + scale(elev)+(1|site)+(1+scale(elev)|date),
           data=m_tmn, REML=FALSE, na.action=na.omit, na.rm=TRUE)
summary(mod9)

mod10 = lmer(tmn ~ tair + scale(elev) + (1|site) + (1+scale(elev)|date) + (1+scale(elev)|unit_7x5),
             data=m_tmn, REML=FALSE, na.action=na.omit)
summary(mod10)
anova(mod9, mod10)
acf(resid(mod10))

mod11 = lmer(tmn ~ tair + scale(elev) + cc_nlcd + (1|site) + 
               (1+scale(elev)|date) + (1+scale(elev)|unit_7x5),
             data=m_tmn, REML=FALSE, na.action=na.omit)
summary(mod11)

mod12 = lmer(tmn ~ tair + scale(elev) + cc_nlcd*cc_nlcd_std_error + (1|site) + 
               (1+scale(elev)|date) + (1+scale(elev)|unit_7x5),
             data=m_tmn, REML=FALSE, na.action=na.omit)
summary(mod12)

anova(mod9, mod10, mod11, mod12)

mod13 = lmer(tmn ~ tair + scale(elev) + cc_nlcd + cc_nlcd:cc_nlcd_std_error + 
               (1|site) + (1+scale(elev)|date) + (1+scale(elev)|unit_7x5),
             data=m_tmn, REML=FALSE, na.action=na.omit)
summary(mod13)


mod14 = lmer(tmn ~ tair + scale(elev) + cc_nlcd + tci + 
               (1|site) + (1+scale(elev)|date) + (1+scale(elev)|unit_7x5),
             data=m_tmn, REML=FALSE, na.action=na.omit)
summary(mod14)

mod15 = lmer(tmn ~ tair + scale(elev) + cc_nlcd + tci + 
               (1|site) + (1+scale(elev)|date) + (1+scale(elev)|unit_7x5:date),
             data=m_tmn, REML=FALSE, na.action=na.omit)
summary(mod15)

anova(mod14, mod15)



#### WORKING WITH THE EOFs
m1 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC3 + PC4 + elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1|site)
summary(m1)

m2 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC3 + PC4 + elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1|site/date)
summary(m2)

m3 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC4 + elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1|site/date)
summary(m3)

m4 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + PC4 + elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1 |site/date)
summary(m4)
anova(m1, m2, m3, m4)

m5 = lme(tmn ~ tair + irrad*cc_nlcd + PC1 + PC2 + PC4 + elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1|site/date)
summary(m5)

m6 = lme(tmn ~ tair + cc_nlcd + PC1 + PC2 + PC4 + elev + tci, 
         data=m_tmn, na.action=na.omit, method='ML', random=~1|site/date)
summary(m6)

m7 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC4*elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1|site/date)
summary(m7)
anova(m1, m2, m3, m4, m5, m6, m7)

m8 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + PC4*elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1|site/date)
summary(m8)
anova(m1, m2, m3, m4, m5, m6, m7, m8)

m9 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + PC4*elev + tci,
         data=m_tmn, na.action=na.omit, method='ML', random=~1 + date|site)
summary(m9)
anova(m1, m2, m3, m4, m5, m6, m7, m8, m9)

m10 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC4*elev + tci,
          data=m_tmn, na.action=na.omit, method='ML', random=~1|site/date)
summary(m10)
anova(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)



#### Top down of min temp
mn1 = gls(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC3 + PC4 + elev + tci,
         data=m_tmn, na.action=na.exclude, method='ML')
summary(mn1)

mn2 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC3 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 | site)
summary(mn2)

mn3 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC3 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 + date| site)
summary(mn3)

mn4 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC3 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 |site/date)
summary(mn4)

mn5 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC3 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 |date/site)
summary(mn5)
anova(mn1, mn2, mn3, mn4, mn5)

mn6 = lme(tmn ~ date + tair + irrad + cc_nlcd + PC1 + PC2 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 | date/site)
summary(mn6)
anova(mn5, mn6)

mn7 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 | date/site)
summary(mn7)
anova(mn5, mn6, mn7)

acf(resid(mn7))

mn8 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 | site/date)
summary(mn8)
anova(mn5, mn6, mn7, mn8)

mn9 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + PC4 + elev + tci,
          data=m_tmn, na.action=na.exclude, method='ML', random=~1 | site/date,
          correlation=corAR1())
summary(mn9)
anova(mn5, mn6, mn7, mn8, mn9)

mn10 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + PC4 + elev + tci + m_tmn_lapse,
           data=m_tmn, na.action=na.exclude, method='ML', random=~1 | date/site)
summary(mn10)
anova(mn5, mn6, mn7, mn10)

mn11 = lme(tmn ~ tair + irrad + cc_nlcd + PC1 + PC2 + elev + tci + m_tmn_lapse,
           data=m_tmn, na.action=na.exclude, method='ML', random=~1 | date/site)
summary(mn11)
anova(mn5, mn6, mn7, mn9, mn10, mn11)

mn12 = lme(tmn ~ tair + irrad + elev*cc_nlcd + PC1 + PC2 + PC4 + tci,
           data=m_tmn, na.action=na.exclude, method='ML', random=~1 | date/site)
summary(mn12)
anova(mn7, mn12)

mn13 = lme(tmn ~ tair + irrad + elev + cc_nlcd + PC1 + PC2 + PC4 + tci,
           data=m_tmn, na.action=na.exclude, method='ML', random=~1 | date/site)
summary(mn13)
anova(mn13, mn12)
