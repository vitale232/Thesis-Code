#--------------------------------
# Name:         plots_create-lapse-plot.R
# Purpose:      Create the plot of lapse rates for the defense presentation
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/04/04
# R:            3.1.2
#--------------------------------
library(ggplot2)

source('~/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

m_tmn$Lapse_ Rate = cut(m_tmn$m_tmn_lapse, 4)

fig1 = ggplot(m_tmn) +
  geom_hline(yintercept=0, lty=2) +
  geom_line(aes(date, PC4)) + 
  geom_point(aes(date, PC4, color=lapse_cuts), size=2.5) +
  ggtitle('PC4 and Lapse Rates across Time') +
  xlab('Date') +
  theme_gray(base_size=24) +
  theme(legend.position='bottom',
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black')) 
print(fig1)
