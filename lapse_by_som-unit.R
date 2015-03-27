# png('~/Dropbox/UNR/UNR-Thesis/Figures/lapse-by-SOM.png', 
#     height=10.25, width=17, units='in', res=300)

source('/home/vitale232/Dropbox/UNR/UNR-Thesis/Data/Thesis-Code/Exploratory_Analysis.R')

lapse = data.frame('date'=m_tmx$date, m_tmx_lapse,
                   'unit'=mapped_7x5$unit,
                   'dist'=mapped_7x5$distance)
x11(height=8, width=17)
plot(lapse$date, lapse$m_tmx_lapse, xlab='Date', ylab='Lapse Rate (°C/km)',
     type='l', col='lightgray')

text(lapse$date, lapse$m_tmx_lapse, paste0('|', as.character(lapse$unit), '|'))

legend('topleft', legend='1-30    SOM Unit')

# dev.off()
