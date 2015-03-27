# x11(height=8, width=13)
png('~/Dropbox/UNR/UNR-Thesis/Figures/elevation_tmx-tmn.png', 
    height=8, width=13, units='in', res=300)
par(mfrow=c(2,1), mar=c(5, 4, 0, 2) + 0.1)
plot(m_tmx[m_tmx$site == 'D4P03', ]$date, m_tmx[m_tmx$site == 'D4P03', ]$tmx, 
     type='l', ylim=c(-30, 40), xlab='Date', ylab='Maximum Temperature (°C)', col='blue', lwd=1.5)
lines(m_tmx[m_tmx$site == 'D1P02', ]$date, m_tmx[m_tmx$site == 'D1P02', ]$tmx, col = 'red', lty=1, lwd=1.5)

plot(m_tmn[m_tmn$site == 'D4P03', ]$date, m_tmn[m_tmn$site == 'D4P03', ]$tmn, 
     type='l', ylim=c(-30, 40), xlab='Date', ylab='Minimum Temperature (°C)', col='blue', lwd=1.5)
lines(m_tmn[m_tmn$site == 'D1P02', ]$date, m_tmn[m_tmn$site == 'D1P02', ]$tmn, col = 'red', lty=1, lwd=1.5)
legend('bottomleft', legend=c('1814 m', '3441 m'), lty=1, col=c('red', 'blue'), lwd=1.5)

dev.off()


setEPS(height=8, width=12)
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/elevation_tmx-tmn.eps')
par(mfrow=c(2,1), mar=c(5, 4, 0, 2) + 0.1)
plot(m_tmx[m_tmx$site == 'D4P03', ]$date, m_tmx[m_tmx$site == 'D4P03', ]$tmx, 
     type='l', ylim=c(-30, 40), xlab='Date', ylab='Maximum Temperature (°C)', col='blue', lwd=1.5,
     cex.axis=1.25, cex.lab=1.25)
lines(m_tmx[m_tmx$site == 'D1P02', ]$date, m_tmx[m_tmx$site == 'D1P02', ]$tmx, col = 'red', lty=1, lwd=1.5)

plot(m_tmn[m_tmn$site == 'D4P03', ]$date, m_tmn[m_tmn$site == 'D4P03', ]$tmn, 
     type='l', ylim=c(-30, 40), xlab='Date', ylab='Minimum Temperature (°C)', col='blue', lwd=1.5,
     cex.axis=1.25, cex.lab=1.25)
lines(m_tmn[m_tmn$site == 'D1P02', ]$date, m_tmn[m_tmn$site == 'D1P02', ]$tmn, col = 'red', lty=1, lwd=1.5)
legend('bottomleft', legend=c('1814 m', '3441 m'), lty=1, col=c('red', 'blue'), lwd=1.5)
dev.off()


start = as.character(unique(m_tmn$site))[1]
sites = as.character(unique(m_tmn$site))[-1]
setEPS(height=8, width=12)
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/all-sites_tmx-tmn.eps')
par(mfrow=c(2,1), mar=c(5, 4, 0, 2) + 0.1)
plot(m_tmx[m_tmx$site == start, ]$date, m_tmx[m_tmx$site ==start, ]$tmx, 
     type='l', ylim=c(-35, 50), xlab='Date', ylab='Maximum Temperature (°C)', col='blue', lwd=1.5,
     cex.axis=1.25, cex.lab=1.25)
for(site in sites){
  lines(m_tmx[m_tmx$site == site, ]$date, m_tmx[m_tmx$site == site, ]$tmx, col=which(sites==site))
}

plot(m_tmn[m_tmn$site == start, ]$date, m_tmn[m_tmn$site ==start, ]$tmn, 
     type='l', ylim=c(-35, 50), xlab='Date', ylab='Minimum Temperature (°C)', col='blue', lwd=1.5,
     cex.axis=1.25, cex.lab=1.25)
for(site in sites){
  lines(m_tmn[m_tmn$site == site, ]$date, m_tmn[m_tmn$site == site, ]$tmn, col=which(sites==site))
}
dev.off()


png(height=7, width=12.8, res=300, units='in',
    filename='~/Dropbox/UNR/UNR-Thesis/Figures/preds_tmn_2013-06-25.png')
plot(tmn_stack, 9)
dev.off()

png(height=7, width=12.8, res=300, units='in',
    filename='~/Dropbox/UNR/UNR-Thesis/Figures/preds_tmn_2013-12-15.png')
plot(tmn_stack, 182)
dev.off()

setEPS()
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/preds_lapse-tmn_2013-06-25.eps')
with(m_tmn[m_tmn$date == as.Date('2013-06-25'),], {
  plot(elev, tmn, xlab='elevation (km)', ylab='minimum temperature (°C)',
       cex.axis=1.25, cex.lab=1.25)
  abline(lm(tmn ~ elev), col=2, lty=2)
  abline(lm(tmn ~ elev)$coefficients[1], -6.5, col=1, lty=3)
  legend('topright', legend=c('regression line', 'standard lapse rate (-6.5°C/km)'), 
         lty=2:3, col=c('red', 'black'))})
dev.off()


setEPS()
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/preds_lapse-tmn_2013-12-15.eps')
with(m_tmn[m_tmn$date == as.Date('2013-12-15'),], {
  plot(elev, tmn, xlab='elevation (km)', ylab='minimum temperature (°C)',
       cex.axis=1.25, cex.lab=1.25)
  abline(lm(tmn ~ elev), col=2, lty=2)
  abline(10, -6.5, col=1, lty=3)
  legend('bottomright', legend=c('regression line', 'standard lapse rate (-6.5°C/km)'), 
         lty=2:3, col=c('red', 'black'))})
dev.off()


setEPS()
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/preds_lapse-tmx_2013-06-25.eps')
with(m_tmx[m_tmx$date == as.Date('2013-06-25'),], {
  plot(elev, tmx, xlab='elevation (km)', ylab='maximum temperature (°C)',
       main='2013-06-25', cex.axis=1.25, cex.lab=1.25)
  abline(lm(tmx ~ elev), col=2, lty=2)
  abline(40, -6.5, col=1, lty=3)
  legend('topright', legend=c('regression line', 'standard lapse rate (-6.5°C/km)'), 
         lty=2:3, col=c('red', 'black'))})
dev.off()

setEPS()
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/preds_lapse-tmx_2013-12-15.eps')
with(m_tmx[m_tmx$date == as.Date('2013-12-15'),], {
  plot(elev, tmx, xlab='elevation (km)', ylab='maximum temperature (°C)',
       cex.axis=1.25, cex.lab=1.25)
  abline(lm(tmx ~ elev), col=2, lty=2)
  abline(25, -6.5, col=1, lty=3)
  legend('topright', legend=c('regression line', 'standard lapse rate (-6.5°C/km)'), 
         lty=2:3, col=c('red', 'black'))})
dev.off()


png(height=7, width=12.8, res=300, units='in',
    filename='~/Dropbox/UNR/UNR-Thesis/Figures/preds_tmx_2013-06-25.png')
plot(tmx_stack, which(names(tmx_stack) == 'X2013.06.25_tmx'))
dev.off()

png(height=7, width=12.8, res=300, units='in',
    filename='~/Dropbox/UNR/UNR-Thesis/Figures/preds_tmx_2013-12-15.png')
plot(tmx_stack, which(names(tmx_stack) == 'X2013.12.15_tmx'))
dev.off()


setEPS(height=7, width=10)
postscript('~/Dropbox/UNR/UNR-Thesis/Figures/validation_tmx_pred-obs')
plot(df$date, df$sage, ylim=c(-10, 20),
     xlab='Date', ylab='prediction - observation (°C)')
points(df$date, df$pj, col='orange')
points(df$date, df$montane, col='darkgreen')
points(df$date, df$subalpine, col='blue')
abline(0, 0, lty=2)
legend('topleft', legend=c('Sagebrush West', 'Pinyon-Juniper', 
                           'Montane', 'Subalpine West'),
       col=c('black', 'orange', 'darkgreen', 'blue'),
       pch=1)
dev.off()
