# 0) preparation ----
# setwd('~/git/floodStorm/')

# Load all the data
load('data/final_prepped_data.RData')

# 1) distribution of perceived personal experiences by partisanship ----
# density estimation
d2r <- density(mdf$snow_degree[which(mdf$wave_id == 2 & mdf$dem == 0)], na.rm = T, bw = 0.065, from = 0, to = 1)
d2d <- density(mdf$snow_degree[which(mdf$wave_id == 2 & mdf$dem == 1)], na.rm = T, bw = 0.065, from = 0, to = 1)
d1r <- density(w1df$damage_degree[which(w1df$dem == 0)], na.rm = T, bw = 0.065, from = 0, to = 1)
d1d <- density(w1df$damage_degree[which(w1df$dem == 1)], na.rm = T, bw = 0.065, from = 0, to = 1)
ds <- list(d1d, d2d, d1r, d2r)

# function to make proper polygon plot
toPolygon <- function(d){
  d$y <- c(0, d$y, 0)
  d$x <- c(0, d$x, 1)
  d
}

pdf('outputs/self_reported_exposure.pdf', height = 5, width = 7.5)
par(mfrow = c(1,2))
par(mar = c(4.5, 3, 0, 0.5) + 0.5)
for(i in c(1,2)){
  plot.new()
  plot.window(xlim = c(0,1), ylim = range(c(d1r$y, d1d$y, d2r$y, d2d$y)))
  axis(1, at = seq(0,1, by = 0.25), cex.axis = 1.3); axis(2, cex.axis = 1.3)
  polygon(toPolygon(ds[[i]]), border = NA, col = adjustcolor(1, 0.1))
  lines(ds[[i]], col = 4, lwd = 3, lty = 5, lend = 1)
  title(xlab = paste('Perceived Personal Experience \n(', c('Hurricane Harvey', '2021 Winter Storms')[i], ')', sep = ''), line = 3.9, cex.lab = 1.3)
  title(ylab = 'Density Estimate', line = 2.4, cex.lab = 1.3)
  polygon(toPolygon(ds[[i + 2]]), border = NA, col = adjustcolor(1, 0.1))
  lines(ds[[i + 2]], col = 2, lwd = 3, lty = 5, lend = 1)
  abline(h = 0, col = gray(0.5), lwd = 1)
  legend('topright', legend = paste('Wave', i), cex = 1.3, bty = 'n')
  box()
}
legend('topleft', fill = c(4, 2), legend = c('Democrats', 'Republicans'), cex = 1.3, bty = 'n')
dev.off()

# 2) distribution of hurricane harvey experiences ----
dmgs <- list(table(w1df$threat_finance), table(w1df$threat_health), table(w1df$property_damage))

pdf('outputs/harvey_damage.pdf', height = 5, width = 7.5)
par(mfrow = c(1,1))
par(mar = c(3,3,0,0) + 0.5)
plot.new()
plot.window(xlim = c(-0.5, 4.5), ylim = c(0, max(unlist(dmgs))))
axis(1, at = 0:4, labels = c('Not at all\n(0)', 'A little\n(1)', 'Moderately\n(2)', 'A lot\n(3)', 'A great deal\n(4)'), tick = F, line = 1, cex.axis = 1.3)
axis(2, cex.axis = 1.3)
for(i in 1:3){
  segments(x0 = (0:4) + c(-0.15, 0, 0.15)[i], y0 = 0, y1 = dmgs[[i]], col = c('#7b3294', '#EB6E1F', '#008837')[i], lwd = 10, lend = 1)
}
abline(h = 0, col = gray(0.5), lwd = 1)
legend('topright', fill = c('#7b3294', '#EB6E1F', '#008837'), legend = c('Threat to personal finance', 'Threat to personal health', 'Damage to personal property'), cex = 1.3, bty = 'n')
box()
title(ylab = 'Count', line = 2.4, cex.lab = 1.3)
dev.off()

# 3) percentage reporting experiences for 2021 winter storms ----
round(sapply(1:13, function(x){sum(mdf[,paste('texas_snow', x, sep = '')],na.rm=T)})/305, 3)

rm(list=ls())
