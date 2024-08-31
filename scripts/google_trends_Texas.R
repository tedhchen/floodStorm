# 0) preparation ----
# setwd('~/git/floodStorm/')
# library(bcp)                 # v4.0.3
# # library(gtrendsR)          # v1.5.1 # This was used to collect data but we now use the archived data

# 1) accessing google trends data ----
COLORS <- c('#7b3294', '#EB6E1F', '#008837')
TERMS <- c('hurricane', 'astros', 'power')
TERMS_names <- c('\"hurricane\"', '\"astros\"', '\"power\"')

# Getting Google trend data
# df <- gtrends(TERMS, geo="US-TX", time="2017-01-01 2021-12-31", low_search_volume=T)

# 1.1) archiving data to ensure reproduction ----
# save(df, file = 'data/gtrends_archive.RData')
load('data/gtrends_archive.RData')

# 1.2) data cleaning ----
interests <- df$interest_over_time
interests$hits <- ifelse(interests$hits == '<1', 0.5, as.numeric(interests$hits))

# plotting
pdf('outputs/gtrends_tricomparison.pdf', height = 5, width = 7.5)
par(mar = c(3,3,0,0) + 0.5)
plot.new()
plot.window(xlim = c(0.5, 0.5 + nrow(interests)/3), ylim = c(0, 110), xaxs = 'i')
axis(1, at = seq(1, 260, by = 52) + 25.5, labels = seq(2017, 2021), tick = F, line = -0.5, cex.axis = 1.3); axis(2, cex.axis = 1.3)
polygon(sort((seq(1, 260, by = 52) - 0.5)[rep(2:5, 2)]), rep(c(-10,120,120,-10), 2), col = grey(0.95), border = grey(0.95))
for(i in c(3,1,2)){
  lines(c(0.5, 2:260, 261.5), bcp(y = interests$hits[interests$keyword == TERMS[i]])$posterior.mean, col = COLORS[i], lwd = 3)
}
text(x = c(2, 45, 200), y = c(105, 90, 85), gsub(' ', '\n', TERMS_names), pos = c(4, 4, 4), col = COLORS, cex = 1.3)
box()
title(xlab = 'Year', ylab = 'Relative Interest', line = 2.4, cex.lab = 1.3)
dev.off()

rm(list= ls())
