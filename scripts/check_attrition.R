# checking respondent balance
# 0) preparation ----
# setwd('~/git/floodStorm/')

# Load all the data
load('data/final_prepped_data.RData')

# 1) identify retained versus attritioned ----
df <- w1df[which(w1df$survey_firm != 'Lucid'),]
df$attrition <- !df$unique %in% unique(mdf$unique_id)

# attrition stats
round(table(df$attrition)/nrow(df), 3)

# 2) check balance ----
# preparing balance examination
check_balance <- c('dem', 'ideo3', 'age', 'female', 'educ', 'hispanic', 'black', 'damage_degree',
                   'spend_relief', 'spend_mitigate', 'fed_tax', 'barrier_policy_support', 'clim_belief')

vnames <- c('Democrats', 'Ideology', 'Age', 'Female', 'Education', 'Hispanic', 'Black', 'Personal\nExperience\n(Harvey)',
            'Disaster Relief\nSpending', 'Climate Change\nMitigation\nSpending', 'Federal Carbon\nEmissions Tax',
            'Infrastructure\nImprovement\n(Flood Barrier)', 'Pro-Climate\nBelief')

# min-max scaling then t-test
i <- 0; balance_stats <- matrix(NA, nrow = length(check_balance), ncol = 3)
for(item in check_balance){
  i <- i + 1
  df[,item] <- rescale(df[,item])
  balance_stats[i,] <- summary(lm(as.formula(paste('attrition ~ ', item)), data = df))$coefficients[2,c(1,2,4)]
}

# calculating confidence intervals
balance_stats <- cbind(balance_stats, balance_stats[,1] - balance_stats[,2] * 1.96 , balance_stats[,1] + balance_stats[,2] * 1.96)

# plotting results
vs <- length(check_balance)
pdf('outputs/attrition_balance.pdf', height = vs * 0.9 + 4.5 * 0.2, width = 7.5)
par(mar = c(3,8,0,0) + 0.75)
plot.new()
plot.window(xlim = sort(c(max(abs(balance_stats[,c(4,5)])), -1*max(abs(balance_stats[,c(4,5)])))), ylim = c(1, vs))
axis(1, cex.axis = 1.3)
axis(2, at = vs:1, labels = vnames, tick = F, line = -0.5, las = 2, cex.axis = 1.3)
polygon(x = rep(c(-1,2,2,-1), floor(vs/2)), y = sort(rep(c(seq(1,vs-1) + 0.5), 2)), border = gray(0.9), col = gray(0.9))
abline(v = 0)
segments(x0 = balance_stats[,4], x1 = balance_stats[,5], y0 = 13:1, col = 1, lwd = 2, lty = 1, lend = 1)
points(balance_stats[,1], 13:1, col = 1, bg = 1, cex = 1.5, pch = 21, lwd = 2)
box()
title(xlab = 'Change in Probability of Attrition', line = 2.4, cex.lab = 1.3)
dev.off()

rm(list=ls())
