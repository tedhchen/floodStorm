#-------------------#
# 0) Preparation ----
#-------------------#
# setwd('~/git/floodStorm/')
# library(marginaleffects)

# Loading all results
load('outputs/all_models.RData')

# # editing legend plotting function
# body(legend)[[60]][[3]][[7]][[3]]$'lend' <- 1

# Coefficient names
vnamemap <- c('Pro-Climate\nBelief', 'Disaster Relief\nSpending', 'Climate Change\nMitigation\nSpending', 'Federal Carbon\nEmissions Tax',
              'Infrastructure\nImprovement\n(Power Grid)', 'Consumption\nRestrictions', 'Trust in\nClimate Science', 'Climate Change\nRisk Perception',
              'Infrastructure\nImprovement\n(Flood Barrier)', 'Pro-Climate\nShare (Like)', 'Pro-Climate\nShare (Retweet)')
names(vnamemap) <- c('clim_belief', 'spend_relief', 'spend_mitigate', 'fed_tax', 'infra_policy_support','env_policy_support',
                     'climate_sci_trust', 'risk_combined', 'barrier_policy_support', 'like', 'retweet')

# study name for plot filenames
snamemap <- c('panel', 'wave2', 'wave1', 'experiment')
snamemap_title <- c('Panel', 'Wave 2', 'Wave 1', 'Experiment')

# mean printing function
meanPrint <- function(x, y, xbars, cex = 1){
  text(x = x, y = y, bquote(paste(bar(x)[R], ' = ', .(sprintf("%.2f", xbars[1])), ', ', bar(x)[D], ' = ', .(sprintf("%.2f", xbars[2])), sep = '')), pos = 2, cex = cex)
}

#-----------------------------#
# 1) Plotting main results ----
#-----------------------------#
# extract results
toOut <- function(model){
  treat <- gsub( " .*$", "", as.character(model$call[[2]])[3])
  outcome <- as.character(model$call[[2]])[2]
  if(class(model) == 'lm'){
    moddf <- model$model
  } else {
    moddf <- model.matrix(model, type = c('lhs', 'rhs'), collin.rm = F)
  }
  demvar <- moddf[, which(substr(colnames(moddf), 1, 4) == 'dem')]
  me <- as.data.frame(marginaleffects(model, variables = treat, newdata = datagrid(dem = c(0, 1))))
  me$means <- round(c(mean(moddf[demvar == 0, colnames(moddf) == outcome]), mean(moddf[demvar == 1, colnames(moddf) == outcome])), 2)
  me[, c('estimate', 'conf.low', 'conf.high', 'p.value', 'dem', 'means')]
}
out <- list()
for(i in 1:4){
  ms <- get(paste('m', i, sep = ''))
  out[[i]] <- list(array(unlist(lapply(ms, toOut)), c(2,6,length(ms))), unlist(lapply(ms, function(x)as.character(x$call[[2]])[2])))
}

# Plotting parameters
legendlines <- 2
legendlineheight <- 0.25 # fixed extra is for the top and bottom margins. each line is 0.2 inches
xxtra <- 0.65; yxtra <- 0.5
leftmar <- 8
botmar <- 4.5
ymarlines <- botmar + (yxtra * 2)
mainexpansion <- 1.1
ypadding <- 0.5

xlabels <- c('Marginal Effect of Geographic Exposure\n (2021 Winter Storms)',
             'Marginal Effect of Perceived Personal Experience\n(2021 Winter Storms)',
             'Marginal Effect of Perceived Personal Experience\n(Hurricane Harvey)',
             'Treatment Effect of Scientific Information\n(Attribution)')

# set coefficients to plot (2,3,4,1 for panel; 2,5,3,4,1 for wave 1, 2, and experiment; 6,7 for wave 1 aux; 6,7,8 for wave 2 and experiment aux)
# 1st set: cfi <- c(2,5,3,4,1); study in c(2,3,4); '_me_full_means.pdf'
# 2nd set: cfi <- c(2,3,4,1);   study in c(1);     '_me_full_means.pdf'
# 3rd set: cfi <- c(6,7);       study in c(3);     '_me_aux_means.pdf'
# 4th set: cfi <- c(6,7,8);     study in c(2,4);   '_me_aux_means.pdf'
figParams <- list(list(c(2,5,3,4,1), c(2,3,4), '_me_full_means.pdf'),
                  list(c(2,3,4,1), c(1), '_me_full_means.pdf'),
                  list(c(6,7), c(3), '_me_aux_means.pdf'),
                  list(c(6,7,8), c(2,4), '_me_aux_means.pdf'))
for(i in 1:length(figParams)){
  for(study in figParams[[i]][[2]]){ # study 1 is panel; 2 is wave 2; 3 is wave 1; 4 is scicomm experiment
    cfi <- figParams[[i]][[1]]
    vs <- length(cfi)
    mlist <- out[[study]]

    pdf(paste('outputs/', snamemap[study], figParams[[i]][[3]], sep = ''), height = ((vs + legendlineheight * legendlines) * mainexpansion) + (ymarlines * 0.2), width = 7.5)
    par(mar = c(botmar + yxtra, leftmar + xxtra, 0 + yxtra, 0 + xxtra))
    plot.new()
    if(study > 1){
      xlim <- c(-0.25, 1.0)
    } else {
      xlim <- c(min(0, range(c(mlist[[1]][,c(2,3),cfi]))), max(0, range(c(mlist[[1]][,c(2,3),cfi])))) * 1.1
    }
    plot.window(xlim = xlim, ylim = c(1 - ypadding - legendlineheight * legendlines, vs + ypadding), yaxs = 'i')
    polygon(x = rep(c(-1,2,2,-1), 4), y = sort(rep(c(seq(1,8) + 0.5), 2)), border = gray(0.95), col = gray(0.95))
    axis(1, cex.axis = 1.3); axis(2, at = vs:1, labels = vnamemap[mlist[[2]][cfi]], las = 2, tick = F, line = -0.5, cex.axis = 1.3)
    mtext('Model', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
    mtext('_____', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
    abline(v = 0, col = gray(0.7))
    segments(x0 = mlist[[1]][1 ,2, cfi], x1 = mlist[[1]][1 ,3, cfi], y0 = vs:1 + 0.12 * mainexpansion, col = 2, lwd = 2, lend = 1)
    segments(x0 = mlist[[1]][2 ,2, cfi], x1 = mlist[[1]][2 ,3, cfi], y0 = vs:1 - 0.12 * mainexpansion, col = 4, lwd = 2, lend = 1)
    points(x = mlist[[1]][1, 1, cfi], y = (vs:1) + 0.12 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
    points(x = mlist[[1]][2, 1, cfi], y = (vs:1) - 0.12 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
    polygon(x = c(-1,2,2,-1), y = c(-1, -1, 0.5, 0.5), col = 'white')
    for(j in vs:1){meanPrint(x = xlim[2] + diff(xlim) * 0.05, y = j - 0.4, xbars = mlist[[1]][,6,rev(cfi)][,j], cex = 1.3)}
    legend(x = xlim[2] + diff(xlim) * 0.05, y = -0.08 * mainexpansion, col = c(4, 2), lty = c(1,1), lwd = 2, legend = c('Democrats', 'Republicans'), bty = 'n', cex = 1.3, horiz = F, xjust = 1, yjust = 0)
    legend(x = xlim[1] - diff(xlim) * 0.1, y = -0.08 * mainexpansion, legend = snamemap_title[study], bty = 'n', cex = 1.3, yjust = 0)

    title(xlab = xlabels[study], line = 3.8, cex.lab = 1.3)
    box()
    dev.off()
  }
}

#-----------------------------------------#
# 2) Plotting wave 1 subset robustness ----
#-----------------------------------------#
# extract results
toOutsub <- function(model){
  outcome <- as.character(model$call[[2]])[2]
  moddf <- model$model
  demvar <- moddf[, which(substr(colnames(moddf), 1, 4) == 'dem')]
  me <- as.data.frame(marginaleffects(model, variables = 'damage_degree', newdata = datagrid(dem = c(0, 1))))
  me$means <- round(c(mean(moddf[demvar == 0, colnames(moddf) == outcome]), mean(moddf[demvar == 1, colnames(moddf) == outcome])), 2)
  me[, c('estimate', 'conf.low', 'conf.high', 'p.value', 'dem', 'means')]
}
mlist <- list(array(unlist(lapply(m3subset, toOutsub)), c(2,6,length(m3subset))), unlist(lapply(m3subset, function(x)as.character(x$call[[2]])[2])))

# Plotting parameters
cfi <- c(2,5,3,4,1) # set coefficients to plot (2,5,3,4,1 for wave 1)
vs <- length(cfi)
legendlines <- 2
legendlineheight <- 0.25 # fixed extra is for the top and bottom margins. each line is 0.2 inches
xxtra <- 0.65; yxtra <- 0.5
leftmar <- 8
botmar <- 4.5
ymarlines <- botmar + (yxtra * 2)
mainexpansion <- 1.1
ypadding <- 0.5

pdf(paste('outputs/wave1_me_full_means_w2sub.pdf', sep = ''), height = ((vs + legendlineheight * legendlines) * mainexpansion) + (ymarlines * 0.2), width = 7.5)
par(mar = c(botmar + yxtra, leftmar + xxtra, 0 + yxtra, 0 + xxtra))
plot.new()
xlim <- c(-0.2, 0.95)
plot.window(xlim = xlim, ylim = c(1 - ypadding - legendlineheight * legendlines, vs + ypadding), yaxs = 'i')
polygon(x = rep(c(-1,2,2,-1), 4), y = sort(rep(c(seq(1,8) + 0.5), 2)), border = gray(0.95), col = gray(0.95))
axis(1, cex.axis = 1.3); axis(2, at = vs:1, labels = vnamemap[mlist[[2]][cfi]], las = 2, tick = F, line = -0.5, cex.axis = 1.3)
mtext('Model', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
mtext('_____', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
abline(v = 0, col = gray(0.7))
segments(x0 = mlist[[1]][1 ,2, cfi], x1 = mlist[[1]][1 ,3, cfi], y0 = vs:1 + 0.12 * mainexpansion, col = 2, lwd = 2, lend = 1)
segments(x0 = mlist[[1]][2 ,2, cfi], x1 = mlist[[1]][2 ,3, cfi], y0 = vs:1 - 0.12 * mainexpansion, col = 4, lwd = 2, lend = 1)
points(x = mlist[[1]][1, 1, cfi], y = (vs:1) + 0.12 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
points(x = mlist[[1]][2, 1, cfi], y = (vs:1) - 0.12 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
polygon(x = c(-1,2,2,-1), y = c(-1, -1, 0.5, 0.5), col = 'white')
for(j in vs:1){meanPrint(x = xlim[2] + diff(xlim) * 0.05, y = j - 0.4, xbars = mlist[[1]][,6,rev(cfi)][,j], cex = 1.3)}
legend(x = xlim[2] + diff(xlim) * 0.05, y = -0.08 * mainexpansion, col = c(4, 2), lty = c(1,1), lwd = 2, legend = c('Democrats', 'Republicans'), bty = 'n', cex = 1.3, horiz = F, xjust = 1, yjust = 0)
legend(x = xlim[1] - diff(xlim) * 0.1, y = -0.08 * mainexpansion, legend = snamemap_title[study], bty = 'n', cex = 1.3, yjust = 0)
title(xlab = 'Marginal Effect of Perceived Personal Experience\n(Hurricane Harvey)', line = 3.8, cex.lab = 1.3)
box()
dev.off()

#-----------------------------------#
# 3) Plotting rewards robustness ----
#-----------------------------------#
# extract results
toOutr <- function(model){
  outcome <- as.character(model$call[[2]])[2]
  moddf <- model$model
  demvar <- moddf[, which(substr(colnames(moddf), 1, 4) == 'dem')]
  me <- as.data.frame(marginaleffects(model, variables = 'snow_degree', newdata = datagrid(more_reward = c(0, 1), dem = c(0, 1))))
  me$means <- round(c(mean(moddf[demvar == 0, colnames(moddf) == outcome]), mean(moddf[demvar == 1, colnames(moddf) == outcome])), 2)
  me[, c('estimate', 'conf.low', 'conf.high', 'p.value', 'dem', 'means', 'more_reward')]
}
mlist <- list(array(unlist(lapply(m2payment_large, toOutr)), c(4,7,length(m2payment_large))), unlist(lapply(m2payment_large, function(x)as.character(x$call[[2]])[2])))

# Plotting parameters
cfi <- c(2,5,3,4,1,6,7,8)
vs <- length(cfi)
legendlines <- 2
legendlineheight <- 0.25
xxtra <- 0.65; yxtra <- 0.5
leftmar <- 8
botmar <- 4.5
ymarlines <- botmar + (yxtra * 2)
mainexpansion <- 1.1
ypadding <- 0.5

pdf('outputs/wave2_me_reward.pdf', height = ((vs + legendlineheight * legendlines) * mainexpansion) + (ymarlines * 0.2), width = 7.5) # fixed extra 0.8 is for the top and bottom margins
par(mar = c(botmar + yxtra, leftmar + xxtra, 0 + yxtra, 0 + xxtra))
plot.new()
xlim <- range(c(mlist[[1]][,c(2,3),cfi]))
plot.window(xlim = xlim, c(1 - ypadding - legendlineheight * legendlines, vs + ypadding), yaxs = 'i')
polygon(x = rep(c(-1,2,2,-1), 4), y = sort(rep(c(seq(1,8) + 0.5), 2)), border = gray(0.95), col = gray(0.95))
axis(1, cex.axis = 1.3); axis(2, at = vs:1, labels = vnamemap[mlist[[2]][cfi]], las = 2, tick = F, line = -0.5, cex.axis = 1.3)
mtext('Model', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
mtext('_____', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
abline(v = 0, col = gray(0.7))
segments(x0 = mlist[[1]][1 ,2, cfi], x1 = mlist[[1]][1 ,3, cfi], y0 = (vs:1) + 0.24 * mainexpansion, col = 2, lwd = 2, lty = 1, lend = 1) # republican base reward
segments(x0 = mlist[[1]][3 ,2, cfi], x1 = mlist[[1]][3 ,3, cfi], y0 = (vs:1) + 0.08 * mainexpansion, col = 2, lwd = 2, lty = 2, lend = 1) # republican high reward
segments(x0 = mlist[[1]][2 ,2, cfi], x1 = mlist[[1]][2 ,3, cfi], y0 = (vs:1) - 0.08 * mainexpansion, col = 4, lwd = 2, lty = 1, lend = 1) # democrat base reward
segments(x0 = mlist[[1]][4 ,2, cfi], x1 = mlist[[1]][4 ,3, cfi], y0 = (vs:1) - 0.24 * mainexpansion, col = 4, lwd = 2, lty = 2, lend = 1) # democrat high reward
points(x = mlist[[1]][1, 1, cfi], y = (vs:1) + 0.24 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
points(x = mlist[[1]][3, 1, cfi], y = (vs:1) + 0.08 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
points(x = mlist[[1]][2, 1, cfi], y = (vs:1) - 0.08 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
points(x = mlist[[1]][4, 1, cfi], y = (vs:1) - 0.24 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
polygon(x = c(-1,4,4,-1), y = c(-1, -1, 0.5, 0.5), col = 'white')
for(j in vs:1){meanPrint(x = xlim[2] + diff(xlim) * 0.05, y = j - 0.4, xbars = mlist[[1]][,6,rev(cfi)][,j], cex = 1.3)}
text(x = xlim[1] - diff(xlim) * 0.01, y = (1:vs) + 0.33,
     labels = ifelse(sapply(1:8, function(x){lmtest::lrtest(m2payment_large[[x]],m2payment_small[[x]])[2,5]})[rev(cfi)] < 0.05, '*', ''), cex = 3)
legend(x = xlim[2] + diff(xlim) * 0.05, y = -0.08 * mainexpansion, col = c(1,1,4,2), lty = c(1,2,1,1), lwd = 2, legend = c('$2', '$4','Democrats', 'Republicans'),
       bty = 'n', cex = 1.3, horiz = F, xjust = 1, yjust = 0, ncol = 2)
legend(x = xlim[1] - diff(xlim) * 0.09, y = -0.08 * mainexpansion, legend = 'Wave 2', bty = 'n', cex = 1.3, yjust = 0)
title(xlab = paste('Marginal Effect of Perceived Personal Experience\n(2021 Winter Storms)', sep = ''), line = 3.8, cex.lab = 1.3)
box()
dev.off()

# Checking interaction effect with lrtest
for(i in 1:8){print(lrtest(m2payment_large[[i]],m2payment_small[[i]]))}

#------------------------------------------#
# 4) Plotting sci + exposure robustness ----
#------------------------------------------#
# sci facts by exposure
# Plotting parameters
cfi <- c(2,5,3,4,1)
vs <- length(cfi)
legendlines <- 2
legendlineheight <- 0.25
xxtra <- 0.65; yxtra <- 0.5
leftmar <- 8
botmar <- 4.5
ymarlines <- botmar + (yxtra * 2)
mainexpansion <- 1.1
ypadding <- 0.5

# extract results
toOutFacts <- function(model, hilo = c(0,5)){
  outcome <- as.character(model$call[[2]])[2]
  moddf <- model$model
  demvar <- moddf[, which(substr(colnames(moddf), 1, 4) == 'dem')]
  me <- as.data.frame(marginaleffects(model, variables = 'condition', newdata = datagrid(snow_degree = scales::rescale(seq(0,9))[hilo + 1], dem = c(0, 1))))
  me$means <- round(c(mean(moddf[demvar == 0, colnames(moddf) == outcome]), mean(moddf[demvar == 1, colnames(moddf) == outcome])), 2)
  me[, c('estimate', 'conf.low', 'conf.high', 'p.value', 'dem', 'means', 'snow_degree')]
}
hilorange <- c(0,5)
mlist <- list(array(unlist(lapply(m4sciexpose, toOutFacts, hilo = hilorange)), c(4,7,length(m4sciexpose))), unlist(lapply(m4sciexpose, function(x)as.character(x$call[[2]])[2])))

# Plotting
pdf('outputs/wave2_me_sciinfo2.pdf', height = ((vs + legendlineheight * legendlines) * mainexpansion) + (ymarlines * 0.2), width = 7.5) # fixed extra 0.8 is for the top and bottom margins
par(mar = c(botmar + yxtra, leftmar + xxtra, 0 + yxtra, 0 + xxtra))
plot.new()
xlim <- c(-0.25, 1.0)
plot.window(xlim = xlim, c(1 - ypadding - legendlineheight * legendlines, vs + ypadding), yaxs = 'i')
polygon(x = rep(c(-1,2,2,-1), 4), y = sort(rep(c(seq(1,8) + 0.5), 2)), border = gray(0.95), col = gray(0.95))
axis(1, cex.axis = 1.3); axis(2, at = vs:1, labels = vnamemap[mlist[[2]][cfi]], las = 2, tick = F, line = -0.5, cex.axis = 1.3)
mtext('Model', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
mtext('_____', side = 2, las = 2, line = 3.5, at = vs + 0.45, adj = 0.5, cex = 1.6)
abline(v = 0, col = gray(0.7))
segments(x0 = mlist[[1]][1 ,2, cfi], x1 = mlist[[1]][1 ,3, cfi], y0 = (vs:1) + 0.24 * mainexpansion, col = 2, lwd = 2, lty = 1, lend = 1) # republican control
segments(x0 = mlist[[1]][3 ,2, cfi], x1 = mlist[[1]][3 ,3, cfi], y0 = (vs:1) + 0.08 * mainexpansion, col = 2, lwd = 2, lty = 2, lend = 1) # republican sci info
segments(x0 = mlist[[1]][2 ,2, cfi], x1 = mlist[[1]][2 ,3, cfi], y0 = (vs:1) - 0.08 * mainexpansion, col = 4, lwd = 2, lty = 1, lend = 1) # democrat control
segments(x0 = mlist[[1]][4 ,2, cfi], x1 = mlist[[1]][4 ,3, cfi], y0 = (vs:1) - 0.24 * mainexpansion, col = 4, lwd = 2, lty = 2, lend = 1) # democrat sci info
points(x = mlist[[1]][1, 1, cfi], y = (vs:1) + 0.24 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
points(x = mlist[[1]][3, 1, cfi], y = (vs:1) + 0.08 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
points(x = mlist[[1]][2, 1, cfi], y = (vs:1) - 0.08 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
points(x = mlist[[1]][4, 1, cfi], y = (vs:1) - 0.24 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
polygon(x = c(-1,4,4,-1), y = c(-1, -1, 0.5, 0.5), col = 'white')
for(j in vs:1){meanPrint(x = xlim[2] + diff(xlim) * 0.05, y = j - 0.4, xbars = mlist[[1]][,6,rev(cfi)][,j], cex = 1.3)}
legend(x = xlim[2] + diff(xlim) * 0.05, y = -0.08 * mainexpansion, col = c(1,1,4,2), lty = c(1,2,1,1), lwd = 2, legend = c('No Exp.', 'Mod. Exp.','Democrats', 'Republicans'),
       bty = 'n', cex = 1.3, horiz = F, xjust = 1, yjust = 0, ncol = 2)
legend(x = xlim[1] - diff(xlim) * 0.09, y = -0.08 * mainexpansion, legend = c('Wave 2 &', 'Experiment'), bty = 'n', cex = 1.3, yjust = 0)
title(xlab = paste('Treatment Effect of Scientific Information (Attribution)\nby Perceived Personal Experience', sep = ''), line = 3.8, cex.lab = 1.3)
box()
dev.off()

# Checking interaction effect with lrtest
for(i in 1:5){print(lrtest(m4[[i]],m4sciexpose[[i]]))}

rm(list=ls())
