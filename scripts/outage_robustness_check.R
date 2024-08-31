# 0) Preparation ----
# setwd('~/git/floodStorm/')
# library(fixest)                      # v0.11.1
# library(marginaleffects)             # v0.9.0

# Loading data
df <- read.csv('data/texas_climate_attitudes.csv', as.is = T)
df$county <- tolower(df$county)
df$major_city <- tolower(df$major_city)
df$county <- gsub(' county', '', df$county)
load('data/outage2021_data_multithreshold.RData')   # data from PowerOutage.US
threshs <- seq(0, 0.5, by = 0.05) # from no aggregation (i.e. threshold @ 0) to up to 50% threshold


#---------------------#
# 1) Data creation ----
#---------------------#

# 1.1) Subsetting to panel ----
# separate respondents by which combinations of surveys they participated in
twave <- ifelse(df$wave_id == 1, df$wave_id * 10, df$wave_id)
partinfo <- aggregate(twave, list(df$unique_id), sum) # partinfo of 12 means participated in both wave 1 and wave 2
cids <- partinfo[which(partinfo[,2] == 12),1]         # identify ids of participants who have partinfo of 12
df <- df[df$unique_id %in% cids,]

# 1.2) Merging outage measure at different aggregation thresholds ----
mergeOutagePanel <- function(outage, df){
  out20 <- outage[[1]]; out21 <- outage[[2]]

  # Merging outage data with panel
  odf <- rbind(out20[[2]], out21[[2]])
  odf$wave <- c(rep(1,nrow(out20[[2]])), rep(2,nrow(out21[[2]])))

  #Merge outage for counties without cities (i.e. at the aggregated level)
  # `major_city` is respondent location; `city` is treatment assignment level (here (i.e. with county-level obs), it is always unknown)
  df_cnties <- merge(df, data.frame(odf[which(odf$city == 'unknown'),], aggr = 1), by.x = c('county', 'wave_id'), by.y = c('county', 'wave'), all.x = T, all.y = F)
  df_cnties <- df_cnties[which(df_cnties$aggr == 1),]

  # Merge outage for cities (i.e. not at the aggregated level)
  # `major_city` is both respondent location and treatment assignment level; to make data interoperable with county-treatment obs, add new column called `city` identical to `major_city`
  df_cties <- merge(df, data.frame(odf[which(odf$city != 'unknown'),], aggr = 0), by.x = c('county', 'major_city', 'wave_id'), by.y = c('county', 'city', 'wave'), all.x = T, all.y = F)
  df_cties <- df_cties[which(df_cties$aggr == 0),]
  df_cties$city <- df_cties$major_city

  # Combine county and city individuals to 'merged data frame'
  mdfr <- rbind(df_cnties[, sort(colnames(df_cnties))], df_cties[, sort(colnames(df_cties))])

  # Making the zip-associated region (level of treatment assignment)
  mdfr$gunit <- apply(mdfr[,c('county', 'city')], 1, paste, collapse = '')

  # Creating outage variable
  mdfr$mean_out <- apply(mdfr[, paste('X', 13:21, sep = '')], 1, mean, na.rm = T)

  # output
  mdfr[order(mdfr$wave_id, mdfr$unique_id), c('unique_id', 'wave_id', 'mean_out', 'aggr', 'gunit')]
}

# getting all respondent's exposures depending on threshold value (i.e. list of length of threshold values)
outages_thresholded <- lapply(out_threshs, mergeOutagePanel, df = df[, c('unique_id', 'wave_id', 'county', 'major_city')])

# merging exposures into main dataframe
mdf <- df[order(df$wave_id, df$unique_id),]
for(i in 1:length(threshs)){
  tdf <- outages_thresholded[[i]]
  colnames(tdf)[c(3,4,5)] <- paste(colnames(tdf)[c(3,4,5)], sprintf("%.2f", threshs[i]), sep = '')
  mdf <- merge(mdf, tdf, by = c('wave_id', 'unique_id'))
}

# number of aggregated (i.e. county) by level of thresholding
colSums(mdf[,grepl('aggr0.', colnames(mdf))])

# 1.3) Data cleaning ----
# Filling in missing values across waves
# i.e. assuming education, race, ethnicity, and gender do not meaningfully change across waves
mdf <- mdf[order(mdf$wave_id,mdf$unique_id),]                                   # standardizing order
mdf$educ <- rep(mdf$educ[which(mdf$wave_id == 1)],2)                            # Education
mdf$hispanic <- rep(mdf$hispanic[which(mdf$wave_id == 1)],2)                    # Hispanic
mdf$female <- rep(mdf$female[which(mdf$wave_id == 1)],2)                        # Female
mdf$black <- rep(mdf$black[which(mdf$wave_id == 1)], 2)                         # Black
mdf$dem <- rep(mdf$dem[which(mdf$wave_id == 1)], 2)                             # Partisanship using wave 1 (specifically for DID)

# making zip code non-numeric
mdf$zip <- as.factor(mdf$zip_current)

# Rescaling
mdf$spend_relief <- rescale(mdf$spend_relief)
mdf$spend_mitigate <- rescale(mdf$spend_mitigate)
mdf$fed_tax <- rescale(mdf$fed_tax)

# creating post variable
mdf$post <- mdf$wave_id - 1

#---------------------#
# 2) Model fitting ----
#---------------------#
# 2.1) Required functions ----
# likelihood ratio test for feols()
fixestLRTest <- function(mBig, mSmall){
  pchisq(q = 2*(fitstat(mBig, 'll')[[1]] - fitstat(mSmall, 'll')[[1]]), df = length(mBig$coefficients) - length(mSmall$coefficients), lower.tail = F)
}

# get subgroup counts for aggr vs not aggr
getAggrRatio <- function(model){
  moddf <- model.matrix(model)
  table(moddf[,'meanpost:aggr']/moddf[,'meanpost'])
}

# 2.2) Looping through different thresholds ----
ms <- list()              # container for baseline models
msAggr <- list()          # container for interaction models
aggrRatio <- list()       # container for subgroup counts
aggrtest <- list()        # container for likelihood ratio test
for(i in 1:length(threshs)){
  # loading outage measure
  mdf$mean_out <- rep(mdf[which(mdf$wave_id == 2), paste('mean_out', sprintf("%.2f", threshs[i]), sep = '')], 2)
  mdf$meanpost <- mdf$mean_out * mdf$post
  mdf$aggr <- rep(mdf[, paste('aggr', sprintf("%.2f", threshs[i]), sep = '')][which(mdf$wave_id == 2)], 2)

  # Belief in Anthropogenic Climate Change
  m1a <- feols(clim_belief ~ meanpost * dem | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  m1aA <- feols(clim_belief ~ meanpost * dem * aggr | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  aggrtestA <- fixestLRTest(m1aA, m1a)

  # Disaster relief spending
  m1b <- feols(spend_relief ~ meanpost * dem | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  m1bA <- feols(spend_relief ~ meanpost * dem * aggr | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  aggrtestB <- fixestLRTest(m1bA, m1b)

  # Climate change mitigation spending
  m1c <- feols(spend_mitigate ~ meanpost * dem | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  m1cA <- feols(spend_mitigate ~ meanpost * dem * aggr | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  aggrtestC <- fixestLRTest(m1cA, m1c)

  # Federal carbon tax
  m1d <- feols(fed_tax ~ meanpost * dem | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  m1dA <- feols(fed_tax ~ meanpost * dem * aggr | unique_id + post, mdf, cluster = paste('gunit', sprintf("%.2f", threshs[i]), sep = ''))
  aggrtestD <- fixestLRTest(m1dA, m1d)

  ms[[i]] <- list(m1a, m1b, m1c, m1d)
  msAggr[[i]] <- list(m1aA, m1bA, m1cA, m1dA)
  if(i > 1){aggrRatio[[i]] <- list(getAggrRatio(m1aA), getAggrRatio(m1bA), getAggrRatio(m1cA), getAggrRatio(m1dA))}
  aggrtest[[i]] <- list(aggrtestA, aggrtestB, aggrtestC, aggrtestD)
}

#----------------#
# 3) Analysis ----
#----------------#
# 3.1) Plot for model selection (whether to include aggregation) ----
offs <- c(-0.0075, -0.0025, 0.0025, 0.0075)
vnamemap <- c('Pro-Climate Belief', 'Disaster Relief Spending', 'Climate Change Mitigation Spending', 'Federal Carbon Emissions Tax')
pcols <- c("#46aa96FF", "#fcae91FF", "#dcdcdcFF", "#7828a0FF")
cfi <- c(2,3,4,1)

pdf('outputs/aggregation_model_selection.pdf', height = 5, width = 7.5)
par(mfrow = c(1,1))
par(mar = c(3,3.5,0,0) + 0.5)
plot.new()
plot.window(xlim = range(threshs[-1]), ylim = range(aggrtest[2:11]))
abline(h = 0.05, col = gray(0.7))
axis(1, at = threshs, cex.axis = 1.3); axis(2, cex.axis = 1.3)
oi = 0
for(i in cfi){
  oi <- oi + 1
  points(threshs + offs[oi], unlist(lapply(aggrtest, '[', i)), bg = pcols[i], lwd = 1, pch = 21, cex = 1.5)
}
legend('left', legend = vnamemap[cfi], fill = pcols[cfi], cex = 1.3, bty = 'n')
text(x = 0.15, y = 0.09, labels = '0.05', cex = 1.3, col = gray(0.7))
box()
title(ylab = expression(paste(italic(p), '-Value, Likelihood Ratio Test for Model Selection          ', sep = '')), xlab = 'Aggregation Threshold', line = 2.4, cex.lab = 1.3)
dev.off()

# 3.2) threshold robustness for all outcomes ----
# function for getting results
toOutr <- function(model, checkAggr = FALSE){
  outcome <- as.character(model$call[[2]])[2]
  moddf <- model.matrix(model, type = c('lhs', 'rhs'), collin.rm = F)
  demvar <- moddf[, which(colnames(moddf) == 'dem')]

  if(checkAggr){
    me <- as.data.frame(marginaleffects(model, variables = 'meanpost', newdata = datagrid(aggr = c(0, 1), dem = c(0, 1))))
    me$means <- round(c(mean(moddf[demvar == 0, colnames(moddf) == outcome]), mean(moddf[demvar == 1, colnames(moddf) == outcome])), 2)
    me[, c('estimate', 'conf.low', 'conf.high', 'p.value', 'dem', 'means', 'aggr')]
  } else {
    me <- as.data.frame(marginaleffects(model, variables = 'meanpost', newdata = datagrid(dem = c(0, 1))))
    me$means <- round(c(mean(moddf[demvar == 0, colnames(moddf) == outcome]), mean(moddf[demvar == 1, colnames(moddf) == outcome])), 2)
    me[, c('estimate', 'conf.low', 'conf.high', 'p.value', 'dem', 'means')]
  }
}

# running function
out <- list()   # container for all outcome
outr <- list()  # container for likelihood ratio test significant outcomes (disaster relief and carbon tax)
for(i in 1:length(threshs)){
  out[[i]] <- lapply(ms[[i]], toOutr)
  if(i > 1){outr[[i]] <- lapply(msAggr[[i]][c(2,4)], toOutr, checkAggr = T)}
}

# variable name mapping for polished plotting
vnamemap <- c('Pro-Climate Belief', 'Disaster Relief Spending', 'Climate Change Mitigation Spending', 'Federal Carbon Emissions Tax')
fnamemap <- c('clim_belief', 'spend_relief', 'spend_mitigate', 'fed_tax')

meanPrint <- function(x, y, xbars, cex = 1){
  text(x = x, y = y, bquote(paste(bar(x)[R], ' = ', .(sprintf("%.2f", xbars[1])), ', ', bar(x)[D], ' = ', .(sprintf("%.2f", xbars[2])), sep = '')), pos = 2, cex = cex)
}

# plotting
out <- out[c(2,4,6,8,10)]

cfi <- c(2,3,4,1); # set coefficients to plot
vs <- length(threshs[c(2,4,6,8,10)]) # subsetting to .05, .15, .25, .35, .45
legendlines <- 2
legendlineheight <- 0.25 # fixed extra is for the top and bottom margins. each line is 0.2 inches
xxtra <- 0.65; yxtra <- 0.5
leftmar <- 3
botmar <- 4.5
ymarlines <- botmar + (yxtra * 2) # each line is 0.2 inches
mainexpansion <- 1.1
ypadding <- 0.5

for(outcome in cfi){
  mlist <- lapply(out, '[[', outcome)
  mlist <- mlist[vs:1]

  pdf(paste('outputs/aggr_', fnamemap[outcome], '.pdf', sep = ''), height = ((vs + legendlineheight * legendlines) * mainexpansion) + (ymarlines * 0.2), width = 6.5)
  par(mar = c(botmar + yxtra, leftmar + xxtra, 0 + yxtra, 0 + xxtra))
  plot.new()
  xlim <- range(unlist(lapply(out, function(x){lapply(x, function(y)y[,c(2,3)])})))
  plot.window(xlim = xlim, ylim = c(1 - ypadding - legendlineheight * legendlines, vs + ypadding), yaxs = 'i')
  axis(1, cex.axis = 1.3); axis(2, at = 1:vs, labels = sprintf("%.2f", threshs[c(2,4,6,8,10)]), tick = F, line = -0.5, cex.axis = 1.3)
  polygon(x = rep(c(-1,2,2,-1), floor(vs/2)), y = sort(rep(c(1:(floor(vs/2)*2) + 0.5), 2)), border = gray(0.9), col = gray(0.9))
  polygon(x = c(-1,2,2,-1), y = c(2.5,2.5,3.5,3.5), lwd = 2, col = rgb(1, 0.8, 0, 0.2), border = rgb(1,0.8,0, 0.2))
  abline(v = 0, col = gray(0.7))
  segments(x0 = unlist(lapply(mlist, function(x)x[1,2])), x1 = unlist(lapply(mlist, function(x)x[1,3])), y0 = (vs:1) + 0.12 * mainexpansion, col = 2, lwd = 2, lend = 1)
  segments(x0 = unlist(lapply(mlist, function(x)x[2,2])), x1 = unlist(lapply(mlist, function(x)x[2,3])), y0 = (vs:1) - 0.12 * mainexpansion, col = 4, lwd = 2, lend = 1)
  points(x = unlist(lapply(mlist, function(x)x[1,1])), y = (vs:1) + 0.12 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
  points(x = unlist(lapply(mlist, function(x)x[2,1])), y = (vs:1) - 0.12 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
  polygon(x = c(-1,2,2,-1), y = c(-1, -1, 0.5, 0.5), col = 'white')
  meanPrint(x = xlim[2] + diff(xlim) * 0.05, y = 1 -0.4, xbars = mlist[[1]][,6], cex = 1.3)
  legend(x = xlim[2] + diff(xlim) * 0.05, y = -0.08 * mainexpansion, col = c(4, 2), lty = c(1,1), lwd = 2, legend = c('Democrats', 'Republicans'), bty = 'n', cex = 1.3, horiz = F, xjust = 1, yjust = 0)
  legend(x = xlim[1] - diff(xlim) * 0.1, y = -0.08 * mainexpansion, legend = 'Panel', bty = 'n', cex = 1.3, yjust = 0)
  title(xlab = paste('Marginal Effect of Geographic Exposure (2021 Winter Storms)      \non ', vnamemap[outcome], sep = ''), line = 3.8, cex.lab = 1.3)
  title(ylab = 'Aggregation Threshold', line = 2.1, cex.lab = 1.3)
  box()
  dev.off()
}

# 3.3) plotting outcomes that are statistically significant in the likelihood ratio test ----
# subsetting to .05, .15, .25, .35, .45
outr <- outr[c(2,4,6,8,10)]
aggrRatio <- aggrRatio[c(2,4,6,8,10)]
aggrtest <- aggrtest[c(2,4,6,8,10)]
aggrtest <- lapply(aggrtest, '[', c(2,4))

# plotting utility functio to print subgroup counts
aggrRatioPrint <- function(x, y, ns, cex = 1){
  text(x = x, y = y, bquote(paste(n[s], ' = ', .(ns[1]), ', ', n[l], ' = ', .(ns[2]), sep = '')), pos = 2, cex = cex)
}

# variable name mapping for polished plotting
vnamemap <- c('Disaster Relief Spending', 'Federal Carbon Emissions Tax')
fnamemap <- c('spend_relief', 'fed_tax')

# plotting
vs <- length(outr)
legendlines <- 2
legendlineheight <- 0.25 # fixed extra is for the top and bottom margins. each line is 0.2 inches
xxtra <- 0.65; yxtra <- 0.5
leftmar <- 3
botmar <- 4.5
ymarlines <- botmar + (yxtra * 2) # each line is 0.2 inches
mainexpansion <- 1.1
ypadding <- 0.5

for(outcome in c(1,2)){
  mlist <- lapply(outr, '[[', outcome)
  mlist <- mlist[vs:1]
  aggrRatioPlot <- lapply(aggrRatio, '[[', 1)[vs:1]

  pdf(paste('outputs/aggrsplit_', fnamemap[outcome], '.pdf', sep = ''), height = ((vs + legendlineheight * legendlines) * mainexpansion) + (ymarlines * 0.2), width = 6.5)
  par(mar = c(botmar + yxtra, leftmar + xxtra, 0 + yxtra, 0 + xxtra))
  plot.new()
  xlim <- range(unlist(lapply(outr, function(x){lapply(x, function(y)y[,c(2,3)])})))
  plot.window(xlim = xlim, ylim = c(1 - ypadding - legendlineheight * legendlines, vs + ypadding), yaxs = 'i')
  axis(1, cex.axis = 1.3); axis(2, at = 1:vs, labels = sprintf("%.2f", threshs[c(2,4,6,8,10)]), tick = F, line = -0.5, cex.axis = 1.3)
  polygon(x = rep(c(-1,4,4,-1), floor(vs/2)), y = sort(rep(c(1:(floor(vs/2)*2) + 0.5), 2)), border = gray(0.9), col = gray(0.9))
  abline(v = 0, col = gray(0.7))

  segments(x0 = unlist(lapply(mlist, function(x)x[1,2])), x1 = unlist(lapply(mlist, function(x)x[1,3])), y0 = (vs:1) + 0.24 * mainexpansion, col = 2, lwd = 2, lty = 1, lend = 1) # republican nonaggr
  segments(x0 = unlist(lapply(mlist, function(x)x[3,2])), x1 = unlist(lapply(mlist, function(x)x[3,3])), y0 = (vs:1) + 0.08 * mainexpansion, col = 2, lwd = 2, lty = 2, lend = 1) # republican aggr
  segments(x0 = unlist(lapply(mlist, function(x)x[2,2])), x1 = unlist(lapply(mlist, function(x)x[2,3])), y0 = (vs:1) - 0.08 * mainexpansion, col = 4, lwd = 2, lty = 1, lend = 1) # democrat nonaggr
  segments(x0 = unlist(lapply(mlist, function(x)x[4,2])), x1 = unlist(lapply(mlist, function(x)x[4,3])), y0 = (vs:1) - 0.24 * mainexpansion, col = 4, lwd = 2, lty = 2, lend = 1) # democrat aggr
  points(x = unlist(lapply(mlist, function(x)x[1,1])), y = (vs:1) + 0.24 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
  points(x = unlist(lapply(mlist, function(x)x[3,1])), y = (vs:1) + 0.08 * mainexpansion, col = 2, bg = 2, cex = 1.5, pch = 21, lwd = 2)
  points(x = unlist(lapply(mlist, function(x)x[2,1])), y = (vs:1) - 0.08 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)
  points(x = unlist(lapply(mlist, function(x)x[4,1])), y = (vs:1) - 0.24 * mainexpansion, col = 4, bg = 4, cex = 1.5, pch = 21, lwd = 2)

  polygon(x = c(-1,4,4,-1), y = c(-1, -1, 0.5, 0.5), col = 'white')
  meanPrint(x = xlim[2] + diff(xlim) * 0.05, y = vs + 0.35, xbars = mlist[[1]][c(1,2),6], cex = 1.3)
  for(j in 1:vs){aggrRatioPrint(x = xlim[2] + diff(xlim) * 0.05, y = vs + 0.6 - j, ns = aggrRatioPlot[[j]], cex = 1.3)}
  legend(x = xlim[2] + diff(xlim) * 0.05, y = -0.08 * mainexpansion, col = c(1,1,4,2), lty = c(1,2,1,1), lwd = 2, legend = c('City Level', 'County Level','Democrats', 'Republicans'),
         bty = 'n', cex = 1.3, horiz = F, xjust = 1, yjust = 0, ncol = 2)
  legend(x = xlim[1] - diff(xlim) * 0.1, y = -0.08 * mainexpansion, legend = 'Panel', bty = 'n', cex = 1.3, yjust = 0)

  text(x = xlim[1] - diff(xlim) * 0.01, y = (1:vs) + 0.33, labels = ifelse(unlist(lapply(aggrtest, '[', outcome)) < 0.05, '*', ''), cex = 3)
  title(xlab = paste('Marginal Effect of Geographic Exposure (2021 Winter Storms)      \non ', vnamemap[outcome], sep = ''), line = 3.8, cex.lab = 1.3)
  title(ylab = 'Aggregation Threshold', line = 2.1, cex.lab = 1.3)
  box()
  dev.off()
}

rm(list=ls())
