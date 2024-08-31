# 0) Preparation ----
# setwd('~/git/floodStorm/')

#---------------------------#
# 1) Panel data creation ----
#---------------------------#

# Loading data
df <- read.csv('data/texas_climate_attitudes.csv', as.is = T)
df$county <- tolower(df$county)
df$major_city <- tolower(df$major_city)
df$county <- gsub(' county', '', df$county)
load('data/outage2021_data.RData')   # data from PowerOutage.US

# 1.1) Subsetting to panel ----
# separate respondents by which combinations of surveys they participated in
twave <- ifelse(df$wave_id == 1, df$wave_id * 10, df$wave_id)
partinfo <- aggregate(twave, list(df$unique_id), sum) # partinfo of 12 means participated in both wave 1 and wave 2
cids <- partinfo[which(partinfo[,2] == 12),1]         # identify ids of participants who have partinfo of 12
df <- df[df$unique_id %in% cids,]

# 1.2) Merging outage data with panel ----
odf <- rbind(out20[[2]], out21[[2]])
odf$wave <- c(rep(1,nrow(out20[[2]])), rep(2,nrow(out21[[2]])))

# 1.2.1) Merge outage for counties without cities (i.e. at the aggregated level) ----
# `major_city` is respondent location; `city` is treatment assignment level (here (i.e. with county-level obs), it is always unknown)
df_cnties <- merge(df, data.frame(odf[which(odf$city == 'unknown'),], aggr = 1), by.x = c('county', 'wave_id'), by.y = c('county', 'wave'), all.x = T, all.y = F)
df_cnties <- df_cnties[which(df_cnties$aggr == 1),]

# 1.2.2) Merge outage for cities (i.e. not at the aggregated level) ----
# `major_city` is both respondent location and treatment assignment level; to make data interoperable with county-treatment obs, add new column called `city` identical to `major_city`
df_cties <- merge(df, data.frame(odf[which(odf$city != 'unknown'),], aggr = 0), by.x = c('county', 'major_city', 'wave_id'), by.y = c('county', 'city', 'wave'), all.x = T, all.y = F)
df_cties <- df_cties[which(df_cties$aggr == 0),]
df_cties$city <- df_cties$major_city

# Combine county and city individuals to 'merged data frame'
mdf <- rbind(df_cnties[, sort(colnames(df_cnties))], df_cties[, sort(colnames(df_cties))])

# Making the zip-associated region (level of treatment assignment)
mdf$gunit <- apply(mdf[,c('county', 'city')], 1, paste, collapse = '')

# Creating outage variable
mdf$mean_out <- apply(mdf[, paste('X', 13:21, sep = '')], 1, mean, na.rm = T)

# 1.2.3) Creating outage plot ----
pdf('outputs/outage_stats_points.pdf', height = 5, width = 15)
set.seed(262723)
par(mfrow = c(1,2))
par(mar = c(3,3,0,0.5) + 0.5)
plot.new()
plot.window(xlim = c(1,27), ylim = c(0,1))
polygon(x = c(12.5,12.5,21.5,21.5), y = c(-0.1,1.1,1.1,-0.1), border = gray(0.95), col = gray(0.95))
axis(1, at = seq(1, 28, by = 7), cex.axis = 1.3);axis(2, cex.axis = 1.3)
for(row in 1:nrow(odf[which(odf$wave == 2),])){
  points(jitter(1:27, amount = 0.35), jitter(unlist(odf[which(odf$wave == 1),][row, as.character(1:27)]), amount = 0.005),
         pch = '.', col = rgb(0,0,0,0.5), cex = 4)
}
lines(1:27, colMeans(odf[which(odf$wave == 1),][, as.character(1:27)], na.rm = T), lwd = 4, col = '#EB6E1F')
legend('topleft', fill = c('#EB6E1F', gray(.95)), legend = c('Mean Outage', 'Exposure Period'), cex = 1.3, bty = 'n')
title(xlab = 'Date (Feb 2020)', ylab = 'Proportion Out', line = 2.4, cex.lab = 1.3)
box()
par(mar = c(3,3.5,0,0) + 0.5)
plot.new()
plot.window(xlim = c(1,27), ylim = c(0,1))
polygon(x = c(12.5,12.5,21.5,21.5), y = c(-0.1,1.1,1.1,-0.1), border = gray(0.95), col = gray(0.95))
axis(1, at = seq(1, 28, by = 7), cex.axis = 1.3);axis(2, cex.axis = 1.3)
for(row in 1:nrow(odf[which(odf$wave == 2),])){
  points(jitter(1:27, amount = 0.35), jitter(unlist(odf[which(odf$wave == 2),][row, as.character(1:27)]), amount = 0.005),
         pch = '.', col = rgb(0,0,0,0.5), cex = 4)
}
lines(1:27, colMeans(odf[which(odf$wave == 2),][, as.character(1:27)], na.rm = T), lwd = 4, col = '#EB6E1F')
title(xlab = 'Date (Feb 2021)', ylab = 'Proportion Out', line = 2.4, cex.lab = 1.3)
box()
dev.off()
# https://www.texastribune.org/2021/02/21/texas-power-outage-grocery-stores-greg-abbott/
# https://doi.org/10.1016/j.erss.2021.102106

# 1.2.4) data cleaning ----
# Filling in missing values across waves
# i.e. assuming education, race, ethnicity, and gender do not meaningfully change across waves
mdf <- mdf[order(mdf$wave_id,mdf$unique_id),]                                   # standardizing order
mdf$educ <- rep(mdf$educ[which(mdf$wave_id == 1)],2)                            # Education
mdf$hispanic <- rep(mdf$hispanic[which(mdf$wave_id == 1)],2)                    # Hispanic
mdf$female <- rep(mdf$female[which(mdf$wave_id == 1)],2)                        # Female
mdf$black <- rep(mdf$black[which(mdf$wave_id == 1)], 2)                         # Black

# making zip code non-numeric
mdf$zip <- as.factor(mdf$zip_current)

# 1.2.5) Rescaling ----
mdf$spend_relief <- rescale(mdf$spend_relief)
mdf$spend_mitigate <- rescale(mdf$spend_mitigate)
mdf$fed_tax <- rescale(mdf$fed_tax)
mdf$snow_degree <- rescale(mdf$snow_degree)
mdf$infra_policy_support <- rescale(mdf$infra_policy_support)
mdf$barrier_policy_support <- rescale(mdf$barrier_policy_support)
mdf$env_policy_support <- rescale(mdf$env_policy_support)
mdf$damage_degree <- rescale(mdf$damage_degree)
mdf$climate_sci_trust <- rescale(mdf$climate_sci_trust)
mdf$risk_combined <- rescale(mdf$risk_combined)
mdf$retweet <- rescale(mdf$retweet)
mdf$like <- rescale(mdf$like)

#-------------------------#
# 2) DID data creation ----
#-------------------------#
# 2.1) Creating treatment * post variable ----
mdf$post <- mdf$wave_id - 1
mdf$mean_out <- rep(mdf$mean_out[which(mdf$wave_id == 2)], 2)
mdf$meanpost <- mdf$mean_out * mdf$post

# 2.2) Using pre-treatment partisanship ----
mdf_did <- mdf
mdf_did$dem <- rep(mdf_did$dem[which(mdf_did$wave_id == 1)], 2)

#----------------------------#
# 3) Wave 1 data creation ----
#----------------------------#
# Loading data
# w1df <- read.csv('data/texas_climate_attitudes.csv', as.is = T)
w1df <- read.csv('data/texas_climate_attitudes.csv', as.is = T)
w1df <- w1df[which(w1df$wave_id == 1),]                                                    # Subset to only wave 1

# 3.1) data cleaning ----
w1df$zip <- as.factor(w1df$zip_current)                                                    # Making zip code non-numeric

# 3.2) rescaling ----
w1df$damage_degree <- rescale(w1df$damage_degree)
w1df$retweet <- rescale(w1df$retweet)
w1df$like <- rescale(w1df$like)
w1df$spend_relief <- rescale(w1df$spend_relief)
w1df$spend_mitigate <- rescale(w1df$spend_mitigate)
w1df$fed_tax <- rescale(w1df$fed_tax)
w1df$barrier_policy_support <- rescale(w1df$barrier_policy_support)

#----------------------------------------#
# 4) Reporting sample characteristics ----
#----------------------------------------#
# 4.1) wave 1 ----
round(table(w1df$age_group)/nrow(w1df), 3)
round(table(w1df$gender)/nrow(w1df), 3)
round(table(ifelse(w1df$educ > 4, 1, 0))/nrow(w1df), 3)
round(table(w1df$dem)/nrow(w1df), 3)
table(w1df$survey_firm,w1df$dem)

# 4.2) wave 2 ----
w2df <- mdf[which(mdf$wave_id == 1),]
round(table(w2df$age_group)/nrow(w2df), 3)
round(table(w2df$gender)/nrow(w2df), 3)
round(table(ifelse(w2df$educ > 4, 1, 0))/nrow(w2df), 3)
round(table(w2df$dem)/nrow(w2df), 3)
table(mdf$survey_firm[which(mdf$wave_id==2)], mdf$dem[which(mdf$wave_id==2)], mdf$more_reward[which(mdf$wave_id==2)])

#------------------------------#
# 5) Write out cleaned data ----
#------------------------------#
save(mdf, mdf_did, w1df, file = 'data/final_prepped_data.RData')
rm(list=ls())
