# 0) Preparation ----
# setwd('~/git/floodStorm/')

# Load all the data
load('data/final_prepped_data.RData')

# manipulation check
round(prop.table(table(mdf$condition, mdf$manip), margin = 1), 2)

# timing check
quantile(mdf$treat_duration[which(mdf$wave_id == 2 & mdf$condition == 0)], c(0.25, 0.5, 0.75), na.rm = T)
quantile(mdf$treat_duration[which(mdf$wave_id == 2 & mdf$condition == 1)], c(0.25, 0.5, 0.75), na.rm = T)

rm(list = ls())
