# 0) Preparation ----
# setwd('~/git/floodStorm/')
# library(fixest)                      # v0.11.1
# library(marginaleffects)             # v0.9.0

# Load all the data
load('data/final_prepped_data.RData')

#-------------#
# 1) PANEL ----
#-------------#
# Belief in Anthropogenic Climate Change
m1a <- feols(clim_belief ~ meanpost * dem | unique_id + post, mdf_did, cluster = 'gunit')

# Disaster relief spending
m1b <- feols(spend_relief ~ meanpost * dem | unique_id + post, mdf_did, cluster = 'gunit')

# Climate change mitigation spending
m1c <- feols(spend_mitigate ~ meanpost * dem | unique_id + post, mdf_did, cluster = 'gunit')

# Federal carbon tax
m1d <- feols(fed_tax ~ meanpost * dem | unique_id + post, mdf_did, cluster = 'gunit')

m1 <- list(m1a, m1b, m1c, m1d)

#--------------#
# 2) WAVE 2 ----
#--------------#
# additive snowstorm experience

# Belief in Anthropogenic Climate Change
m2a <- lm(clim_belief ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Disaster relief spending
m2b <- lm(spend_relief ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Climate change mitigation spending
m2c <- lm(spend_mitigate ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Federal carbon tax
m2d <- lm(fed_tax ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# power grid
m2e <- lm(infra_policy_support ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# personal consumption
m2f <- lm(env_policy_support ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# climate science trust
m2g <- lm(climate_sci_trust ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# risk perception
m2h <- lm(risk_combined ~ snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

m2 <- list(m2a, m2b, m2c, m2d, m2e, m2f, m2g, m2h)

#------------------#
# 3) EXPERIMENT ----
#------------------#
# information experiment conditions (w/ and w/o climate message)

# Belief in Anthropogenic Climate Change
m4a <- lm(clim_belief ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Disaster relief spending
m4b <- lm(spend_relief ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Climate change mitigation spending
m4c <- lm(spend_mitigate ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Federal carbon tax
m4d <- lm(fed_tax ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# power grid
m4e <- lm(infra_policy_support ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# personal consumption
m4f <- lm(env_policy_support ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# climate science trust
m4g <- lm(climate_sci_trust ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# risk perception
m4h <- lm(risk_combined ~ condition * dem + snow_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

m4 <- list(m4a, m4b, m4c, m4d, m4e, m4f, m4g, m4h)

#--------------#
# 4) WAVE 1 ----
#--------------#
# additive harvey experience with same variables as panel

# Belief in Anthropogenic Climate Change
m3a <- lm(clim_belief ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = w1df)

# Disaster relief spending
m3b <- lm(spend_relief ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = w1df)

# Climate change mitigation spending
m3c <- lm(spend_mitigate ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = w1df)

# Federal carbon tax
m3d <- lm(fed_tax ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = w1df)

# barrier
m3e <- lm(barrier_policy_support ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = w1df)

# Social media activism
m3f <- lm(like ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = w1df)
m3g <- lm(retweet ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = w1df)

m3 <- list(m3a, m3b, m3c, m3d, m3e, m3f, m3g)

#---------------------------------#
# 5) Robustness: wave 1 subset ----
#---------------------------------#
# Testing for differences in wave 1 results based on wave 2 subset

# Belief in Anthropogenic Climate Change
m3h <- lm(clim_belief ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Disaster relief spending
m3i <- lm(spend_relief ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Climate change mitigation spending
m3j <- lm(spend_mitigate ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Federal carbon tax
m3k <- lm(fed_tax ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# barrier
m3l <- lm(barrier_policy_support ~ damage_degree * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

m3subset <- list(m3h, m3i, m3j, m3k, m3l)

#---------------------------#
# 6) Robustness: payment ----
#---------------------------#
# Testing for difference in wave 2 results based on payment

# 6.1) Without interaction term ----
# Belief in Anthropogenic Climate Change
m2i <- lm(clim_belief ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Disaster relief spending
m2j <- lm(spend_relief ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Climate change mitigation spending
m2k <- lm(spend_mitigate ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Federal carbon tax
m2l <- lm(fed_tax ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# power grid
m2m <- lm(infra_policy_support ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# personal consumption
m2n <- lm(env_policy_support ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# climate science trust
m2o <- lm(climate_sci_trust ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

# risk perception
m2p <- lm(risk_combined ~ snow_degree * dem + more_reward * dem + ideo3 + age + female + educ + hispanic + black, data = mdf)

m2payment_small <- list(m2i, m2j, m2k, m2l, m2m, m2n, m2o, m2p)

# 6.2) With interaction term ----
# Belief in Anthropogenic Climate Change
m2q <- lm(clim_belief ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Disaster relief spending
m2r <- lm(spend_relief ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Climate change mitigation spending
m2s <- lm(spend_mitigate ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Federal carbon tax
m2t <- lm(fed_tax ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

# power grid
m2u <- lm(infra_policy_support ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

# personal consumption
m2v <- lm(env_policy_support ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

# climate science trust
m2w <- lm(climate_sci_trust ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

# risk perception
m2x <- lm(risk_combined ~ snow_degree * dem * more_reward + ideo3 + age + female + educ + hispanic + black, data = mdf)

m2payment_large <- list(m2q, m2r, m2s, m2t, m2u, m2v, m2w, m2x)

#----------------------------------#
# 7) Robustness: sci + exposure ----
#----------------------------------#
# interaction between sci comm experiments and self reported winter storm exposure

# Belief in Anthropogenic Climate Change
m4i <- lm(clim_belief ~ condition * dem * snow_degree + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Disaster relief spending
m4j <- lm(spend_relief ~ condition * dem * snow_degree + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Climate change mitigation spending
m4k <- lm(spend_mitigate ~ condition * dem * snow_degree + ideo3 + age + female + educ + hispanic + black, data = mdf)

# Federal carbon tax
m4l <- lm(fed_tax ~ condition * dem * snow_degree + ideo3 + age + female + educ + hispanic + black, data = mdf)

# power grid
m4m <- lm(infra_policy_support ~ condition * dem * snow_degree + ideo3 + age + female + educ + hispanic + black, data = mdf)

m4sciexpose <- list(m4i, m4j, m4k, m4l, m4m)

#---------------------------#
# 8) Writing out results ----
#---------------------------#
save(m1, m2, m3, m4, m3subset, m2payment_small, m2payment_large, m4sciexpose, mdf_did, mdf, w1df, file = 'outputs/all_models.RData')
rm(list=ls())
