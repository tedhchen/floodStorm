#------------#
# 0) Prep ----
#------------#
# loading packages
setwd('~/git/floodStorm/')
library(scales)              # v1.3.0
library(fixest)              # v0.11.1
library(marginaleffects)     # v0.9.0
library(lmtest)              # v0.9-40
library(bcp)                 # v4.0.3
# library(gtrendsR)          # v1.5.1 # This was used to collect data but we now use the archived data

# editing legend plotting function
body(legend)[[60]][[3]][[7]][[3]]$'lend' <- 1

# sessionInfo()
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
#
# Matrix products: default
#
#
# locale:
#   [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
#   [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8
#
# time zone: America/New_York
# tzcode source: internal
#
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] bcp_4.0.3             lmtest_0.9-40         zoo_1.8-11            marginaleffects_0.9.0 fixest_0.11.1         scales_1.3.0
#
# loaded via a namespace (and not attached):
#   [1] Formula_1.2-5       R6_2.5.1            numDeriv_2016.8-1.1 lattice_0.22-6      glue_1.7.0          generics_0.1.3
#   [7] lifecycle_1.0.4     cli_3.6.3           dreamerr_1.2.3      sandwich_3.0-2      data.table_1.14.8   compiler_4.4.1
#  [13] rstudioapi_0.14     tools_4.4.1         nlme_3.1-164        munsell_0.5.1       Rcpp_1.0.13         colorspace_2.1-1
#  [19] rlang_1.1.4

#------------------#
# 1) Clean data ----
#------------------#
source('scripts/immedate_data_prep.R')

#------------------------#
# 2) Do main analysis ----
#------------------------#
source('scripts/main_analysis.R')

#---------------------------#
# 3) Additional analysis ----
#---------------------------#
# 3.1) checking attention to treatment stimuli ----
source('scripts/treatment_checks.R')

# 3.2) power outage threshold ----
source('scripts/outage_robustness_check.R')

# 3.3) wave 2 attrition ----
source('scripts/check_attrition.R')

#--------------------#
# 4) Plot figures ----
#--------------------#
source('scripts/main_plotting.R')
source('scripts/google_trends_Texas.R')
source('scripts/plot_reported_exposure.R')

# 4.1) Notes on figure locations ----
# Fig 1: `main_plotting.R`
# Fig 2: `immediate_data_prep.R`
# Fig 3: `main_plotting.R`
# Fig 5: `main_plotting.R`
# Fig 6: `google_trends_Texas.R`
# Fig 7: `check_attrition.R`
# Fig 8: `plot_reported_exposure.R`
# Fig 9: `plot_reported_exposure.R`
# Fig S1.1: `main_plotting.R`
# Fig S2.1: `main_plotting.R`
# Fig S2.2: `main_plotting.R`
# Fig S2.3: `outage_robustness.R`
# Fig S3.1: `outage_robustness.R`
# Fig S3.2: `outage_robustness.R`
