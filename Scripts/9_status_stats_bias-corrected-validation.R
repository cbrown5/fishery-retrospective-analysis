#
# Predict bias in for MRA
#

# Assumes we are at MRA but using model fit to data from 
# 2nd-MRA to predict forwards to MRA
# Using Brel as at MRA

# CJ Brown 2022-07-26

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)

rm(list = ls())

#
#Observed stats for 2MRA 
# #
# load("Outputs/2022-02-11_processesed-assessment-data.rda")
# dat_MRA_MRY <- filter(dat_MRA, tsyear == finish2) %>%
#   select(stocklong, tsyear, Brel_MRA, SSB_MRA)
#   
# rm(dat_B0, dat_LRR, dat_MRA, dat_MRY, dat_MRY_stock_means)


#
# Filter for bias in 2nd MRA, from the full dataset
#

datstock <- read.csv("Outputs/glm-covariates-merged.csv") %>%
  group_by(stocklong) %>%
  filter(tsyear == max(tsyear)) %>%
  select(stocklong, clupeids, trend.50yr.coef.cap,
         HADISSTmean.5yr, dollar_per_tonne, start.year,
         lnBB0,#presumed status in this year
         lnBrel_MRA, #MRA status for this year
         SSB_MRA, #MRA SSB for this year
         Delta_Brel
  ) %>%
  mutate(stock_value = log(SSB_MRA * dollar_per_tonne/1000000))

#
# Stats for 2nd-MRA
#
 load(file = paste0("Outputs/status-validation/model-fit-validation.rda"))
 m1_validate$formula
dat2val <- read.csv("Outputs/status-validation/glm-covariates-validation.csv") 

datprednew <- dat2val %>%
  group_by(stocklong) %>%
  filter(tsyear == max(tsyear)) %>% 
  select(stocklong, year.diff, start.diff,
         Year_MRA) %>%
  left_join(datstock)

#
# Make predictions of error 
#

mpred <- posterior_epred(m1_validate, datprednew) %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  data.frame() %>%
  cbind(datprednew) 

g1 <- ggplot(mpred) + 
  aes(x = Delta_Brel, y = X50.) +
   geom_point() +
  stat_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1)

g1

with(mpred, cor.test(Delta_Brel, X50.))



  

