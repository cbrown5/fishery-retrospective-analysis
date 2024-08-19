#
# Predict bias in 2030
#

# Assumes we are in 2030 looking back to MRA, what 
# bias is inferred in the MRA? 

# CJ Brown 2024-01-31

rm(list = ls())

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)
load(file = paste0("Outputs/Delta_Brel/best-model-fit.rda"))
load("Outputs/2024-08-02_processesed-assessment-data-Bmax.rda")
dat2 <- read.csv("Outputs/glm-covariates-merged-Bmax.csv")
ivar <- "Delta_Brel"
m1$formula

dat_MRA_MRY <- filter(dat_MRA, tsyear == finish2) 
datprednew <- dat_MRA_MRY

sum(datprednew$Brel_MRA<0.1)
sum(datprednew$Brel_MRA<0.4)
sum(datprednew$Brel_MRA>0.4)

#Make covariates for MRA
#select covariates
MRA_covars <- dat2 %>%
  select(stocklong, start.diff.MRA, stock_value, HADISSTmean.5yr,
         HADISSTtrend.50yr.coef, clupeoid, tsyear) %>%
  group_by(stocklong) %>%
  filter(tsyear == max(tsyear)) %>%
  ungroup() %>%
  #TS length for the assessment is TS length at the MRA
  mutate(start.diff = start.diff.MRA,
         trend.50yr.coef.cap = ifelse(HADISSTtrend.50yr.coef>0.05, 0.05,
                                      HADISSTtrend.50yr.coef)*100)
nrow(MRA_covars)
length(unique(MRA_covars$stocklong))

#
# Raw bias stats
#

years_in_future <- 10
#Project year.diff for MRA to 2030
datprednew$year.diff <- years_in_future

#add covariates
datprednew <- datprednew %>%
  left_join(MRA_covars, by = "stocklong") 

#filter for just one assessment per stock
# use values of covaraites for each stock
datprednew <- datprednew %>%
  mutate(lnBrel_MRA = log(Brel_MRA)) %>%
  select(year.diff, start.diff, 
         lnBrel_MRA,
         Brel_MRA,
         trend.50yr.coef.cap, HADISSTmean.5yr,
         stock_value, 
         stocklong,
         clupeoid) 

if(FALSE){
  #as a comparison, use the means of assessments like in figure 4
  # and condition on the average stock
  
datprednew <- dat_MRA_MRY %>% mutate(
  year.diff = years_in_future,
  start.diff = mean(dat2$start.diff),
  HADISSTmean.5yr = mean(dat2$HADISSTmean.5yr),
  trend.50yr.coef.cap = mean(dat2$trend.50yr.coef.cap),
  stock_value = mean(dat2$stock_value),
  clupeoid = "Other",
  lnBrel_MRA = log(Brel_MRA),
  stocklong = NA
)

}

nrow(datprednew)

mpred <- posterior_epred(m1, datprednew) %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  data.frame() %>%
  cbind(datprednew) %>%
  #calculate status estimate
  mutate(status_estimate = exp(lnBrel_MRA - X50.),
         status = exp(lnBrel_MRA))

g1 <- ggplot(mpred) + 
  geom_density(aes(x = status_estimate), color = "red") + 
  geom_density(aes(x = status)) +
  xlim(0, 3) + 
  xlab("B/B1") + 
  ylab("Density")
g1
# ggsave(g1, file = paste0("Outputs/",ivar,"/status-adjusted.png"))

x1 <- quantile(mpred$status_estimate, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
x2 <- quantile(mpred$status, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

status_q <- rbind(x1, x2) %>% data.frame() %>% round(2)
status_q$val <- c("Estimated unbiased", "Current")  

# write.csv(status_q, paste0("Outputs/",ivar,"/status_unbiased.csv"))

status_prop <- mpred %>%
  summarize(sum(status_estimate<0.1)/n(),
            sum(status_estimate<0.2)/n(),
            sum(status_estimate<0.3)/n(),
            sum(status_estimate<0.4)/n(),
            sum(status<0.1)/n(),
            sum(status<0.2)/n(),
            sum(status<0.3)/n(),
            sum(status<0.4)/n(),
            sum(status_estimate<0.1),
            sum(status_estimate<0.2),
            sum(status_estimate<0.3),
            sum(status_estimate<0.4),
            sum(status<0.1),
            sum(status<0.2),
            sum(status<0.3),
            sum(status<0.4)) %>%
  signif(2)
status_prop

# write.csv(status_prop, paste0("Outputs/",ivar,"/proportion-stock-status.csv"))

#
# Stats for paper 
#
datprednew2 <- 
  expand.grid(Brel = c(0.05,0.1, 1),
              stocklong = NA,
              Group = NA, 
              tsyear = 2030,
              year.diff = 10,
              start.diff = mean(dat2$start.diff),
              stock_value = mean(dat2$stock_value),
              trend.50yr.coef.cap =mean(dat2$trend.50yr.coef.cap),
              HADISSTmean.5yr = mean(dat2$HADISSTmean.5yr),
              clupeoid = "Other") %>%
  mutate(lnBrel_MRA = log(Brel))

mpred2 <- posterior_epred(m1, datprednew2,
                          re_formula = NA) %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  data.frame() %>%
  cbind(datprednew2) %>%
  #calculate status estimate
  mutate(status_estimate = exp(lnBrel_MRA - X50.),
         status = exp(lnBrel_MRA))


mpred2$X50.
exp(mpred2$X50.)
exp(mpred2$X2.5.)
exp(mpred2$X97.5.)
mpred2$status_estimate

