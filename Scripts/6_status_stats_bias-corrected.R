#
# Predict bias in 2030
#

# Assumes we are in 2030 looking back to MRA, what 
# bias is inferred in the MRA? 

# CJ Brown 2022-07-26

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)
load(file = paste0("Outputs/Delta_Brel/best-model-fit.rda"))
dat2 <- read.csv("Outputs/glm-covariates-merged.csv")
ivar <- "Delta_Brel"
m1$formula
datprednew <- dat2

#
# Raw bias stats
#

dat2 %>% group_by(stocklong) %>%
  summarize(x = mean(Delta_Brel)) %>%
  summarize(sum(x>0),
            sum(x<0),
            sum(x>0)/230,
            sum(x<0)/230,
            sum(x>0.5),
            sum(x>1)) %>%
  data.frame()


#

#Project year.diff for MRA to 2030
datprednew$year.diff <- 2030 - dat2$Year_MRA
#add years to 2030 to start.diff
datprednew$start.diff <- dat2$start.diff.MRA + dat2$year.diff

#filter for just one assessment per stock
datprednew <- datprednew %>%
  group_by(stocklong) %>%
  mutate(final_assessment = ifelse(tsyear == max(tsyear), 1, 0)) %>%
  filter(final_assessment == 1) %>% 
  select(year.diff, start.diff, 
         lnBrel_MRA, 
         trend.50yr.coef.cap, HADISSTmean.5yr,
         stock_value, 
         stocklong, tsyear,
         clupeids) %>%
  distinct()
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

ggsave(g1, file = paste0("Outputs/",ivar,"/status-adjusted.png"))

x1 <- quantile(mpred$status_estimate, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
x2 <- quantile(mpred$status, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

status_q <- rbind(x1, x2) %>% data.frame() %>% round(2)
status_q$val <- c("Estimated unbiased", "Current")  

write.csv(status_q, paste0("Outputs/",ivar,"/status_unbiased.csv"))

status_prop <- mpred %>%
  summarize(sum(status_estimate<0.1)/n(),
            sum(status_estimate<0.2)/n(),
            sum(status_estimate<0.4)/n(),
            sum(status<0.1)/n(),
            sum(status<0.2)/n(),
            sum(status<0.4)/n(),
            sum(status_estimate<0.1),
            sum(status_estimate<0.2),
            sum(status_estimate<0.4),
            sum(status<0.1),
            sum(status<0.2),
            sum(status<0.4)) %>%
  signif(2)

write.csv(status_prop, paste0("Outputs/",ivar,"/proportion-stock-status.csv"))

#
# Stats for paper 
#
datprednew2 <- 
  expand.grid(Brel = c(0.05,0.1, 0.9),
              stocklong = NA,
              Group = NA, 
              tsyear = 2030,
              year.diff = 10,
              start.diff = mean(dat2$start.diff),
              stock_value = mean(dat2$stock_value),
              trend.50yr.coef.cap =mean(dat2$trend.50yr.coef.cap),
              HADISSTmean.5yr = mean(dat2$HADISSTmean.5yr),
              clupeids = "Other") %>%
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

