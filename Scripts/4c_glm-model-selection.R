# GLM of bias for B/B0 ("Brel")
#
# CJ Brown 2023-05-12
#
#4b: Model excluding some stocks, by request of reviewer
#
# Exclude stocks that are failing to rebuild because
# of climate issues
# Bristol Bay red king crab:Red king crab_Bristol Bay 
# Bering Sea snow crab: Snow crab_Bering Sea
# Gulf of Alaska Pacific cod: Pacific cod_Gulf of Alaska
# two Blue King crab stocks: Blue king crab_Pribilof Islands, 
# Blue king crab_Saint Matthew Island


library(dplyr)
library(ggplot2)
library(brms)
library(forcats)

dat <- read.csv("Outputs/2022-02-11_glm-data.csv")
load("Outputs/2022-02-11_processesed-assessment-data.rda")
datcovar <- read.csv("Data/glm-covariates-Hadley.csv")
stock_groups <- read.csv("Data/stock_groups.csv")
regions <- read.csv("Data/regions.csv")
theme_set(theme_classic())

#stocks to exclude b/c of climate effecting recovery
stocks_exclude <- c("Red king crab_Bristol Bay",
                    "Snow crab_Bering Sea",
                    "Pacific cod_Gulf of Alaska",
                    "Blue king crab_Pribilof Islands",
                    "Blue king crab_Saint Matthew Island")

dat2 <- 
  #Join  covariates
  inner_join(dat, datcovar) %>%
  left_join(stock_groups) %>%
  #Join MRA to get Brel_MRA
  left_join(select(dat_MRA, stocklong, Brel_MRA, SSB_MRA, tsyear,
                   Year_MRA = finish2)) %>%
  mutate(stock_value = log(SSB_MRA * dollar_per_tonne/1000000), 
         lnBrel_MRA = log(Brel_MRA),
         start.year = tsyear - start.diff,
         trend.50yr.coef.cap = ifelse(HADISSTtrend.50yr.coef>0.05, 0.05,
                                      HADISSTtrend.50yr.coef)*100,
         clupeoids = factor(ifelse(Fishery.group == "Herrings, sardines, anchovies",
                           "Clupeid", "Other"))) %>%
  rename(Delta_Brel = d.B.B0,
         Delta_B = d.B..ln.B.B.recent.,
         Delta_B1 = d.B0..ln.B0.B0.recent.,
         fishery_group = Fishery.group) %>%
  filter(!(stocklong %in% stocks_exclude))
nrow(dat2)
dat2$clupeoids <- relevel(dat2$clupeoids, ref = "Other")

#Exclude some stocks


#Write data for supplemental file 
 # write.csv(dat2, "Outputs/glm-covariates-merged.csv",
          # row.names = FALSE)

response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

ivar <- response_vars[1]

#
# GLMM Delta_Brel
#

m1 <- brm(Delta_Brel ~ (year.diff + 
                          stock_value +
                          year.diff)*lnBrel_MRA +
            start.diff +
            HADISSTmean.5yr + 
            trend.50yr.coef.cap +
            clupeoids + 
            (1|stocklong),
          data = dat2,
          chains = 4,
          cores = 4,
          iter = 4000)

summary(m1)

#Review response: there is no leverage for bayesian
# models, so what we did was.... 

l1 <- loo(m1)
l1$pointwise[,"influence_pareto_k"]
#rerun model excluding obs one by one 
# where pareto_k > 0.7
#UP to here: read help file then continue

#
# B model 
#


m2 <- brm(Delta_B ~ (year.diff + 
                       stock_value +
                       year.diff)*lnBrel_MRA +
            start.diff +
            HADISSTmean.5yr + 
            trend.50yr.coef.cap +
            clupeoids + 
            (1|stocklong),
          data = dat2,
          chains = 4,
          cores = 4,
          iter = 4000)

summary(m2)

#
# B1 model 
#


m3 <- brm(Delta_B1 ~ (year.diff + 
                        stock_value +
                        year.diff)*lnBrel_MRA +
            start.diff +
            HADISSTmean.5yr + 
            trend.50yr.coef.cap +
            clupeoids + 
            (1|stocklong),
          data = dat2,
          chains = 4,
          cores = 4,
          iter = 4000)

summary(m3)
#
# Response conditional on B/B1 and Years to MRA
#

newdata <- with(dat2, expand.grid(
  #need to put back onto scale of model fitting (ie divide by 1 SD)
  lnBrel_MRA = c(log(0.1), 
                 log(0.4), 
                 log(1)),
  year.diff = seq(min(year.diff), max(year.diff), length.out = 50),
  start.diff = mean(start.diff),
  HADISSTmean.5yr = mean(HADISSTmean.5yr),
  trend.50yr.coef.cap = mean(trend.50yr.coef.cap),
  stock_value = mean(stock_value),
  clupeoids = "Other",
  stocklong = NA,
  Group = NA
))

pdat1 <- posterior_epred(m1, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat2 <- posterior_epred(m2, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat3 <- posterior_epred(m3, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)
#put status back onto real scale
pdat1$status <- exp(pdat1$lnBrel_MRA)
pdat2$status <- exp(pdat2$lnBrel_MRA)
pdat3$status <- exp(pdat3$lnBrel_MRA)

#
#Figure of predicted year MRA effect
#

g1 <- ggplot(pdat1) + 
  aes(x = year.diff, y = exp(`50%`), fill = factor(status),
      group = status) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7) +
  ylab(expression(Delta*'B/B'[1])) +
  xlab("Survey age (years") +
  # xlim(0, 15) + 
  # scale_y_continuous(breaks = seq(0.5, 3, by = 0.5),
                     # labels = seq(0.5, 3, by = 0.5),
                     # limits = c(0.5, 3.2)) +
  scale_fill_manual(expression('B/B'[1]), values = c("#d41515", "black", "#0537ab"))


g2 <- ggplot(pdat2) + 
  aes(x = year.diff, y = exp(`50%`), fill = factor(status),
      group = status) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7) +
  ylab(expression(Delta*'B')) +
  xlab("Survey age (years") +
  # xlim(0, 15) + 
  # scale_y_continuous(breaks = seq(0.5, 3, by = 0.5),
  # labels = seq(0.5, 3, by = 0.5),
  # limits = c(0.5, 3.2)) +
  scale_fill_manual(expression('B/B'[1]), values = c("#d41515", "black", "#0537ab"))

g3 <- ggplot(pdat3) + 
  aes(x = year.diff, y = exp(`50%`), fill = factor(status),
      group = status) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7) +
  ylab(expression(Delta*'B'[1])) +
  xlab("Survey age (years") +
  # xlim(0, 15) + 
  # scale_y_continuous(breaks = seq(0.5, 3, by = 0.5),
  # labels = seq(0.5, 3, by = 0.5),
  # limits = c(0.5, 3.2)) +
  scale_fill_manual(expression('B/B'[1]), values = c("#d41515", "black", "#0537ab"))

library(patchwork)
gall <- (g1 + g2 + g3)+
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") + 
  plot_layout(guides='collect') 
gall

ggsave(gall, file =paste0("Outputs/obsolesence-effects-excluding-5-stocks.png"),
       width = 8, height =3)

save(pdat1, pdat2, pdat3, file = "Outputs/2023-05-12_predictions-excluding-5-stocks.rda")
