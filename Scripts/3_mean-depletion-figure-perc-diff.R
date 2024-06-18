# Plot of mean depletion trends 
#CJ Brown 2023-02-13
#
# Mean depletion figure as percentage difference from MRA

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

# load("Outputs/2022-02-11_processesed-assessment-data.rda")
load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

number_of_years_prior_MRA <- 5

dat_LRR2 <- dat_LRR %>%
  #Select just those stocks with full time-series from 1980 to 2010 or after
  #currently this option is off
  # filter((maxyearMRA > 2009) & (minyear < 1981)) %>%
  filter(tsyear > 1959 & !is.na(finish2.y)) %>%
  #Get rid of years of data missing an MRA 
  mutate(years_to_MRA = finish2.y - finish2.x,
         # finish2.y = MRY of MRA
         # finish2.x = MRY of this assess
         assess_age = case_when(
           years_to_MRA == 0 ~ "MRA",
           years_to_MRA >0 & years_to_MRA <= 3 ~ "1-3 yr old",
           years_to_MRA >3 & years_to_MRA <= 8 ~ "4-8 yr old",
           years_to_MRA >8 ~ ">8 yr old"
         )) 

#axes limits 
xmin <- 1980 #1980
xmax <- 2020
ymin <- 0.2
ymax <- 1

yaxis <- scale_y_continuous(breaks = seq(0.2, 1, by = 0.2),
                            limits = c(ymin, ymax),
                            labels = seq(0.2, 1, by = 0.2))

#
# ALL STOCKS 
#


dat_assess_mean <- dat_LRR2 %>% 
  #LRR diff
  mutate(Brel_diff = (log(Brel) - log(Brel_MRA))) %>%
  group_by(tsyear, assess_age, stocklong) %>%
  summarize(Brel_diff = (mean(Brel_diff))) %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = (mean(Brel_diff)),
            n = n(),
            Dep_SE = (sd(Brel_diff)/sqrt(n))) %>%
  ungroup() %>%
  filter(n>9) %>%
  #Remove year/group combos with <4 assessments
  group_by(assess_age) %>%
  mutate(maxyr = max(tsyear),
         notmax = maxyr != tsyear)
#change order
dat_assess_mean$assess_age <- factor(dat_assess_mean$assess_age,
levels = c("MRA",
           "1-3 yr old",
           "4-8 yr old",
           ">8 yr old"))

pal <- c("#E69F00", "#56B4E9", "#009E73")#, "#0072B2")

g1 <- dat_assess_mean %>%
  #remove MRA as its delta == 1
  filter(notmax) %>%
  filter(assess_age != "MRA") %>%
  ggplot() +
  aes(x = tsyear, y = exp(Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "black") +
  geom_line() +
  geom_ribbon(aes(ymin = exp(Depletion - Dep_SE), 
                  ymax = exp(Depletion + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression(Delta*'B/B'[max])) +
  xlab("Year") + 
  xlim(xmin, xmax) +
  # yaxis +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  scale_fill_manual("Assessment age", 
                     values = pal)  
g1
# ggsave("Outputs/mean-depletion-perc-diff.png")

#
# As boxplot - to check distribution 
#

if (FALSE){
dat_LRR2 %>% 
  #percentage diff
  #LRR diff
  filter(assess_age != "MRA") %>%
  mutate(Brel_diff = exp(log(Brel) - log(Brel_MRA))) %>%
  filter(tsyear > 1980) %>%
  ggplot() +
  aes(x = tsyear, y = Brel_diff,
      group = tsyear) +
  geom_boxplot() +
  geom_hline(yintercept = 1)+
  scale_y_log10() +
  facet_wrap(~assess_age, scales = "free") +
  theme_classic()
}
#
# Get stock status and oldest assessment age 
#

stock_assess_morethan_10yr <- dat_LRR2 %>%
  group_by(stocklong) %>%
  summarize(minassess = min(finish2.x),
            MRAMRY = max(finish2.y)) %>%
  mutate(diff = MRAMRY - minassess) %>%
  filter(diff>8)
nrow(stock_assess_morethan_10yr)

stock_status_MRAMRY <- dat_LRR2 %>%
  #just the MRAs
  filter(finish2.y == finish2.x) %>%
  #stock status X years before MRY of the MRA
  mutate(MRAMRY_min5 = finish2.y - number_of_years_prior_MRA)  %>%
  filter(tsyear == MRAMRY_min5) %>% 
  # filter(tsyear == finish2.y) %>% #deactivate if I want
  # to have status in year of ts. finish2.y is final year of MRA
  select(stocklong, Brel_MRA, tsyear) %>%
  #Use Brel for the year of the datapoint, but in the MRA
  mutate(status = ifelse(Brel_MRA>0.4, "Sustainable",
                         "Depleted")) %>%
  select(stocklong, status)
  
#
# Depleted and sustainable stocks
#

dat_assess_mean_status <- dat_LRR2 %>%
  left_join(stock_status_MRAMRY) %>%
  mutate(Brel_diff = (log(Brel) - log(Brel_MRA))) %>%
  group_by(tsyear, assess_age, status, stocklong) %>%
  summarize(Brel_diff = mean(Brel_diff)) %>%
  group_by(tsyear, assess_age, status) %>%
  summarize(Depletion = mean(Brel_diff),
            n = n(),
            Dep_SE = sd(Brel_diff)/sqrt(n)) %>%
  filter(n>9) %>%
  ungroup() %>%
  group_by(assess_age) %>%
  mutate(maxyr = max(tsyear),
         notmax = maxyr != tsyear)

dat_assess_mean_status$assess_age <- 
  factor(dat_assess_mean_status$assess_age,
         levels = c("MRA",
                    "1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old"))


g2 <- 
  dat_assess_mean_status %>%
  filter(assess_age != "MRA") %>%
  filter(notmax & status == "Sustainable") %>% 
  ggplot() +
  aes(x = tsyear, y = exp(Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "black") +
  geom_line() +
  geom_ribbon(aes(ymin = exp(Depletion - Dep_SE), 
                  ymax = exp(Depletion + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression(Delta*'B/B'[max])) +
  xlab("Year") + 
  xlim(xmin, xmax) +
  # ylim(1980, 2020) +
  # yaxis +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  scale_fill_manual("Assessment age", 
                    values = pal) +
  theme(legend.position = "none")


g3 <- 
  dat_assess_mean_status %>%
  filter(assess_age != "MRA") %>%
  filter(notmax & status == "Depleted") %>% 
  ggplot() +
  aes(x = tsyear, y = exp(Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "black") +
  geom_line() +
  geom_ribbon(aes(ymin = exp(Depletion - Dep_SE), 
                  ymax = exp(Depletion + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression(Delta*'B/B'[max])) +
  xlab("Year") + 
  xlim(xmin, xmax) +
  # ylim(1980, 2020) +
   # yaxis +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  scale_fill_manual("Assessment age", 
                    values = pal) +
  theme(legend.position = "none")

g2
g3
#
# Stocks with >10yr old assessments 
#

dat_assess_mean_10yrold <- dat_LRR2 %>%
  mutate(Brel_diff = (log(Brel) - log(Brel_MRA))) %>%
  inner_join(stock_assess_morethan_10yr) %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = mean(Brel_diff),
            n = n(),
            Dep_SE = sd(Brel_diff)/sqrt(n)) %>%
  filter(n>14) %>%
  ungroup() %>%
  group_by(assess_age) %>%
  mutate(maxyr = max(tsyear),
         notmax = maxyr != tsyear)

dat_assess_mean_10yrold$assess_age <- 
  factor(dat_assess_mean_10yrold$assess_age,
         levels = c("MRA",
                    "1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old"))

g4 <- dat_assess_mean_10yrold %>%
  filter(assess_age != "MRA") %>%
  filter(notmax) %>%
  #Filter out final year (due to small sample bias)
  ggplot() +
  aes(x = tsyear, y = exp(Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "black") +
  geom_line() +
  geom_ribbon(aes(ymin = exp(Depletion - Dep_SE), 
                  ymax = exp(Depletion + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression(Delta*'B/B'[1])) +
  xlab("Year") + 
  xlim(xmin, xmax) +
  # ylim(1980, 2020) +
  yaxis +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  scale_fill_manual("Assessment age", 
                    values = pal) +
  theme(legend.position = "none")

dat_status_diff <- dat_assess_mean_status
save(g1, g2, g3, g4, dat_status_diff, 
     file = paste0("Outputs/timeseries-diff-plots-Bmax-",number_of_years_prior_MRA,"year.rda"))

