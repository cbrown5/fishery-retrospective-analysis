# Plot of mean depletion trends 
#CJ Brown 2022-11-08
#
# Figure 2 sensitivity test - Figure ED2
# this version only includes stocks that had data from 1980 to 2010

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

# load("Outputs/2022-02-11_processesed-assessment-data.rda")
load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

sefun <- function(x){
  n <- sum(!is.na(x))
  sd(x)/sqrt(n)
}

dat_LRR2 <- dat_LRR %>%
  #Select just those stocks with full time-series from 1980 to 2010 or after
  filter((maxyearMRA > 2015) & (minyear < 1981)) %>%
  filter(tsyear > 1959 & !is.na(finish2.y)) %>%
  #Get rid of years of data missing an MRA 
  #Only keep MRY of each assessment
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
ymin <- 0
ymax <- 2

yaxis <- scale_y_continuous(breaks = seq(0, ymax, by = 0.25),
                            limits = c(ymin, ymax),
                            labels = seq(0, ymax, by = 0.25))

#
# ALL STOCKS 
#

dat_assess_mean <- dat_LRR2 %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            Dep_SE = sefun(log(Brel)),
            n = n()) %>%
  ungroup() %>%
  # filter(n>14) %>%
  #Remove year/group combos with <4 assessments
  group_by(assess_age) %>%
  mutate(maxyr = max(tsyear),
         notmax = maxyr != tsyear)
length(unique(dat_LRR2$stocklong))

#change order
dat_assess_mean$assess_age <- factor(dat_assess_mean$assess_age,
levels = c("MRA",
           "1-3 yr old",
           "4-8 yr old",
           ">8 yr old"))

pal <- c("black", "#E69F00", "#56B4E9", "#009E73")#, "#0072B2")

g1 <- ggplot(dat_assess_mean) +
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_hline(yintercept = 0.4, color = "grey60", 
             linetype = 2) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(log(Depletion) - Dep_SE), 
                  ymax = exp(log(Depletion) + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values = pal)+ 
  scale_fill_manual("Assessment age", 
                    values = 
                      pal) 

#
# Stocks that rise in last 5 years of MRA
#

getslope <- function(x,y){
  m <- lm(log(y) ~ x)
  coef(m)[2]
}

xdat <- filter(dat_LRR2, assess_age == "MRA") %>%
  mutate(year_diff = maxyear - tsyear) %>% 
  filter(year_diff < 6) %>%
  group_by(stocklong) %>%
  summarize(b = getslope(tsyear, Brel))

sum(xdat$b>0)
sum(xdat$b<0)
mean(exp(xdat$b)-1)

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
  mutate(MRAMRY_min5 = finish2.y - 0)  %>%
  filter(tsyear == MRAMRY_min5) %>% 
  # filter(tsyear == finish2.y) %>% #deactivate if I want
  # to have status in year of ts. finish2.y is final year of MRA
  #active if I want to have status in year of TS
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
  group_by(tsyear, assess_age, status) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            Dep_SE = sefun(log(Brel)),
            n = n()) %>%
  filter(n>14) %>%
  ungroup() %>%
  group_by(assess_age) %>%
  mutate(maxyr = max(tsyear),
         notmax = maxyr != tsyear)

dat_LRR2 %>%
  left_join(stock_status_MRAMRY) %>%
  select(status, stocklong) %>%
  distinct() %>%
  group_by(status) %>%
  summarize(n())

dat_assess_mean_status$assess_age <- 
  factor(dat_assess_mean_status$assess_age,
         levels = c("MRA",
                    "1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old"))

g2 <- 
  dat_assess_mean_status %>%
  filter(notmax & status == "Sustainable") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_hline(yintercept = 0.4, color = "grey60", 
             linetype = 2) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(log(Depletion) - Dep_SE), 
                  ymax = exp(log(Depletion) + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values = 
                       pal) + 
  scale_fill_manual("Assessment age", 
                     values = 
                       pal) 
  # theme(legend.position = "none")

g3 <- 
  dat_assess_mean_status %>%
  filter(notmax & status == "Depleted") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_hline(yintercept = 0.4, color = "grey60", 
             linetype = 2) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(log(Depletion) - Dep_SE), 
                  ymax = exp(log(Depletion) + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values =pal) + 
   theme(legend.position = "none")+ 
  scale_fill_manual("Assessment age", 
                    values = 
                      pal) 


#
# Stocks with >10yr old assessments 
#

dat_assess_mean_10yrold <- dat_LRR2 %>%
  inner_join(stock_assess_morethan_10yr) %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            Dep_SE = sefun(log(Brel))) %>%
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
  filter(notmax) %>%
  #Filter out dip in final year (due to small sample bias)
  ggplot() + 
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_hline(yintercept = 0.4, color = "grey60", 
             linetype = 2) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(log(Depletion) - Dep_SE), 
                  ymax = exp(log(Depletion) + Dep_SE),
                  fill = assess_age),
              color = NA,
              alpha = 0.5)+
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")+ 
  scale_fill_manual("Assessment age", 
                    values = 
                      pal) 

gall <- (g1 + g4) / (g2 + g3) + 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) +
  plot_layout(guides='collect') 

# ggsave("Outputs/depletion_timeseries-figures-all-scales-same-complete-1980_2010_withSEs-R4.png", gall,
       # width = 8, height = 4)

# save(g1, g2, g3, g4, file = "Outputs/timeseries-plots-complete-1980_2016_withSEs.rda")
save(g1, g2, g3, g4, file = "Outputs/timeseries-plots-complete-1980_2010_withSEs.rda")