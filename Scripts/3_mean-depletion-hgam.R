# Plot of mean depletion trends 
#CJ Brown 2022-06-12

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(mgcv)

load("Outputs/2022-02-11_processesed-assessment-data.rda")

dat_LRR2 <- dat_LRR %>%
  # filter(stocklong == "Pacific cod_Gulf of Alaska") %>%
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
ymin <- 0.2
ymax <- 1.5



#
# HGAM
#
dat_LRR2$lnBrel <- log(dat_LRR2$Brel)
dat_LRR2$assessid <- factor(dat_LRR2$assessid)
dat_LRR2$assess_age <- factor(dat_LRR2$assess_age) 

system.time(
  m1 <- bam(lnBrel ~ s(tsyear, by = assessid, m = 1) + 
              s(tsyear, by = assess_age, m = 2),
            data = filter(dat_LRR2, tsyear>1979))
)

#
# ALL STOCKS 
#

dat_assess_mean <- dat_LRR2 %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            n = n()) %>%
  ungroup() %>%
  filter(n>14) %>%
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

pal <- c("black", "#E69F00", "#56B4E9", "#009E73")#, "#0072B2")

g1 <- ggplot(dat_assess_mean) +
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_line() +
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(1960, 2020) + 
  ylim(ymin, ymax) +
  scale_color_manual("Assessment age", 
                     values = pal)

ggsave("Outputs/mean-depletion.png")

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
  filter(tsyear == finish2.y) %>% #deactivate if I want
  # to have status in year of ts
  select(stocklong, Brel_MRA, tsyear) %>%
  #Use Brel for the year of the datapoint, but in the MRA
  mutate(status = ifelse(Brel_MRA>0.5, "Sustainable",
                         "Depleted")) %>%
  select(stocklong, status)
  
#active if I want to have status in year of TS


#
# Depleted and sustainable stocks
#

dat_assess_mean_status <- dat_LRR2 %>%
  left_join(stock_status_MRAMRY) %>%
  group_by(tsyear, assess_age, status) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            n = n()) %>%
  filter(n>14) %>%
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
  filter(notmax & status == "Sustainable") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_line() +
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(1960, 2020) + 
  ylim(ymin, ymax) +
  scale_color_manual("Assessment age", 
                     values = 
                       pal)#+ 
  # theme(legend.position = "none")

g3 <- 
  dat_assess_mean_status %>%
  filter(notmax & status == "Depleted") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_line() +
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(1960, 2020) + 
  ylim(ymin, ymax) +
  scale_color_manual("Assessment age", 
                     values =pal) + 
   theme(legend.position = "none")

#   scale_color_manual("Assessment age", 
#                      values = 
#                        RColorBrewer::brewer.pal(9, "Purples")[5:9])

 ggsave("Outputs/mean-depletion-sustainable.png", g2)
 ggsave("Outputs/mean-depletion-depeleted.png", g3)

#
# Stocks with >10yr old assessments 
#

dat_assess_mean_10yrold <- dat_LRR2 %>%
  inner_join(stock_assess_morethan_10yr) %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = exp(mean(log(Brel)))) %>%
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
  geom_line() +
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(1960, 2020) + 
  ylim(ymin, ymax) +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")
#   scale_color_manual("Assessment age", 
#                      values = 
#                        RColorBrewer::brewer.pal(9, "Purples")[5:9])

ggsave("Outputs/mean-depletion-10yrold-assessments.png", g4,
       width = 5, height = 3)


gall <- (g1 + g4) / (g2 + g3) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides='collect') 

ggsave("Outputs/depletion_timeseries-figures-all-scales-same.png", gall,
       width = 8, height = 4)

