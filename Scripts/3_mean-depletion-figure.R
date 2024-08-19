# Plot of mean depletion trends 
#CJ Brown 2024-02-02

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

 # load("Outputs/2022-02-11_processesed-assessment-data.rda")
load("Outputs/2024-08-02_processesed-assessment-data-Bmax.rda")
# load("Outputs/2024-01-10_processesed-assessment-data-B1.rda")
dat_LRR2 <- dat_LRR %>%
  # filter(stocklong == "Pacific cod_Gulf of Alaska") %>%
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

filter(dat_LRR2, tsyear>2015) %>%
  pull(stocklong) %>%
  unique() %>%
  length()

#axes limits 
xmin <- 1980 #1980
xmax <- 2018
ymin <- 0.18
ymax <- 0.8

yaxis <- scale_y_continuous(breaks = seq(0.2, ymax, by = 0.2),
                            limits = c(ymin, ymax),
                            labels = seq(0.2, ymax, by = 0.2))

#
# ALL STOCKS 
#

#how many stocks after a certain year? 
# filter(dat_LRR2, tsyear > 2015) %>% 
#   # filter(assess_age == "1-3 yr old") %>%
#   pull(stocklong) %>%
#   unique() %>%
#   length()

dat_assess_mean <- dat_LRR2 %>%
  group_by(tsyear, assess_age, stocklong) %>%
  summarize(Brel = exp(mean(log(Brel))),
            n2 = n()) %>%
  group_by(tsyear, assess_age) %>%
  summarize(lnDepletion = mean(log(Brel)),
            n = n(),
            n2 = sum(n2),
            sdlnBrel = sd(log(Brel))) %>%
  ungroup() %>%
  mutate(Dep_CI = 1.96 * sdlnBrel/sqrt(n), 
         Depletion = exp(lnDepletion),
         lwr = exp(lnDepletion - Dep_CI),
         upr = exp(lnDepletion + Dep_CI)) %>%
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

pal <- c("black", "#E69F00", "#56B4E9", "#009E73")#, "#0072B2")

g1 <- ggplot(dat_assess_mean) +
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  # geom_hline(yintercept = 1, color = "grey60") +
  geom_hline(yintercept = 0.4, color = "grey60", 
             linetype = 2) +
  geom_line()+
  theme_classic() + 
  ylab(expression('Depletion (B/B'[max]*')')) +
  xlab("") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = c(0.8, 0.9),
          legend.key.size = unit(0.3, "cm"),
          axis.title=element_text(size=9),
          axis.text = element_text(color = "black"),
         legend.text = element_text(size=8),
         legend.title = element_text(size=9))

g1
# ggsave("Outputs/mean-depletion.png")

#Check status in particular years
(filter(dat_assess_mean, tsyear == 1980)) %>% data.frame()
(filter(dat_assess_mean, tsyear == 2007)) %>% data.frame()
(filter(dat_assess_mean, tsyear == 2008))
(filter(dat_assess_mean, tsyear == 2017))%>% data.frame()

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

#Nb settled on more than 8 years, rather than more than 10 years
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
  select(stocklong, Brel_MRA, tsyear) %>%
  #Use Brel for the year of the datapoint, but in the MRA
  mutate(status = ifelse(Brel_MRA>0.4, "Sustainable",
                         "Depleted")) %>%
  select(stocklong, status)

#MRY MRA Status

dat_LRR2 %>%
  #just the MRAs
  filter(finish2.y == finish2.x) %>%
  #stock status X years before MRY of the MRA
  mutate(MRAMRY_min5 = finish2.y - 0)  %>%
  filter(tsyear == MRAMRY_min5) %>% 
  # filter(tsyear == finish2.y) %>% #deactivate if I want
  # to have status in year of ts. finish2.y is final year of MRA
  select(stocklong, Brel_MRA, tsyear) %>% 
  mutate(status = case_when(Brel_MRA>0.4 ~ "sus",
                            Brel_MRA<=0.4 & Brel_MRA>0.1 ~ "dep",
                            Brel_MRA<=0.1 ~ "coll")) %>%
  group_by(status) %>%
  summarize(n())

stock_status_MRAMRY %>%
  select(status, stocklong) %>%
  distinct() %>%
  group_by(status) %>%
  summarize(n())
93/230
137/230

#
# Depleted and sustainable stocks
#

dat_assess_mean_status <- dat_LRR2 %>%
  left_join(stock_status_MRAMRY) %>%
  group_by(tsyear, assess_age, stocklong, status) %>%
  summarize(Brel = exp(mean(log(Brel))),
            n = n()) %>%
  group_by(tsyear, assess_age, status) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            n = n()) %>%
  filter(n>9) %>%
  ungroup() %>%
  group_by(assess_age) %>%
  mutate(maxyr = max(tsyear),
         notmax = maxyr != tsyear)

dat_assess_mean_status$assess_age <- 
  factor(dat_assess_mean_status$assess_age,
         levels = c("MRA", "1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old"),
         labels = c("1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old",
                    "Most recent"))


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
  theme_classic() + 
  ylab(expression('Depletion (B/B'[max]*')')) +
  xlab("") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values = 
                       pal) +
  theme( axis.title=element_text(size=9),
         axis.text = element_text(color = "black"),
         legend.position = "none")

g2
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
  theme_classic() + 
  ylab(expression('Depletion (B/B'[max]*')')) +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values =pal) + 
   theme( axis.title=element_text(size=9),
          axis.text = element_text(color = "black"),
          legend.position = "none")

 # ggsave("Outputs/mean-depletion-sustainable.png", g2)
 # ggsave("Outputs/mean-depletion-depeleted.png", g3)

#
# Stocks with >10yr old assessments 
#

dat_assess_mean_10yrold <- dat_LRR2 %>%
  inner_join(stock_assess_morethan_10yr) %>%
  group_by(tsyear, assess_age, stocklong) %>%
  summarize(Brel = exp(mean(log(Brel))),
            n = n()) %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            n = n()) %>%
  filter(n>9) %>%
  ungroup() %>%
  group_by(assess_age) %>%
  mutate(maxyr = max(tsyear),
         notmax = maxyr != tsyear)

dat_assess_mean_10yrold$assess_age <- 
  factor(dat_assess_mean_10yrold$assess_age,
         levels = c("MRA", "1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old"),
         labels = c("1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old",
                    "Most recent"))

g4 <- dat_assess_mean_10yrold %>%
  filter(notmax) %>%
  #Filter out final year (due to small sample bias)
  ggplot() + 
  aes(x = tsyear, y = (Depletion),
      color = assess_age, group = assess_age) + 
  geom_hline(yintercept = 1, color = "grey60") +
  geom_hline(yintercept = 0.4, color = "grey60", 
             linetype = 2) +
  geom_line() +
  theme_classic() + 
  ylab(expression('Depletion (B/B'[max]*')')) +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme( axis.title=element_text(size=9),
         axis.text = element_text(color = "black"),
         legend.position = "none")

# ggsave("Outputs/mean-depletion-10yrold-assessments.png", g4,
       # width = 5, height = 3)

# 
# gall <- (g1 + g4) / (g2 + g3) + 
#   plot_annotation(tag_levels = "A") + 
#   plot_layout(guides='collect') 

gall <- g1/g2/g3 + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(heights = c(0.1,0.1,0.1)) & 
  theme(plot.margin = margin(0, 5, 0.1, 5, "pt"))&
  theme(plot.tag = element_text(face = 'bold'))

gall

ggsave("Outputs/depletion_timeseries-figures-all-scales-same-bmax.pdf", gall,
       width = 12, height = 12*6/4, units = "cm")

ggsave("Outputs/depletion_timeseries-figures-all-scales-same-bmax.png", gall,
       width = 4, height = 6)

save(g1, g2, g3, g4, 
     dat_assess_mean,
     dat_assess_mean_10yrold,
     dat_assess_mean_status,
     file = "Outputs/timeseries-plots-1980_2010.rda")


#
# Sample size plots 
#
xmin <- 1980
xmax <- 2020
ymin <- 0
ymax <- 250
yaxis <- scale_y_continuous(breaks = seq(0, ymax, by = 50),
                            limits = c(ymin, ymax),
                            labels = seq(0, ymax, by = 50))

pal <- c("black", "#E69F00", "#56B4E9", "#009E73")#, "#0072B2")

gA <- dat_assess_mean %>%
  filter(n>9) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of stocks") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gA

x <- filter(dat_assess_mean_status, status == "Sustainable") %>%
  filter(n>9) %>%
  filter(assess_age == "MRA") 


gB <- dat_assess_mean_status %>%
  filter(status == "Sustainable") %>%
  filter(n>9) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of stocks") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gB

gC <- dat_assess_mean_status %>%
  filter(status == "Depleted") %>%
  filter(n>9) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of stocks") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gC


gall <- (gA / gB / gC)  + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides='collect') 
gall

ggsave("Outputs/depletion_timeseries-figures-sample-size-all-scales-same.png", gall,
       width = 6, height = 6)



#
# Extended data figure 4
#

# log difference of B/B1 (depletion) for the MRY of each assessment 
# vs the MRA-MRY. x axis is years difference between MRY and MRA-MRY.
# Values averaged by stocks 

#First get stock status in MRY of MRA
stock_status_MRAMRY <- dat_LRR2 %>%
  #just the MRAs
  filter(finish2.y == finish2.x) %>%
  #stock status X years before MRY of the MRA
  mutate(MRAMRY_min5 = finish2.y - 0)  %>%
  filter(tsyear == MRAMRY_min5) %>% 
  select(stocklong, Brel_MRA, tsyear) %>%
  #Use Brel for the year of the datapoint, but in the MRA
  mutate(Status = ifelse(Brel_MRA>0.4, "Sustainable",
                         "Overfished"),
         #make new variable for status in MRY
         Brel_MRAMRY = Brel_MRA) %>%
  select(stocklong, Status,Brel_MRAMRY)

depletion_diff <- dat_LRR2 %>%
  #select MRY of each assessment
  # finish2.y = MRY of MRA
  # finish2.x = MRY of this assess
  filter(tsyear == finish2.x) %>%
  #remove MRA
  filter(!(finish2.x == finish2.y)) %>%
  left_join(stock_status_MRAMRY) %>% 
  mutate(delta_Brel = log(Brel) - log(Brel_MRAMRY)) %>%
  group_by(stocklong, Status) %>%
  summarize(delta_Brel = mean(delta_Brel),
            years_to_MRA = mean(years_to_MRA))

g1 <- ggplot(depletion_diff) + 
  aes(x = years_to_MRA, y = delta_Brel, color = Status) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE,
              formula = y~x-1) +
  xlab("Years to MRA") + 
  scale_x_continuous(breaks = 0:20) + 
  ylab("Difference between \n depletion levels (ln)") +
  theme_classic()

g1
ggsave("Outputs/2023-03-10_depletion-differences.png", 
       g1, width = 5, height = 3)

depletion_diff %>% 
  group_by(Status) %>%
  summarize(n())
  
#Linear Model status

m1 <- lm(delta_Brel ~ years_to_MRA-1, data = filter(depletion_diff, 
                                                  Status == "Overfished"))
# plot(m1)
summary(m1)

m2 <- lm(delta_Brel ~ years_to_MRA-1, data = filter(depletion_diff, 
                                                  Status == "Sustainable"))
# plot(m1)
summary(m2)
