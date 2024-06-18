# Plot of mean depletion trends 
# Using sustainbility categories of :
 #0.1, 0.3, 0.5
#CJ Brown 2023-11-15

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

# load("Outputs/2022-02-11_processesed-assessment-data.rda")
# load("Outputs/2024-01-10_processesed-assessment-data-B1.rda")
load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

dat_LRR2 <- dat_LRR %>%
  # filter(stocklong == "Pacific cod_Gulf of Alaska") %>%
  filter(tsyear > 1959 & !is.na(finish2.y)) %>%
  #Get rid of years of data missing an MRA 
  mutate(years_to_MRA = finish2.y - finish2.x,
         years_before_assessment = finish2.y - tsyear,
         # finish2.y = MRY of MRA
         # finish2.x = MRY of this assess
         assess_age = case_when(
           years_to_MRA == 0 ~ "MRA",
           years_to_MRA >0 & years_to_MRA <= 3 ~ "1-3 yr old",
           years_to_MRA >3 & years_to_MRA <= 8 ~ "4-8 yr old",
           years_to_MRA >8 ~ ">8 yr old"
         )) 

# write.csv(select(dat_LRR2, assessid, stocklong, Year_of_MRA = finish2.x,
#                  Year_of_assessment = finish2.y,
#                  commonname, scientificname, region, country, 
#                  years_to_MRA, assess_age, 
#                  SSB, SSB_MRA, Brel, Brel_MRA, BrelLRR), 
#           file = "Outputs/LRR.csv",
#      row.names = FALSE)

#axes limits 
xmin <- 1980
xmax <- 2020
ymin <- 0
ymax <- 1

yaxis <- scale_y_continuous(breaks = seq(0, 1, by = 0.25),
                            limits = c(ymin, ymax),
                            labels = seq(0, 1, by = 0.25))

#
# ALL STOCKS 
#

dat_assess_mean <- dat_LRR2 %>%
  group_by(tsyear, assess_age, stocklong) %>%
  summarize(Brel = exp(mean(log(Brel))),
    n = n()) %>%
  group_by(tsyear, assess_age) %>%
  summarize(Depletion = exp(mean(log(Brel))),
            n = n()) %>%
  ungroup() %>%
  filter(n>10) %>%
  #Remove year/group combos with <4 assessments
  group_by(assess_age) 
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
  theme_classic() + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlab("Year") + 
  xlim(xmin, xmax) +
  yaxis +
  scale_color_manual("Assessment age", 
                     values = pal)
g1
# ggsave("Outputs/mean-depletion.png")

#Check status in particular years
(filter(dat_assess_mean, tsyear == 1980))
(filter(dat_assess_mean, tsyear == 2005))
(filter(dat_assess_mean, tsyear == 2008))
(filter(dat_assess_mean, tsyear == 2016))

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
  select(stocklong, Brel_MRA, years_before_assessment) %>%
  #Use Brel for the year of the datapoint, but in the MRA
  mutate(status = case_when(Brel_MRA<=0.1 ~ "Collapsed",
                            Brel_MRA>0.1 & Brel_MRA<=0.3 ~ "Overfished",
                            Brel_MRA>0.3 & Brel_MRA<=0.5 ~ "Sustainable",
                            Brel_MRA>0.5 ~ "Lightly fished")) %>%
  select(stocklong, status)

#Stocks per category
stock_status_MRAMRY %>%
  select(status, stocklong) %>%
  distinct() %>%
  group_by(status) %>%
  summarize(n())

#MRY MRA Status

dat_LRR2 %>%
  #just the MRAs
  filter(finish2.y == finish2.x) %>%
  #stock status X years before MRY of the MRA
  mutate(MRAMRY_min5 = finish2.y - 0)  %>%
  filter(tsyear == MRAMRY_min5) %>% 
  # filter(tsyear == finish2.y) %>% #deactivate if I want
  # to have status in year of ts. finish2.y is final year of MRA
  select(stocklong, Brel_MRA, years_before_assessment) %>% 
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
68/230
162/230

#
# Depleted and sustainable stocks
#

dat_assess_mean_status <- dat_LRR2 %>%
  left_join(stock_status_MRAMRY) %>%
  group_by(tsyear, assess_age, status, stocklong) %>%
  summarize(Brel =
              exp(mean(log(Brel))),
            n = n()) %>%
  ungroup() %>%
  group_by(tsyear, assess_age, status) %>%
  summarize(Depletion = 
              exp(mean(log(Brel))),
            n = n()) %>%
  filter(n>5) %>%
  ungroup() %>%
  group_by(assess_age) 

dat_assess_mean_status$assess_age <- 
  factor(dat_assess_mean_status$assess_age,
         levels = c("MRA",
                    "1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old"))

gall <- NULL
status_names <- unique(stock_status_MRAMRY$status)[c(1,4,2,3)]
# status_names <- unique(stock_status_MRAMRY$status)[c(1,3,2,4)]
for (i in status_names){
  gtemp <- 
    dat_assess_mean_status %>%
    filter(status == i) %>% 
    ggplot() + 
    #Filter out dip in final year (due to small sample bias)
    aes(x = tsyear, y = (Depletion),
        color = assess_age, group = assess_age) + 
    geom_hline(yintercept = 1, color = "grey60") +
    geom_hline(yintercept = 0.4, color = "grey60", 
               linetype = 2) +
    geom_line() +
    theme_classic() + 
    ylab(expression('Depletion (B/B'[1]*')')) +
    xlab("Year") + 
    xlim(xmin, xmax) + 
    yaxis +
    scale_color_manual("Assessment age", 
                       values = 
                         pal)
  gall <- c(gall, list(gtemp))
}

gall2 <- (gall[[1]] + gall[[2]]) / (gall[[3]] + gall[[4]]) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides='collect') 
gall2


ggsave("Outputs/depletion_timeseries-figures-all-scales-same_4-status-cat-Bmax.png", 
       gall2,
       width = 8, height = 4)


#
# Check stocks for specific plot
#

gall[[3]]

x <- dat_LRR2 %>%
  left_join(stock_status_MRAMRY) %>%
  filter(status == "Overfished")

dat_overfished <- dat_assess_mean_status %>%
  filter(status == "Overfished")

stocks_with_8yrolds <- unique(filter(x, assess_age == ">8 yr old")$stocklong)
stocks_with_4yrolds <- unique(filter(x, assess_age == "4-8 yr old")$stocklong)

x1 <- x %>%
  filter(stocklong %in% stocks_with_8yrolds) %>%
  ggplot() + 
  aes(x = tsyear, y = Brel) + 
  geom_line(aes(color = stocklong, 
                group = paste0(stocklong, finish2.x))
            ) + 
  geom_line(dat = dat_overfished, 
            aes(y = Depletion),
            color = "black") +
  facet_wrap(~assess_age)# + 
  # ylim(0, 1)

library(plotly)

ggplotly(x1)
