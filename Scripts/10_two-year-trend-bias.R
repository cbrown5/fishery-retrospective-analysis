#Bias in trend over 2 year intervals
# CJ Brown 2024-01-30

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(DataGLMRepeat)

# load("Outputs/2022-02-11_processesed-assessment-data.rda")
load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

number_of_years_prior_MRA <- 0
#number of years prior to MRA to classify stock status

#axes settings 
xmin <- 1980 #1980
xmax <- 2020
ymin <- -0.01
ymax <- 0.065

yaxis <- scale_y_continuous(breaks = seq(-0.01, 0.05, by = 0.02),
                            limits = c(ymin, ymax),
                            labels = seq(-0.01, 0.05, by = 0.02))

yaxis2 <- scale_y_continuous(breaks = seq(-0.1, 0.5, by = 0.2),
                            limits = c(-0.1, 0.5),
                            labels = seq(-0.1, 0.5, by = 0.2))


pal <- c("#E69F00", "#56B4E9", "#009E73")#, "#0072B2")

#
# Calculate trends and trend differences
#

dat_trend_diff <- dat_LRR %>%
  #select each assessment
  group_by(finish2.x, stocklong) %>%
  #Difference timeseries for each assessment to get 2 year trends
  DataGLMRepeat::with_groups(.,{
    
    x <- data.frame(tsyear, Brel, Brel_MRA) %>%
      arrange(tsyear)
    diff_year <- diff(x$tsyear)
    
    #LRR of B/B1
    deltaBrel <- exp(log(Brel/Brel_MRA))-1
    
    #instantaneous per year trends 
    Brel_trend <- diff(log(x$Brel))/diff_year
    Brel_MRA_trend <- diff(log(x$Brel_MRA))/diff_year
    trend_diff <- exp(Brel_trend - Brel_MRA_trend)-1
    
    dout <- data.frame(finish2 = finish2.x[1], 
                         years_to_MRA = max(finish2.y, na.rm = TRUE) - finish2.x[1],
                         years_to_MRY = finish2.x[1] - tsyear[2:nrow(x)], 
                         # finish2.y = MRY of MRA
                         # Use max(finish2.y, na.rm = TRUE)
                         #here because "Redfish SE Australia" has NAs
                         # for the 1999 assessment in years that predate the
                         # earliest year included in the timeseries for the 
                         #2013 MRA. If used [1] the MRY of MRA is NA for this stock
                         # finish2.x = MRY of this assess
                         stocklong = stocklong[1],
                         tsyear = tsyear[2:nrow(x)],
                         deltaBrel = deltaBrel[2:nrow(x)],
                         obsolescence = finish2.y[1] - tsyear[2:nrow(x)],
                         trend_diff = trend_diff,
                         Brel_trend = Brel_MRA_trend,
                         Brel_MRA_trend = Brel_MRA_trend)
    dout
  })

#bind them
dat_trend_diff <- do.call("rbind", dat_trend_diff)

#
# Calculate status and obsolesence categories 
#

stock_status_MRA <- dat_LRR %>%
  filter(finish2.y == finish2.x) %>%
  #stock status X years before MRY of the MRA
  mutate(MRAMRY_min5 = finish2.y - number_of_years_prior_MRA)  %>%
  # mutate(MRAMRY_min5 = finish2.y)  %>%
  filter(tsyear == MRAMRY_min5) %>% 
  select(stocklong, Brel_MRA, tsyear) %>%
  #Use Brel for the year of the datapoint, but in the MRA
  mutate(status = ifelse(Brel_MRA>0.4, "Sustainable",
                         "Depleted")) %>%
  select(stocklong, status)
  
dat_trend_diff2 <- dat_trend_diff %>%
  mutate(
         assess_age = case_when(
           years_to_MRA == 0 ~ "MRA",
           years_to_MRA >0 & years_to_MRA <= 3 ~ "1-3 yr old",
           years_to_MRA >3 & years_to_MRA <= 8 ~ "4-8 yr old",
           years_to_MRA >8 ~ ">8 yr old"
         )) %>%
  left_join(stock_status_MRA)

#
# Effect of stock average bias on MRA trend 
#

#Get trend in final year
stock_trend_bias <- dat_trend_diff2 %>%
  filter((years_to_MRY == 0)  & (assess_age != "MRA")) %>%
  group_by(stocklong) %>%
  #Average bias across assessments
  # Taking log below because above we took exponent
  # to make the bias a %
  summarize(mean_trend_LRR = mean(log(trend_diff+1))) 

#filter for MRY MRA to get that trend, then join to add bias on 
MRA_trend <- dat_trend_diff2 %>%
  filter((years_to_MRY == 0)  & (assess_age == "MRA")) %>%
  left_join(stock_trend_bias, by = "stocklong") %>%
  #Adjusted trend (instantaneous, per year) = 
  # MRA trend + LRR for trends
  mutate(adjusted_trend = Brel_MRA_trend + mean_trend_LRR)

with(MRA_trend, plot(Brel_MRA_trend,adjusted_trend))
abline(h=0)
abline(v=0)

#Which ones change direction?
flips <- MRA_trend %>%
  mutate(flip_pos = (Brel_MRA_trend <= 0) & (adjusted_trend>0),
         flip_neg = (Brel_MRA_trend >= 0) & (adjusted_trend<0))


#which ones were negative, but go positive
flips %>%
  summarize(sum(flip_pos))

#which ones were positive, but go negative
flips %>%
  summarize(sum(flip_neg))

flips %>%
  group_by(status, flip_neg, flip_pos) %>%
  summarize(n())

#how many by status? 
flips %>%
  group_by(status) %>%
  summarize(n())

#
# Summary stats 
#

dat_assess_mean_status <- dat_trend_diff2 %>%  
  group_by(years_to_MRY, assess_age, status, stocklong) %>%
  summarize(trend_diff = mean(trend_diff),
            deltaBrel = mean(deltaBrel)) %>%
  group_by(years_to_MRY, assess_age, status) %>%
  summarize(median_trend = median(trend_diff),
            median_deltaBrel = median(deltaBrel),
            n = n()) %>%
  filter(n>9) %>%
  ungroup() %>%
  group_by(assess_age) %>%
  filter(assess_age != "MRA")

dat_assess_mean_status$assess_age <- 
  factor(dat_assess_mean_status$assess_age,
         levels = c("1-3 yr old",
                    "4-8 yr old",
                    ">8 yr old"))
#
# Plot delta Brel by depleted and sustainable stocks
#

g4 <- 
  dat_assess_mean_status %>%
  filter(status == "Sustainable") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = years_to_MRY, y = exp(median_deltaBrel),
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 1) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab(expression(Delta*'B/B'[max])) +
  xlab("Years before assessment") + 
  xlim(0, 15) +
  scale_color_manual("Assessment age", 
                     values = 
                       pal) +
  scale_fill_manual("Assessment age", 
                    values = pal)

g5 <- 
  dat_assess_mean_status %>%
  filter(status == "Depleted") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = years_to_MRY, y = exp(median_deltaBrel),
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 1) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab("") +
  xlab("Years before assessment") + 
  xlim(0, 15) +
  scale_color_manual("Assessment age", 
                     values = 
                       pal) +
  scale_fill_manual("Assessment age", 
                    values = pal)

g4 + g5
#
# Plot trends by depleted and sustainable stocks
#


g2 <- 
  dat_assess_mean_status %>%
  filter(status == "Sustainable") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = years_to_MRY, y = exp(median_trend),
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 1) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab(expression(Delta*' trend')) +
  xlab("Years before assessment") + 
  xlim(0, 15) +
  scale_color_manual("Assessment age", 
                     values = 
                       pal) +
  scale_fill_manual("Assessment age", 
                     values = pal)

g3 <- 
  dat_assess_mean_status %>%
  filter(status == "Depleted") %>% 
  ggplot() + 
  #Filter out dip in final year (due to small sample bias)
  aes(x = years_to_MRY, y = exp(median_trend),
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 1) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab("") +
  xlab("Years before assessment") + 
  xlim(0, 15) +
  scale_color_manual("Assessment age", 
                     values = 
                       pal) +
  scale_fill_manual("Assessment age", 
                    values = pal)

#
# Combined plot
#
gBrel_sus <- g4
gBrel_dep <- g5
gtrend_sus <- g2
gtrend_dep <- g3

save(gBrel_sus, gBrel_dep, gtrend_sus, gtrend_dep, 
     file = paste0("Outputs/brel-and-trend-bias-Bmax-",number_of_years_prior_MRA,"year.rda"))

gall <- (g4 + g5)/(g2 + g3) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides='collect') & 
  theme(plot.tag = element_text(face = 'bold'))
gall

#save plot
ggsave(paste0("Outputs/bias-in-trends-Brel-Bmax-",number_of_years_prior_MRA,"-year-prior-MRA.png"), gall,
       width = 8, height = 6)
