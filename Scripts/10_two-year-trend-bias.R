#Bias in trend over 2 year intervals
# CJ Brown 2023-02-24

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(DataGLMRepeat)

load("Outputs/2022-02-11_processesed-assessment-data.rda")

#axes settings 
xmin <- 1980 #1980
xmax <- 2020
ymin <- -0.06
ymax <- 0.065

yaxis <- scale_y_continuous(breaks = seq(-0.05, 0.05, by = 0.02),
                            limits = c(ymin, ymax),
                            labels = seq(-0.05, 0.05, by = 0.02))

yaxis2 <- scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.2),
                            limits = c(-0.5, 0.5),
                            labels = seq(-0.5, 0.5, by = 0.2))


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
                         years_to_MRA = finish2.y[1] - finish2.x[1],
                         years_to_MRY = finish2.x[1] - tsyear[2:nrow(x)], 
                         # finish2.y = MRY of MRA
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
  mutate(MRAMRY_min5 = finish2.y - 5)  %>%
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
# Summary stats 
#

dat_assess_mean_status <- dat_trend_diff2 %>%
  group_by(years_to_MRY, assess_age, status) %>%
  summarize(median_trend = median(trend_diff),
            median_deltaBrel = median(deltaBrel),
            n = n()) %>%
  filter(n>14) %>%
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
  aes(x = years_to_MRY, y = median_deltaBrel,
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 0) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab(expression(Delta*'B/B'[1])) +
  xlab("Years before assessment") + 
  xlim(0, 15) +
  yaxis2 +
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
  aes(x = years_to_MRY, y = median_deltaBrel,
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 0) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab("") +
  xlab("Years before assessment") + 
  xlim(0, 15) +
   yaxis2 +
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
  aes(x = years_to_MRY, y = median_trend,
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 0) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab(expression(Delta*' trend')) +
  xlab("Years before assessment") + 
  xlim(0, 15) +
  yaxis +
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
  aes(x = years_to_MRY, y = median_trend,
      color = assess_age, group = assess_age,
      fill = assess_age) +
  geom_hline(yintercept = 0) +
  geom_line() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp")) + 
  theme_classic() + 
  ylab("") +
  xlab("Years before assessment") + 
  xlim(0, 15) +
  yaxis +
  scale_color_manual("Assessment age", 
                     values = 
                       pal) +
  scale_fill_manual("Assessment age", 
                    values = pal)

#
# Combined plot
#

gall <- (g4 + g5)/(g2 + g3) + 
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides='collect') & 
  theme(plot.tag = element_text(face = 'bold'))
gall

#save plot
# ggsave("Outputs/bias-in-trends-Brel.png", gall,
       # width = 8, height = 6)
