#Brings together the panels for the supplemental version
# of figure 2, which has complete timeseries 1980-2010
# and includes percentage difference plots 

# CJ Brown 2023-03-02

rm(list = ls())
library(ggplot2)
library(dplyr)
library(patchwork)

load("Outputs/timeseries-plots-1980_2010.rda")

gA <- g1
gB <- g2
gC <- g3
gD <- g4

load("Outputs/timeseries-diff-plots.rda")

gall <- (gA + gB + gC) / (gD + g2 + g3) + 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") + 
  plot_layout(guides='collect') 

ggsave("Outputs/fig2-6panels.png", gall,
       width = 10, height = 4)


#
# Sample size plots 
#
xmin <- 1980
xmax <- 2020
ymin <- 0
ymax <- 350
yaxis <- scale_y_continuous(breaks = seq(0, ymax, by = 50),
                            limits = c(ymin, ymax),
                            labels = seq(0, ymax, by = 50))

pal <- c("black", "#E69F00", "#56B4E9", "#009E73")#, "#0072B2")

gA <- dat_assess_mean %>%
  filter(n>14) %>%
  ggplot() + 
    aes(x = tsyear, y = n,
        color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of assessments") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gA

x <- filter(dat_assess_mean_status, status == "Sustainable") %>%
  filter(n>14 & tsyear > 1980) %>%
  filter(assess_age == "MRA") 

y <- filter(dat_status_diff, status == "Sustainable") %>%
  filter(n>14 & tsyear > 1980) %>%
  filter(assess_age == "MRA") 

gB <- dat_assess_mean_status %>%
  filter(status == "Sustainable") %>%
  filter(n>14) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of assessments") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gB

gC <- dat_assess_mean_status %>%
  filter(status == "Depleted") %>%
  filter(n>14) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of assessments") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gC


gD <- dat_assess_mean_10yrold %>%
  filter(n>14) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of assessments") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gD

gE <- dat_status_diff %>%
  filter(status == "Sustainable") %>%
  filter(n>14) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of assessments") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) + 
  theme(legend.position = "none")

gE

gF <- dat_status_diff %>%
  filter(status == "Depleted") %>%
  filter(n>14) %>%
  ggplot() + 
  aes(x = tsyear, y = n,
      color = assess_age, group = assess_age) +
  geom_line() +
  theme_classic() + 
  ylab("Number of assessments") +
  xlab("Year") + 
  xlim(xmin, xmax) + 
  yaxis +  
  scale_color_manual("Assessment age", 
                     values = pal) 

gF
gall <- (gA + gB + gC) / (gD + gE + gF) + 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") + 
  plot_layout(guides='collect') 
gall

ggsave("Outputs/depletion_timeseries-figures-sample-size-all-scales-same.png", gall,
       width = 10, height = 4)


