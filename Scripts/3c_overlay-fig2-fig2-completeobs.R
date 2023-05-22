# Plot of mean depletion trends 
#CJ Brown 2023-05-15
#
#Figure 2, but with the version with complete obs 1980-2010 overlayed over the 
#top

rm(list =ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

load("Outputs/timeseries-plots-1980_2010.rda")
load("Outputs/timeseries-plots-complete-1980_2016_withSEs.rda")

#Remove SE shading
g1$layers[[4]] <- g2$layers[[4]] <-
g3$layers[[4]] <-
g4$layers[[4]] <- NULL

g1b <- g1 + 
    geom_line(data = dat_assess_mean, 
              alpha = 0.5,
              linewidth = 1)
  
g2b <- g2 + 
  geom_line(data = filter(dat_assess_mean_status,
                          notmax & status == "Sustainable"), 
            alpha = 0.5,
            linewidth = 1) 

g3b <- g3 + 
  geom_line(data = filter(dat_assess_mean_status,
                          notmax & status == "Depleted") ,
            alpha = 0.5,
            linewidth = 1) 

g4b <- g4 + 
   geom_line(data = dat_assess_mean_10yrold,
            alpha = 0.5,
            linewidth = 1)


gall <- (g1b + g4b) / (g2b + g3b) + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides='collect') 

ggsave("Outputs/fig2-overlay-noSEs.png", gall,
       width = 8, height = 4)
