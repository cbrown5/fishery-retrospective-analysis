# Figure 1 (Pacific cod plot) but for every stock
#CJ Brown 2022-11-16

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(ggforce)

load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

stocks <- unique(dat_LRR$stocklong)

nstocks <- length(stocks)
npages <- ceiling(nstocks/4)
#
# Depletion plots 
#

for (i in 1:npages){
g2 <- ggplot(dat_LRR) + 
  aes(x = tsyear, y = Brel, color = finish2.x,
      group = finish2.x) + 
  geom_hline(yintercept = 1, color = "grey60", linetype = 3) +
  geom_hline(yintercept = 0.4, color = "grey60", linetype = 2) +
  geom_line() + #position = position_dodge(width = 3))  +
  facet_wrap_paginate(~ stocklong, ncol = 2, nrow = 2,
                      page = i,
                      scales = "free")+
  scale_color_viridis_c("Assmnt \n year", direction = -1) +
  scale_y_log10() +
  theme_classic() +
  xlab("Year") + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  theme(axis.title.y = element_text(lineheight = 0.5))
print(i)
ggsave(g2, 
       width = 10, height = 5,
       filename = paste0("Outputs/subplots/subplot_p",i,".png"))
}
