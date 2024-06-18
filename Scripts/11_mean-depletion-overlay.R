#Make figure 3
# CJ Brown
# 2023-03-23

rm(list = ls())
library(ggplot2)
library(patchwork)

load("Outputs/timeseries-diff-plots-Bmax-5year.rda")
gA <- g2
gD <- g3

load("Outputs/brel-and-trend-bias-Bmax-5year.rda")

ylim <- c(0.8, 1.5)
gA <- gA +
    theme(legend.position = c(0.45, 0.9),
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size=8),
          legend.title = element_text(size=10)) + 
  ylim(ylim)+ 
  xlab("")
gA
gD <- gD + ylim(ylim)

gBrel_sus <- gBrel_sus + 
  theme(legend.position = "none") + ylim(ylim)+ 
  xlab("")

gBrel_dep <- gBrel_dep + ylim(ylim) + 
  ylab(expression(Delta*'B/B'[max])) + 
  theme(legend.position = "none")

ylim2 <- c(0.975, 1.05)

gtrend_sus <- gtrend_sus + ylim(ylim2) + 
  theme(legend.position = "none") + 
  xlab("")


gtrend_dep <- gtrend_dep+ ylim(ylim2) + 
  theme(legend.position = "none") + 
  ylab(expression(Delta*' trend'))


gallfix <- 
  (gA + gBrel_sus + gtrend_sus)/(gD + gBrel_dep + gtrend_dep) + 
  plot_annotation(tag_levels ="A")+ 
  plot_layout(nrow = 2)

gallfix

ggsave("Outputs/Brel and trend bias 5 year.png",
       gallfix,
       width = 8, height =6)
