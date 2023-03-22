#Make figure 3
# CJ Brown
# 2023-03-23
rm(list = ls())
library(ggplot2)
library(patchwork)

load("Outputs/2023-03-10_plots-main-models.rda")
load("Outputs/2023-03-23_plots-stability-model.rda")

gallfix <- gfixie[[1]] + 
  (gfixie[[2]] + theme(axis.text.y = element_blank())) +
  (gfixie[[3]]+ theme(axis.text.y = element_blank())) + 
  (g1_2ndMRA+ theme(axis.text.y = element_blank())) + 
  plot_annotation(tag_levels ="a",
                  tag_prefix = "(",
                  tag_suffix = ")") + 
  plot_layout(guides='collect',
              nrow = 1) 

ggsave("Outputs/fixed-effects-deltas.png",
       gallfix,
       width = 8, height =3)
