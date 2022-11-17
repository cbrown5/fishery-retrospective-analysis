#Pacific cod plot
#CJ Brown 2022-06-12

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)

load("Outputs/2022-02-11_processesed-assessment-data.rda")

pcdat <- filter(dat_LRR, stocklong == "Pacific cod_Gulf of Alaska")

#
# SSB plots 
#

ggplot(pcdat) + 
  aes(x = tsyear, y = SSB, color = finish2.x,
      group = finish2.x) + 
  geom_line() + 
  geom_line(aes(y = SSB_MRA), 
            color = "hotpink", size = 1) +
  # geom_line(data = filter(pcdat, finish2.x == 2018), 
            # color = "black", size = 1) +
  scale_color_gradient("",high = "darkblue",
                       low = "lightblue") +
  theme_classic() +
  xlab("Year") + 
  ylab("Spawning stock biomass \n
       (Tonnes)") 

unique(pcdat$finish2.x)

pal <- c("#E69F00", "#56B4E9", "#009E73")
pal <- rev(colorRampPalette(pal)(12))
pal <- viridis_pal()(12)
# pal <- RColorBrewer::brewer.pal(9, "BuPu")
# pal <- colorRampPalette(pal)(14)[3:14]
pal[12] <- "black"

pcdat$assess_year <- as.character(pcdat$finish2.x)
pcdat <- within(pcdat, {
  assess_year[finish2.x == 2018] <-  "2018 (MRA)"
})


g1 <- ggplot(pcdat) + 
  aes(x = tsyear, y = SSB/1000, color = assess_year,
      group = finish2.x) + 
  geom_line() + 
  scale_color_manual("", values = pal) + 
  geom_line(data = filter(pcdat, finish2.x == 2018), 
            color = "black", size = 0.7) +
  geom_line(data = filter(pcdat, finish2.x == 2014), 
            color = "black", size = 0.9, 
            linetype = 5) +
  theme_classic() +
  xlab("Year") + 
  ylab("Spawning stock biomass \n
       ('000s tonnes)")  +
  xlim(1972, 2020) +
  # scale_y_continuous(breaks = seq(0, 500, by = 100),
                     # labels= comma,
                     # limits = c(0, 600)) +
  scale_y_log10(breaks = c(50, 100, 200, 400))  +
  guides(color = guide_legend(ncol = 3))+
  theme(axis.title.y = element_text(lineheight = 0.5),
        legend.position = c(0.8, 0.95),
        legend.text=element_text(size=6,
                                 lineheight = 0.1),
        legend.key.size = unit(0.5, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill='transparent'))
g1

#
# Depletion plots 
#

g2 <- ggplot(pcdat) + 
  aes(x = tsyear, y = Brel, color = factor(finish2.x),
      group = finish2.x) + 
  geom_hline(yintercept = 1, color = "grey60", linetype = 3) +
  geom_hline(yintercept = 0.4, color = "grey60", linetype = 2) +
  # geom_hline(yintercept = 0.1, color = "grey60") +
  geom_line() + 
  scale_color_manual("", values = pal) + 
  geom_line(data = filter(pcdat, finish2.x == 2018), 
            color = "black", size = 1) +
  geom_line(data = filter(pcdat, finish2.x == 2014), 
                                                  color = "black", size = 0.9, 
                                                  linetype = 5) +
  # scale_y_continuous(breaks = c(0, 0.25, 0.5, 1, 2, 4),
                     # limits = c(0, 4)) +
  scale_y_log10(breaks = c(0.25, 0.5, 1,2, 3)) +
  theme_classic() +
  xlab("Year") + 
  ylab(expression('Depletion (B/B'[1]*')')) +
  xlim(1972, 2020) +
  theme(axis.title.y = element_text(lineheight = 0.5),
        legend.position = "none")

gall <- g1/g2 +  
  plot_annotation(tag_levels = "A")

ggsave("Outputs/pacific-cod-figures.png", gall,
       width = 5, height = 6)

ggsave("Outputs/pacific_cod_time-series.png", g1,
       width = 5.5, height = 3.8)

ggsave("Outputs/pacific_cod_depletion.png", g2)
