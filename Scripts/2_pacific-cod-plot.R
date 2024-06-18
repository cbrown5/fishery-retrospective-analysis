#Pacific cod plot
#CJ Brown 2024-02-02

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(readr)

load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

pcdat <- filter(dat_LRR, stocklong == "Pacific cod_Gulf of Alaska")

#
# Load SD data (not provided in the repo, need to contact
# assessment authors to access)
#

sddat <- readxl::read_xlsx("Data/Pacific cod SSB and STD data2.xlsx")

sddat <- filter(sddat, Year < 2019)

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
  geom_ribbon(data = sddat, 
              aes(x = Year, ymin = (SSB_2018 - SD_2018)/1000,
                  ymax = (SSB_2018 + SD_2018)/1000,
                  y = SSB_2018), 
              group = 1,
              alpha = 0.5,
              color = NA, 
              fill = "grey20")  +
  geom_line() + 
  scale_color_manual("", values = pal) + 
  geom_line(data = filter(pcdat, finish2.x == 2018), 
            color = "black", size = 0.7) +
  geom_line(data = filter(pcdat, finish2.x == 2014), 
            color = "black", size = 0.9, 
            linetype = 5) + 
  #add orange arrow for delta
  geom_segment(aes(x = 2014, xend = 2014, y = 110, yend = 185),
               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "cm")),
               color = "orange", size = 0.65) +
  geom_segment(aes(x = 2014, xend = 2014, y = 185, yend = 110),
               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "cm")),
               color = "orange", size = 0.65)+
  
  #dashed horizontal arrow from 2014 to 2018 
  # geom_segment(aes(x = 2014, xend = 2016, y = 150, yend = 150),
  #              arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "cm")),
  #              color = "black", size = 0.5, linetype = 1) +
  #add delta at end of the dashed arrow
  geom_text(aes(x = 2018, y = 160, 
                label = as.character(expression(Delta*'B'[max]))),
            color = "black", size = 6, fontface = "plain",
            size.unit = "pt",parse = T,check_overlap = TRUE) +    
  geom_text(aes(x = 2018, y = 135, 
                label = '(2014)'),
            color = "black", size = 6, fontface = "plain",
            size.unit = "pt",check_overlap = TRUE) +
  theme_classic() +
  xlab("Year") + 
  ylab("Spawning stock biomass \n
       ('000s tonnes)")  +
  scale_y_continuous(breaks = seq(0, 500, by = 100),
                     labels= comma,expand = c(0, 0),
                     limits = c(0, 600)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1976, 2020)) +
  # scale_y_log10(breaks = c(50, 100, 200, 400))  +
  guides(color = guide_legend(ncol = 3))+
  theme(axis.title.y = element_text(lineheight = 0.5),
        legend.position = c(0.76, 0.95),
        legend.text=element_text(size=7,
                                 lineheight = 0.1),
        axis.text = element_text(color = "black"),
        axis.title=element_text(size=9),
        legend.key.size = unit(0.5, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.background = element_rect(fill='transparent'))
g1

#
# Depletion plots 
#

g2 <- ggplot(pcdat) + 
  aes(x = tsyear, y = Brel, color = factor(finish2.x),
      group = finish2.x)+
  geom_rect(aes(xmin = 1976, xmax = 2020, ymin = 0, ymax = 0.4), 
            fill = "#e6f2ff", alpha = 1, color = NA)  +
  geom_rect(aes(xmin = 1976, xmax = 2020, ymin = 0, ymax = 0.1), 
            fill = "#b3d9ff", alpha = 0.5, color = NA) + 
  #add font for Overfished
  geom_text(aes(x = 1991, y = 0.3, label = "Overfished"), 
            color = "black", size = 8, fontface = "plain",
            size.unit = "pt",check_overlap = TRUE) +
  geom_text(aes(x = 1991, y = 0.05, label = "Collapsed"), 
            color = "black", size = 8, fontface = "plain",
            size.unit = "pt",check_overlap = TRUE) +   
  geom_text(aes(x = 2010, y = 0.75, label = "Sustainably fished"),
                size.unit = "pt", 
            color = "black", size = 8, fontface = "plain",check_overlap = TRUE) +   
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
  #add orange arrow for delta
  geom_segment(aes(x = 2014, xend = 2014, y = 0.33, yend = 0.4),
                arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "cm")),
                color = "orange", size = 0.65) +
  geom_segment(aes(x = 2014, xend = 2014, y = 0.4, yend = 0.33),
               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "cm")),
               color = "orange", size = 0.65)+
  
  #dashed horizontal arrow from 2014 to 2018 
  # geom_segment(aes(x = 2014, xend = 2016, y = 0.36, yend = 0.36),
  #               arrow = arrow(type = "closed", angle = 30, length = unit(0.1, "cm")),
  #               color = "black", size = 0.5, linetype = 1) +
  #add delta at end of the dashed arrow
  geom_text(aes(x = 2018, y = 0.36, 
                label = as.character(expression(Delta*'B/B'[max]))),
            color = "black", size = 6, fontface = "plain",
            size.unit = "pt",parse = T,check_overlap = TRUE) +    
  geom_text(aes(x = 2018, y = 0.3, 
                label = '(2014)'),
            color = "black", size = 6, fontface = "plain",
            size.unit = "pt",check_overlap = TRUE) +    
  scale_y_continuous(breaks = c(0, 0.25, 0.4, 0.5, 0.75, 1),
                     limits = c(0, 1),expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1976, 2020)) +
  # scale_y_log10(breaks = c(0.25, 0.5, 1,2, 3)) +
  theme_classic() +
  xlab("Year") + 
  ylab(expression('Depletion (B/B'[max]*')')) +
  theme(axis.title.y = element_text(lineheight = 0.5),
        axis.title=element_text(size=9),
        axis.text = element_text(color = "black"),
        legend.position = "none")

gall <- g1/g2 +  
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = 'bold'))

gall

ggsave("Outputs/pacific-cod-figures.pdf", gall,
       width = 12, height = 12*6/5, units = "cm")

ggsave("Outputs/pacific_cod_time-series.png", g1,
       width = 5.5, height = 3.8)

ggsave("Outputs/pacific_cod_depletion.png", g2)
