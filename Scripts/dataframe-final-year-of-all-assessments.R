#Dataframe for final year of each assesssment
# CJ Brown 2024-02-01


library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

# load("Outputs/2022-02-11_processesed-assessment-data.rda")
load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

dout <- dat_LRR %>%
  filter(tsyear == finish2.x) %>%
  select(assessid, stocklong, commonname, 
         scientificname, region, country, tsyear,
         MRA_year = `max finish2`,
         SSB, SSB_MRA, Brel, Brel_MRA, B0, B0_MRA, 
         BrelLRR, BLRR, 
         B0LRR) %>%
  mutate(is_MRA = MRA_year == tsyear)
nrow(dout)
readr::write_csv(dout, file = "Outputs/assessements and Log Response Ratios final year of each assessment.csv")
