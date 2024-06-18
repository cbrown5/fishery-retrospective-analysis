#Process fishery data 
#Calculate B1, bias and other statistics for analysis
#
# CJ Brown 
#2024-01-10
#
# Note in paper we refer to the statistic 'B1' as
# the biomass in the initial year of the time-series.
# In the code this biomass in initial year is variable B0. 
# 2024-01-10 - updated to use Bmax instead of B1

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(patchwork)
library(readr)

#CB's package on github: cbrown5/DataGLMRepeat
# using for its 'within_group' function 
library(DataGLMRepeat)

dat <- read_csv("Data/stock-timeseries-databases.csv")

#RAM data to compare Bmax to B0 from assessments where available. 
RLSADB <- read_csv("Data/RLSADB v4.44 bioparams-view.csv")

useB1_asB0 <- FALSE
#Set to FALSE to use max(SSB), so to TRUE to use SSB in first year

if (useB1_asB0){
  suffix <- "B1"
} else {
  suffix <- "Bmax"
}

#
#Steps for Brel
#

dat2 <- select(dat,
               assessid,
               stocklong, 
               finish2, #final year of this assessment
               commonname,
               scientificname,
               region,
               country,
               initial, #initial year of this assessmnet
               `max finish2`, #most recent stock assess year
               tsyear, #year of data
               SSB) %>%
  filter(tsyear >= 1920)#truncate to begin in 1920
  

# ----------------- #
# Calculate B0 etc... 
# ----------------- #

# For each stock assessment: (using stocklong and final year columns)

# Calculate B0
dat_B0temp <- dat2 %>% 
  group_by(stocklong, finish2) %>%
  DataGLMRepeat::with_groups(.,{
  minyear = min(tsyear)
  minyear1 <- sort(unique(tsyear))[2] # nearest year after min
  
  if (useB1_asB0){
    B0 <- SSB[tsyear == minyear]
  } else {
    B0 <- max(SSB)
  }
  # # Calculate assessment year (last year of each assessment series)
  maxyear <- max(tsyear)
  
  dout <- data.frame(finish2 = finish2[1], 
                     stocklong = stocklong[1],
                     minyear = minyear, 
                     B0 = B0,
                     maxyear = maxyear)
  dout
  })

length(dat_B0temp) 
length(unique(dat2$assessid))

#visualize B0 values
hist(unlist(lapply(dat_B0temp, function(x) x$B0)))

#What are the really huge stocks? 
which(unlist(lapply(dat_B0temp, function(x) x$B0))>8E06)

#check if sums to NA
sum(unlist(lapply(dat_B0temp, function(x) x$B0))) #no NAs

dat_B0 <- do.call("rbind", dat_B0temp)

# Join to all data
dat3 <- dat2 %>% left_join(dat_B0, by = c("stocklong", "finish2"))
nrow(dat2) == nrow(dat3)

# Calculate Brel (SSB/B0)
dat3$Brel <- dat3$SSB/dat3$B0

#check that assessment year between databse and
# my calculations 
which(dat3$maxyear != dat3$finish2)

#
# Trends analysis
#

dat_trends <- dat3 %>% 
  group_by(finish2, stocklong) %>%
  DataGLMRepeat::with_groups(.,{
    minyear <- min(tsyear)
    
    maxyear <- max(tsyear)
    #Calculate trend using past three years. 
    #log trend (so rate of change)
    yrs <- sort(unique(tsyear))
    nyrs <- length(yrs)
    diff_BB0 <- trend <- trend2 <- rep(NA, nyrs)
    Bslope10 <- Bslope5 <- Bslope2<- NA
    #Trend every year for previous three years 
    if(nyrs>3){
      #second year difference, since not in loop below
      diff_BB0[2] <- Brel[2] - Brel[1]
    for (iyrs in 3:nyrs){
      
      ## Three year difference 
      #sometimes there are gaps, so trend over previous 3 years available
      #(not always consecutive years)
        trendyrs <- c(yrs[iyrs]-2, yrs[iyrs]-1, yrs[iyrs])
        iyrtemp <- which(tsyear %in% trendyrs)
        m1 <- lm(log(SSB[iyrtemp]) ~ tsyear[iyrtemp])
        trend[iyrs] <- coef(m1)[2]
        
        #TS difference
        #if tsyear is >1 year difference then takes an average between this year
        # and most recent previous year
        lastyear <- max(yrs[yrs<yrs[iyrs]]) #in most cases this is just yrs[iyrs]-1
        diff_BB0[iyrs] <- (Brel[tsyear == yrs[iyrs]] - Brel[tsyear == lastyear])/
          (yrs[iyrs]-lastyear)
        
        ## Two year difference 
        trendyrs <- c(yrs[iyrs]-1, yrs[iyrs])
        iyrtemp <- which(tsyear %in% trendyrs)
        m1 <- lm(log(SSB[iyrtemp]) ~ tsyear[iyrtemp])
        trend2[iyrs] <- coef(m1)[2]
        
    }
    }

    dout <- data.frame(finish2 = finish2, 
                       stocklong = stocklong,
                       tsyear = yrs,
                       trend = trend,
                       trend2 = trend2,
                       diff_BB0 = diff_BB0)
    dout
  })

dat_trends <- do.call("rbind", dat_trends)
dat_trends$trend_percent <- 100*(exp(dat_trends$trend)-1)

#Check number years per stock
dat2 %>% group_by(stocklong, finish2) %>%
  summarize(n = n()) %>%
  filter(n<5)

#Join to dat3 so we have matching trends each stock/year
dat3a <- dat3 %>%
  left_join(dat_trends)
nrow(dat3)
nrow(dat3a)
nrow(dat_trends)

#
# Identify Most Recent Assessment (max of maximum year per assessment)
#

dat_MRAB0 <- dat_B0 %>%
  group_by(stocklong) %>%
  filter(maxyear == max(maxyear)) %>%
  select(stocklong, minyearMRA = minyear,
         B0MRA_CB = B0, 
         maxyearMRA = maxyear) %>%
  ungroup() 

dat4 <- left_join(dat3a, dat_MRAB0) 
nrow(dat3) == nrow(dat4)

#More checks 
which(dat4$`max finish2` != dat4$maxyearMRA) #should be length 0 

#
# Join SSB in each year
#

#data - most recent assessment
dat_MRA <- dat4 %>%
  filter(finish2 == `max finish2`) %>% #MRAs
  select(stocklong, finish2, tsyear, SSB_MRA = SSB,  Brel_MRA = Brel,
          trend_MRA = trend, trend_MRA_percent = trend_percent,
         trend2_MRA = trend2, 
         B0_MRA = B0)

nrow(dat_MRA) < nrow(dat4)

#should be TRUE: 
length(unique(dat_MRA$stocklong)) == length(unique(dat4$stocklong))

sum(!(dat4$stocklong %in% dat_MRA$stocklong))
sum(!(dat4$stocklong %in% dat_MRA$stocklong))

#Join MRAs back to all assessment timeseries
# Calculate bias statistics


dat5 <- dat4 %>% 
  left_join(dat_MRA, by = c("stocklong", "tsyear")) %>%
  mutate(finalCB = (maxyear == maxyearMRA)+0) %>%
  # Bias in final year of each assessment  
  mutate(BrelLRR = log(Brel/Brel_MRA),
         trend_rel = trend - trend_MRA,
         trend2_rel = trend2 - trend2_MRA,
         BLRR = log(SSB/SSB_MRA),
         B0LRR = log(B0/B0_MRA))

nrow(dat4)
nrow(dat5)

#Filter for most recent year in each assessment
dat6 <- dat5 %>% filter(tsyear == maxyear & finalCB == 0)


#check LRR is always zero when using most recent year
all((dat5 %>% filter(tsyear == maxyear & finalCB ==1) %>% pull(BrelLRR))==0)
#Check trend is always zero in most recent year
all((dat5 %>% filter(tsyear == maxyear & finalCB ==1) %>% pull(trend_rel))==0)

# ----------------- #
# Stats 
# ----------------- #

#Number of stocks
nrow(dat6)

#
#Average by stocks 
#
dat6_bystock <- dat6 %>%
  group_by(stocklong, country) %>%
  summarize(BrelLRR = mean(BrelLRR, na.rm = TRUE),
            trend_rel = mean(trend_rel, na.rm = TRUE),
            trend_rel2 = mean(trend2_rel, na.rm = TRUE),
            BLRR = mean(BLRR, na.rm = TRUE),
            B0LRR = mean(B0LRR, na.rm = TRUE),
            Brel = mean(Brel, na.rm = TRUE))

dat_MRA_MRY <- filter(dat_MRA, tsyear == finish2)

# ------------ 
# Comparison to B0 as calculated from assessments 
# ------------ 

#This uses TB0 if available and SSB0 if not
adb_B0 <- RLSADB %>% filter(!is.na(TB0) | !is.na(SSB0)) %>% 
  mutate(B0_assessment = ifelse(is.na(TB0), SSB0, TB0)) %>%
           select(stocklong, B0_assessment) 
nrow(adb_B0)
dat_B0_2 <- left_join(dat_B0, adb_B0)
nrow(dat_B0)
nrow(dat_B0_2)

#Compare B initial (our 'B0') to 
# B0 in assessment database, for available stocks

plot(log10(dat_B0_2$B0_assessment),log10(dat_B0_2$B0))
abline(0,1)
abline(lm(log(B0)~log(B0_assessment), data = dat_B0_2), col = 'red')

summary(lm(log(B0)~log(B0_assessment), data = dat_B0_2))

#average difference
mean(with(dat_B0_2, {
  (B0-B0_assessment)/B0_assessment
}), na.rm = TRUE)

#
# Save data 
#

#Most recent year for each assessment 
dat_LRR <- dat5 #bias in all years
dat_MRY <- dat6 # bias just in most recent year
dat_MRY_stock_means <- dat6_bystock #mean bias by stocks
dat_B0 <- dat_B0_2

save(dat_B0, dat_MRA, dat_LRR, 
     dat_MRY, dat_MRY_stock_means, 
     file = paste0("Outputs/2024-01-10_processesed-assessment-data-",suffix,".rda"))

#
# Data frame for GLMMs
#

#Should be 756 stock assessments
nrow(dat_MRY) == 756

dat_glm <- dat_MRY %>%
  mutate(`year diff` = maxyearMRA - tsyear, 
         `start diff` = tsyear - minyear,
         `start diff MRA` = tsyear - minyearMRA,
         lnBB0 = log(Brel),
         `d slope SSB` = trend_rel,
         `d slope2 SSB` = trend2_rel,
           ) %>%
  select(stocklong, #stock IDs
         tsyear, #Assessment year
         `year diff`, #Most recent 
          # assessment year minus year of this assessment
          `start diff`, # This assessment's year minus
         #first year of this assessment
         `start diff MRA`,
         # This assessment's year minus the first year of the MRA
         lnBB0, #ln of B/B0 for this assessment
         `d B (ln B/B recent)`= BLRR, #log(SSB/SSB_MRA) where MRA is most 
         #recent assessment
         `d B0 (ln B0/B0 recent)` = B0LRR, #log(B0/B0_MRA)
         `d B/B0` = BrelLRR, #log(B_relative/B_relative_MRA) where
         # B_relative is B/B0
         `d slope SSB`, # trend - trend_MRA, where trend is the trend
         # the three years before the assessment and trend_MRA is the 
         # same for the MRA
         `d slope2 SSB`, #as above but over 2 years
         minyear
  )

write.csv(dat_glm, paste0("Outputs/2024-01-10glm-data-",suffix,".csv"),
          row.names = FALSE)


dat_glm_bystock <- dat6_bystock %>%
  left_join(dat_MRA_MRY) %>%
  mutate(lnBB0 = log(Brel)) %>% 
  select(stocklong, #stock IDs
         #Bslope10, #trend over 10 years
         # Bslope5,#trend over 5 years
         # Bslope2,
         `d B (ln B/B recent)`= BLRR, #log(SSB/SSB_MRA) where MRA is most 
         #recent assessment
         `d B0 (ln B0/B0 recent)` = B0LRR, #log(B0/B0_MRA)
         `d B/B0` = BrelLRR, #log(B_relative/B_relative_MRA) where
         # B_relative is B/B0
         `d slope SSB` = trend_rel,# trend - trend_MRA, where trend is the trend
         # the three years before the assessment and trend_MRA is the 
         # same for the MRA
         lnBB0,
        finish2_MRA = finish2, 
        tsyear_MRA = tsyear, 
        B_MRA = SSB_MRA,
        Brel_MRA,
        trend_MRA,
        trend_MRA_percent,
        B0_MRA
  )

write.csv(dat_glm_bystock, paste0("Outputs/2024-01-10_glm-data-stock-means-",suffix,".csv"),
          row.names = FALSE)






