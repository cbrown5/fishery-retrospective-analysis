# GLM of bias for B/B0 ("Brel")
#
# CJ Brown 2022-07-26
#
# Fits multiple GLMs to assess correlates of bias
# in the relative biomass statistic
# Compares models to identify most parsimonious model 
#
# Note: This saves outputs to folder Outputs/Brel/
# So make sure you create that folder locally (its in the gitignore so isn't
# on github). 

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)


dat <- read.csv("Outputs/status-validation/2022-11-11_glm-data-validation.csv")
load("Outputs/status-validation/2022-11-11_processesed-assessment-data-validation.rda")
datcovar <- read.csv("Data/glm-covariates-Hadley.csv")
stock_groups <- read.csv("Data/stock_groups.csv")
regions <- read.csv("Data/regions.csv")
theme_set(theme_classic())



dat2 <- 
  #Join  covariates
  inner_join(dat, datcovar) %>%
  left_join(stock_groups) %>%
  #Join MRA to get Brel_MRA
  left_join(select(dat_MRA, stocklong, Brel_MRA, SSB_MRA, tsyear,
                   Year_MRA = finish2)) %>%
  mutate(stock_value = log(SSB_MRA * dollar_per_tonne/1000000), 
         lnBrel_MRA = log(Brel_MRA),
         start.year = tsyear -start.diff,
         trend.50yr.coef.cap = ifelse(HADISSTtrend.50yr.coef>0.05, 0.05,
                                      HADISSTtrend.50yr.coef)*100,
         clupeids = factor(ifelse(Fishery.group == "Herrings, sardines, anchovies",
                                  "Clupeid", "Other"))) %>%
  rename(Delta_Brel = d.B.B0,
         Delta_B = d.B..ln.B.B.recent.,
         Delta_B1 = d.B0..ln.B0.B0.recent.,
         fishery_group = Fishery.group)
dat2$clupeids <- relevel(dat2$clupeids, ref = "Other")

nrow(dat2)

#Write data for supplemental file 
write.csv(dat2, "Outputs/status-validation/glm-covariates-validation.csv",
          row.names = FALSE)

response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

ivar <- response_vars[1]
  


#
# GLMM Delta_Brel
#
 

form4 <- paste(ivar, " ~
                 (stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 HADISSTmean.5yr +
                 clupeids + 
                (1|stocklong)")

m1_validate <- brm(form4,
             data = dat2,
             chains = 4,
             iter = 4000)

 save(m1_validate, file = "Outputs/status-validation/model-fit-validation.rda")

#
#Checks 
#
# plot(m1)
qqnorm(resid(m1_validate))
qqline(resid(m1_validate))


#
#Table of params
#
sm1 <- summary(m1_validate)
sm1effects <- rbind(sm1$fixed, sm1$random$Group,
                    sm1$random$stocklong) %>%
  signif(2) %>%
  data.frame() %>%
  tibble::rownames_to_column("Parameter")
sm1effects$Parameter[11] <- "SD Stock"
write.csv(sm1effects,
         "Outputs/status-validation/effects-validation.csv")

#
# Predictions vs residuals 
#

m1residpred <- cbind(resid(m1_validate), predict(m1_validate), dat2) %>% data.frame()

g1 <- ggplot(m1residpred) +
  aes(x = Delta_Brel, y = Estimate.1) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Q2.5.1, ymax = Q97.5.1),
                 alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1) + 
  xlab("Observed") + 
  ylab("Predicted")
  
#
# Random effect predictions 
#
#
# Summaries by stocks 
#

dat_MRA_MRY <- filter(dat_MRA, tsyear == finish2) %>%
  select(stocklong, Brel_MRA)
dat_MRA_years <- dat_MRA %>% group_by(stocklong) %>%
  summarize(Year = min(tsyear),
            max_year_MRA = max(tsyear)) %>%
  mutate(Years = max_year_MRA - Year)

dat_by_stock <- dat2 %>% group_by(stocklong,
                                  country) %>%
  left_join(regions) %>% # add region names 
  # Summaries for supp table 1
  summarize(Region = Region[1],
            N = n(),
            mean(Delta_Brel),
            mean(Delta_B),
            mean(Delta_B1),
            sd(Delta_Brel),
            sd(Delta_B),
            sd(Delta_B1)) %>%
  left_join(dat_MRA_MRY) %>% # add B/B1 for most recent year of the MRA
  left_join(dat_MRA_years) %>% # add year and years variables
  arrange(country)

datr <- ranef(m1_validate)$stocklong %>% data.frame() %>%
  tibble::rownames_to_column("stocklong") %>%
  left_join(dat_by_stock)

g2 <- ggplot(datr) +
  aes(x = `mean(Delta_Brel)`, y = Estimate.Intercept) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Q2.5.Intercept, ymax = Q97.5.Intercept),
                alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Observed") + 
  ylab("Predicted")

library(patchwork)
gall <- (g1 / g2) +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides='collect') 
gall
