# GLM of bias for B/B0 ("Brel")
#
# CJ Brown 2024-02-02
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

dat <- read.csv("Outputs/2024-01-10glm-data-Bmax.csv")
load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

# dat <- read.csv("Outputs/2024-01-10glm-data-B1.csv")
# load("Outputs/2024-01-10_processesed-assessment-data-B1.rda")

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
         start.year = tsyear - start.diff,
         trend.50yr.coef.cap = ifelse(HADISSTtrend.50yr.coef>0.05, 0.05,
                                      HADISSTtrend.50yr.coef)*100,
         clupeoid = factor(ifelse(Fishery.group == "Herrings, sardines, anchovies",
                           "Clupeid", "Other"))) %>%
  rename(Delta_Brel = d.B.B0,
         Delta_B = d.B..ln.B.B.recent.,
         Delta_B1 = d.B0..ln.B0.B0.recent.,
         fishery_group = Fishery.group)
nrow(dat2)
dat2$clupeoid <- relevel(dat2$clupeoid, ref = "Other")

#Write data for supplemental file 
 write.csv(dat2, "Outputs/glm-covariates-merged-Bmax.csv",
          row.names = FALSE)

response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

ivar <- response_vars[1]


#
# Summaries by stocks 
#

#number of old assessments with more than 1.5 bias
sum(exp(dat2$Delta_Brel)>1.5)/nrow(dat2)
sum(exp(dat2$Delta_Brel)>2)/nrow(dat2)
#% of stocks within 5%
sum((exp(dat2$Delta_Brel)  < 1.05) & (exp(dat2$Delta_Brel)  > 0.95))/nrow(dat2)
sum((exp(dat2$Delta_Brel)  < 1.32) & (exp(dat2$Delta_Brel)  > 0.9))/nrow(dat2)

dat_MRA_MRY <- filter(dat_MRA, tsyear == finish2) %>%
  select(stocklong, Brel_MRA)
dat_MRA_years <- dat_MRA %>% group_by(stocklong) %>%
  summarize(Year = min(tsyear),
            max_year_MRA = max(tsyear)) %>%
  mutate(Years = max_year_MRA - Year)

dat_by_stock <- dat2 %>% group_by(stocklong,
                                  country, Common.name) %>%
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

#Save table for table S1, reorder columns
datoutput <- dat_by_stock[,c("Common.name", "Region",
                             "country", "Year",
                             "Years", "N",
                             "Brel_MRA",
                             "mean(Delta_Brel)",
                             "mean(Delta_B)",
                             "mean(Delta_B1)",
                             "sd(Delta_Brel)",
                             "sd(Delta_B)",
                             "sd(Delta_B1)")]
 # write.csv(datoutput,
 # "Outputs/stock-summaries2.csv")

sum(dat_by_stock$`mean(Delta_Brel)` > 0)
sum(dat_by_stock$`mean(Delta_Brel)` < 0)

sum(exp(dat_by_stock$`mean(Delta_Brel)`)  > 1.05)
sum(exp(dat_by_stock$`mean(Delta_Brel)`)  < 0.95)

sum(exp(dat_by_stock$`mean(Delta_Brel)`)  > 2)

exp(mean((dat_by_stock$`mean(Delta_Brel)`)))
exp(sd((dat_by_stock$`mean(Delta_Brel)`))/sqrt(230))

exp(mean(dat_by_stock$`mean(Delta_B)`))
exp(sd((dat_by_stock$`mean(Delta_B)`))/sqrt(230))
exp(mean(dat_by_stock$`mean(Delta_B1)`))
exp(sd((dat_by_stock$`mean(Delta_B1)`))/sqrt(230))

#
# Summary stats excluding stocks where B1 year differs across
# assessments
#

stock_diff_years <- dat2 %>% group_by(stocklong) %>%
  summarize(minsy  = min(minyear),
            maxsy = max(minyear),
            diff = maxsy - minsy) %>%
  filter(diff==0) 
100*(1-nrow(stock_diff_years)/nrow(dat_by_stock))
dat_by_stock2 <- inner_join(dat_by_stock, stock_diff_years)
1-nrow(dat_by_stock2)/230

nrow(dat_by_stock2)
sum(dat_by_stock2$`mean(Delta_Brel)` > 0)
sum(dat_by_stock2$`mean(Delta_Brel)` < 0)
sum(exp(dat_by_stock2$`mean(Delta_Brel)`)  > 1.05)
sum(exp(dat_by_stock2$`mean(Delta_Brel)`)  < 0.95)
sum(exp(dat_by_stock2$`mean(Delta_Brel)`)  > 1.5)
sum(exp(dat_by_stock2$`mean(Delta_Brel)`)  > 2)

exp(mean((dat_by_stock2$`mean(Delta_Brel)`)))
exp(sd((dat_by_stock2$`mean(Delta_Brel)`))/sqrt(230))
exp(mean(dat_by_stock2$`mean(Delta_B)`))
exp(sd((dat_by_stock2$`mean(Delta_B)`))/sqrt(230))
1-exp(mean(dat_by_stock2$`mean(Delta_B1)`))
exp(sd((dat_by_stock2$`mean(Delta_B1)`))/sqrt(230))

#
# GLMM Delta_Brel
#
 

#Specify model formulas
form1 <- paste(ivar, " ~
                (start.diff +
                HADISSTmean.5yr + trend.50yr.coef.cap +
                 stock_value +
                 year.diff)*lnBrel_MRA +
                 clupeoid + 
                (1|stocklong)")
form2 <- paste(ivar, " ~
                (HADISSTmean.5yr + trend.50yr.coef.cap +
                 stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff+
                 clupeoid + 
                (1|stocklong)")
form3 <- paste(ivar, " ~
                (HADISSTmean.5yr  +
                 stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff +
                 clupeoid + 
                 trend.50yr.coef.cap+
                (1|stocklong)")
form4 <- paste(ivar, " ~
                 (stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 HADISSTmean.5yr +
                 clupeoid + 
                (1|stocklong)")
form5 <- paste(ivar, " ~
                 year.diff*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 HADISSTmean.5yr +
                 stock_value +
                 clupeoid + 
                (1|stocklong)")
form6 <- paste(ivar, " ~
                 year.diff+lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 HADISSTmean.5yr +
                 stock_value +
                 clupeoid + 
                (1|stocklong)")

#Non-linear model, to test
formNL <- paste(ivar, " ~
                 t2(year.diff, lnBrel_MRA, k = 5) +
                 start.diff +
                 trend.50yr.coef.cap +
                 HADISSTmean.5yr +
                 stock_value+
                 clupeoid +
                (1|stocklong)")
#  

#Function to run model given a formula
fitmods <- function(form){
  m1 <- brm(as.formula(form),
              data = dat2,
            chains = 4,
            iter = 6000)
  m1 <- add_criterion(m1, "waic")
  return(m1)
}

# Run all the models
forms <- list(form1, form2, form3, 
              form4, form5, form6)

mout <- lapply(forms, fitmods)

NLmod <- brm(as.formula(formNL),
             data = dat2,
             chains = 4,
             cores = 4,
             iter = 4000,
             #has trouble converging 
             # with NL model, so increase adapt_delta
             control = list(adapt_delta = 0.9))
save(NLmod, file = "Outputs/non-linear-model.rda")
# smod <- summary(mint)
# sort(abs(smod$fixed[,1]/smod$fixed[,2]))
mout2 <- c(mout, list(NLmod))
getwaic <- function(x){
  y <- tibble::rownames_to_column(data.frame(waic(x)$estimate),
                              "x")
  y$formula <- as.character(x$formula)[1]
  y
}
xout <- lapply(mout2, getwaic) %>%
  do.call("rbind", .) %>%
  arrange(Estimate) %>%
  arrange(x)

write.csv(xout, paste0("Outputs/",ivar,"/model-waic2.csv"))
#
#Table of params for all models 
#
sm_all <- NULL

for (i in 1:length(mout)){
  sm1 <- summary(mout[[i]])
  sm1effects <- rbind(sm1$fixed, sm1$random$Group,
                      sm1$random$stocklong) %>%
    signif(2) %>%
    data.frame() %>%
    tibble::rownames_to_column("Parameter")
  sm1effects$Parameter[sm1effects$Parameter == "sd(Intercept)"]  <- "SD Stock"
  sm1effects$model_formula <- as.character(sm1$formula$formula)[3]
  sm1effects$model_number <- i
  sm_all <- c(sm_all, list(sm1effects))
}
sm_all <- do.call("rbind", sm_all)
write.csv(sm_all,
          paste0("Outputs/",ivar,"/all_models_effects.csv"))

#save model file to look at later
# note this model is re-run in script 6
m1 <- mout[[4]]
 save(m1, file = paste0("Outputs/",ivar,"/model-fit.rda"))
# load(file = paste0("Outputs/",ivar,"/model-fit.rda"))
 
conditional_effects(m1, effect = "year.diff",
                           conditions = 
                             data.frame(lnBrel_MRA = c(log(0.1),
                                                       log(0.4),
                                                       log(1))),
                           plot = FALSE)
#
#Checks 
#
# plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))

#
#Table of params
#
sm1 <- summary(m1)
sm1effects <- rbind(sm1$fixed, sm1$random$Group,
                    sm1$random$stocklong) %>%
  signif(2) %>%
  data.frame() %>%
  tibble::rownames_to_column("Parameter")


sm1effects$Parameter[11] <- "SD Stock"
write.csv(sm1effects,
          paste0("Outputs/",ivar,"/effects.csv"))

#
# Predictions vs residuals 
#

m1residpred <- cbind(resid(m1), predict(m1), dat2) %>% data.frame()

ggplot(m1residpred) +
  aes(x = Estimate.1, y = Estimate) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = Q2.5.1, xmax = Q97.5.1),
                alpha = 0.25) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5),
                 alpha = 0.25)

g1 <- ggplot(m1residpred) +
  aes(x = Delta_Brel, y = Estimate.1) + 
  geom_point(alpha = 0.3) + 
  geom_errorbar(aes(ymin = Q2.5.1, ymax = Q97.5.1),
                 alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1) + 
  # geom_point(data = filter(m1residpred, Delta_Brel>1.9),
  # aes(color = stocklong)) + 
  xlab("Observed") + 
  ylab("Predicted")
  
#
# Random effect predictions 
#

datr <- ranef(m1)$stocklong %>% data.frame() %>%
  tibble::rownames_to_column("stocklong") %>%
  left_join(dat_by_stock)

g2 <- ggplot(datr) +
  aes(x = `mean(Delta_Brel)`, y = Estimate.Intercept) + 
  geom_point(alpha = 0.3) + 
  geom_errorbar(aes(ymin = Q2.5.Intercept, ymax = Q97.5.Intercept),
                alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Observed") + 
  ylab("Predicted")


library(patchwork)
gall <- (g1 / g2) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") + 
  plot_layout(guides='collect') 

ggsave("Outputs/predictions-vs-obs-Brel-GLMM.png", gall,
       width = 6, height = 4)

