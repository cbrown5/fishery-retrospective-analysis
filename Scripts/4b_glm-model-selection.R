# GLM of bias for B/B0 ("Brel")
#
# CJ Brown 2023-03-21
#
#4b: Model with year of MRA as a covariate

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)

dat <- read.csv("Outputs/2024-01-10glm-data-Bmax.csv")
load("Outputs/2024-01-10_processesed-assessment-data-Bmax.rda")

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
         clupeoids = factor(ifelse(Fishery.group == "Herrings, sardines, anchovies",
                           "Clupeid", "Other"))) %>%
  rename(Delta_Brel = d.B.B0,
         Delta_B = d.B..ln.B.B.recent.,
         Delta_B1 = d.B0..ln.B0.B0.recent.,
         fishery_group = Fishery.group)
nrow(dat2)
dat2$clupeoids <- relevel(dat2$clupeoids, ref = "Other")


#Write data for supplemental file 
 # write.csv(dat2, "Outputs/glm-covariates-merged.csv",
          # row.names = FALSE)

response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

ivar <- response_vars[1]

#
# Plot of MRA year vs. 
#
  
pairs(select(dat2, start.diff, HADISSTmean.5yr,
             trend.50yr.coef.cap,
             stock_value,
             year.diff,Year_MRA,
             lnBrel_MRA))

corrplot::corrplot(cor(select(dat2, start.diff, HADISSTmean.5yr,
             trend.50yr.coef.cap,
             stock_value,
             year.diff,
             lnBrel_MRA, Year_MRA)))

g1 <- dat2 %>%
  select(stocklong, Year_MRA, lnBrel_MRA) %>%
  distinct() %>%
  mutate(status = ifelse(exp(lnBrel_MRA)<0.4, 
                         "Depleted","Sustainable")) %>%
  ggplot() +
  aes(x = Year_MRA, y = lnBrel_MRA) + 
  geom_point() + 
  facet_grid(.~status) +
  xlab("Year of MRA") + 
  ylab("Depletion level \n (natural log)") +
  stat_smooth(se = FALSE)

ggsave("Outputs/year-mra-vs-depletion.png", g1,
       width = 5, height = 4)

#
# GLMM Delta_Brel
#

#scale covariates by 1 SD so we can use standard coef. This is
# is more efficient for sampling than using a differnet prior for
# each coef

dat_sd <- dat2 %>% select(start.diff, HADISSTmean.5yr,
       trend.50yr.coef.cap,
       stock_value,
       year.diff,Year_MRA,
       lnBrel_MRA) %>%
  summarize(across(start.diff:lnBrel_MRA,sd))
  

dat3 <- dat2 %>%
  mutate(start.diff = start.diff/dat_sd$start.diff,
         HADISSTmean.5yr = HADISSTmean.5yr/dat_sd$HADISSTmean.5yr,
         trend.50yr.coef.cap = trend.50yr.coef.cap/dat_sd$trend.50yr.coef.cap,
         stock_value = stock_value/dat_sd$stock_value,
         year.diff = year.diff/dat_sd$year.diff,
         Year_MRA = Year_MRA/dat_sd$Year_MRA,
         lnBrel_MRA = lnBrel_MRA/dat_sd$lnBrel_MRA
         )

prior1 <- prior(normal(0, 2), class = b)

m1 <- brm(Delta_Brel ~ (Year_MRA + 
                          stock_value +
                          year.diff)*lnBrel_MRA +
            start.diff +
            HADISSTmean.5yr + 
            trend.50yr.coef.cap +
            clupeoids + 
            (1|stocklong),
          data = dat3,
          chains = 4,
          cores = 4,
          iter = 4000,
          prior = prior1)

summary(m1)

#
# B model 
#


m2 <- brm(Delta_B ~ (Year_MRA + 
                          stock_value +
                          year.diff)*lnBrel_MRA +
            start.diff +
            HADISSTmean.5yr + 
            trend.50yr.coef.cap +
            clupeoids + 
            (1|stocklong),
          data = dat3,
          chains = 4,
          cores = 4,
          iter = 4000,
          prior = prior1)

summary(m2)

#
# B1 model 
#


m3 <- brm(Delta_B1 ~ (Year_MRA + 
                       stock_value +
                       year.diff)*lnBrel_MRA +
            start.diff +
            HADISSTmean.5yr + 
            trend.50yr.coef.cap +
            clupeoids + 
            (1|stocklong),
          data = dat3,
          chains = 4,
          cores = 4,
          iter = 4000,
          prior = prior1)

summary(m3)
#
# Response conditional on B/B1 and Years to MRA
#

newdata <- with(dat3, expand.grid(
  #need to put back onto scale of model fitting (ie divide by 1 SD)
  lnBrel_MRA = c(log(0.1)/dat_sd$lnBrel_MRA, 
                 log(0.4)/dat_sd$lnBrel_MRA, 
                 log(1)/dat_sd$lnBrel_MRA),
  Year_MRA = seq(min(Year_MRA), max(Year_MRA), length.out = 50),
  year.diff = mean(year.diff),
  start.diff = mean(start.diff),
  HADISSTmean.5yr = mean(HADISSTmean.5yr),
  trend.50yr.coef.cap = mean(trend.50yr.coef.cap),
  stock_value = mean(stock_value),
  clupeoids = "Other",
  stocklong = NA,
  Group = NA
))

#put year_mra back onto real scale
newdata$Year_MRA_unscaled <- 
  newdata$Year_MRA * dat_sd$Year_MRA

pdat1 <- posterior_epred(m1, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat2 <- posterior_epred(m2, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat3 <- posterior_epred(m3, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)
#put status back onto real scale
pdat1$status <- exp(pdat1$lnBrel_MRA*dat_sd$lnBrel_MRA)
pdat2$status <- exp(pdat2$lnBrel_MRA*dat_sd$lnBrel_MRA)
pdat3$status <- exp(pdat3$lnBrel_MRA*dat_sd$lnBrel_MRA)

#
#Figure of predicted year MRA effect
#

g1 <- ggplot(pdat1) + 
  aes(x = Year_MRA_unscaled, y = exp(`50%`), fill = factor(status),
      group = status) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7) +
  ylab(expression(Delta*'B/B'[max])) +
  xlab("Year of MRA") +
  # xlim(0, 15) + 
  # scale_y_continuous(breaks = seq(0.5, 3, by = 0.5),
                     # labels = seq(0.5, 3, by = 0.5),
                     # limits = c(0.5, 3.2)) +
  scale_fill_manual(expression('B/B'[max]), values = c("#d41515", "black", "#0537ab"))


g2 <- ggplot(pdat2) + 
  aes(x = Year_MRA_unscaled, y = exp(`50%`), fill = factor(status),
      group = status) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7) +
  ylab(expression(Delta*'B')) +
  xlab("Year of MRA") +
  # xlim(0, 15) + 
  # scale_y_continuous(breaks = seq(0.5, 3, by = 0.5),
  # labels = seq(0.5, 3, by = 0.5),
  # limits = c(0.5, 3.2)) +
  scale_fill_manual(expression('B/B'[max]), values = c("#d41515", "black", "#0537ab"))

g3 <- ggplot(pdat3) + 
  aes(x = Year_MRA_unscaled, y = exp(`50%`), fill = factor(status),
      group = status) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7) +
  ylab(expression(Delta*'B'[max])) +
  xlab("Year of MRA") +
  # xlim(0, 15) + 
  # scale_y_continuous(breaks = seq(0.5, 3, by = 0.5),
  # labels = seq(0.5, 3, by = 0.5),
  # limits = c(0.5, 3.2)) +
  scale_fill_manual(expression('B/B'[max]), values = c("#d41515", "black", "#0537ab"))

gall <- (g1 + g2 + g3)+
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides='collect') 
gall

ggsave(gall, file =paste0("Outputs/years-of-MRA-Brelative.png"),
       width = 8, height =3)

save(pdat1, pdat2, pdat3, file = "Outputs/2024-01-30_predicitions-with-year-MRA.rda")
