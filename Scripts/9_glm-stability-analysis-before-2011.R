# GLM of bias for B/B0 ("Brel")
#
# CJ Brown 2023-02-13
#
# Fits same GLM as full analysis, but 
# only using assessments 2010 and before.

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)

dat <- read.csv("Outputs/2023-02-13_glm-data-before-2011.csv")
load("Outputs/2023-02-13_processesed-assessment-data-before-2011.rda")
datcovar <- read.csv("Data/glm-covariates-Hadley.csv")
stock_groups <- read.csv("Data/stock_groups.csv")
regions <- read.csv("Data/regions.csv")
theme_set(theme_classic())

#Use -6 years for these two as missing 2004 year of data
six_years <- c("Deepwater_Flathead SE Australia", "Tiger_flathead SE Australia")

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

#
# GLMM Delta_Brel
#
ivar <- "Delta_Brel"
form4 <- paste(ivar, " ~
                 (stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 HADISSTmean.5yr +
                 clupeoids + 
                (1|stocklong)")

m1 <- brm(as.formula(form4),
          data = dat2,
          chains = 4,
          iter = 6000)
#
#Checks 
#
qqnorm(resid(m1))
qqline(resid(m1))


#
#Table of params for all models 
#

sm1 <- summary(m1)
sm1effects <- rbind(sm1$fixed, sm1$random$Group,
                    sm1$random$stocklong) %>%
  signif(2) %>%
  data.frame() %>%
  tibble::rownames_to_column("Parameter")
sm1effects$Parameter[sm1effects$Parameter == "sd(Intercept)"]  <- "SD Stock"

write.csv(sm1effects,
          paste0("Outputs/",ivar,"/models_effects-before-2011.csv"))


conditional_effects(m1, effect = "year.diff",
                           conditions = 
                             data.frame(lnBrel_MRA = c(log(0.1),
                                                       log(0.4),
                                                       log(1))),
                           plot = FALSE)

fixef <- fixef(m1) %>% 
  data.frame() %>%
  tibble::rownames_to_column() %>%
  mutate(signif = sign(Q2.5) == sign(Q97.5)) %>%
  filter(rowname != "Intercept")  %>%
  mutate(params = fct_recode(factor(rowname),
                             "Duration" = "start.diff",
                             "Mean SST" = "HADISSTmean.5yr",
                             "SST trend" = "trend.50yr.coef.cap",
                             "Clupeoid" = "clupeoidsClupeid",
                             "Value" = "stock_value",
                             "Obsolescence" = "year.diff",
                             "Depletion" = "lnBrel_MRA",
                             "Value by \n obsolescence" = "stock_value:lnBrel_MRA",
                             "Depletion by \n obsolescence" = "year.diff:lnBrel_MRA"
  ))%>%
  mutate(params = factor(params, levels = c(
    "Duration",
    "Mean SST",
    "SST trend",
    "Clupeoid",
    "Value",
    "Obsolescence",
    "Depletion",
    "Value by \n obsolescence",
    "Depletion by \n obsolescence"
  )))

g1 <-
  ggplot(fixef) +
  aes(x = params, y = Estimate, color = signif) + 
  geom_hline(yintercept= 0) + 
  geom_point(size = 2.3) + 
  ylim(-0.6, 0.32) +
  xlab("") + 
  geom_linerange(aes(ymin = Q2.5,
                     ymax = Q97.5), 
                 size = 0.8) + 
  coord_flip() + 
  scale_color_manual(values = c("black", "#d41515")) + 
  theme(legend.position = "none")

ggsave(g1, file = paste0("Outputs/",ivar,"/fixed-effects-before-2011-inc-clupeids.png"))

g1_2ndMRA <- g1
save(fixef, g1_2ndMRA, file = "Outputs/2023-03-10_plots-stability-model.rda")


#
# Response conditional on B/B1 and Years to MRA
#

newdata <- with(m1$data, expand.grid(
  lnBrel_MRA = c(log(0.1), log(0.4), log(1)),
  year.diff = seq(min(year.diff), max(year.diff), by = 1),
  start.diff = mean(start.diff),
  HADISSTmean.5yr = mean(HADISSTmean.5yr),
  trend.50yr.coef.cap = mean(trend.50yr.coef.cap),
  stock_value = mean(stock_value),
  clupeoids = "Other",
  stocklong = NA,
  Group = NA
))

pdat <- posterior_epred(m1, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat$status <- exp(pdat$lnBrel_MRA)

#
#Figure 4 in the paper
#
g1 <- ggplot(pdat) + 
  aes(x = year.diff, y = exp(`50%`), fill = factor(status),
      group = status)+
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7)+
  ylab(expression(Delta*'B/B'[1])) +
  xlab("Obsolescence (yrs)") +
  xlim(0, 15) + 
  scale_y_continuous(breaks = seq(0.5, 6, by = 0.5),
                     labels = seq(0.5, 6, by = 0.5),
                     limits = c(0.5, 6)) +
  scale_fill_manual(expression('B/B'[1]), values = c("#d41515", "black", "#0537ab"))

ggsave("Outputs/Obsolesence-value-deltas-before-2011.png",
       g1,
       width = 5, height =3)
