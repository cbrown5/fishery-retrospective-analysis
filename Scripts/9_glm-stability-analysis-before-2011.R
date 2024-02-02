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
library(tidybayes)
library(tidyr)

dat <- read.csv("Outputs/2024-01-24_glm-data-before-2011-Bmax.csv")
load("Outputs/2024-01-24_processesed-assessment-data-before-2011-Bmax.rda")
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

#
# Dataframe of SDs of each covariable 
#


SDs_of_covariables <- data.frame(
  param = c(
    "b_stock_value" ,
    "b_year.diff" ,
    "b_lnBrel_MRA" , 
    "b_start.diff", 
    "b_trend.50yr.coef.cap",
    "b_HADISSTmean.5yr",
    "b_clupeoidsClupeid",
    "b_stock_value:lnBrel_MRA",                                            
    "b_year.diff:lnBrel_MRA"
  ),
  sd = with(dat2, 
            c(
              sd(stock_value),
              sd(year.diff),
              sd(lnBrel_MRA),
              sd(start.diff),
              sd(trend.50yr.coef.cap),
              sd(HADISSTmean.5yr),
              1,
              sd(stock_value*lnBrel_MRA),
              sd(year.diff * lnBrel_MRA)
            )
  )
)


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
          paste0("Outputs/",ivar,"/models_effects-before-2011-Bmax.csv"))


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
                             "Age" = "year.diff",
                             "Depletion" = "lnBrel_MRA",
                             "Value by depletion" = "stock_value:lnBrel_MRA",
                             "Depletion by age" = "year.diff:lnBrel_MRA"
  ))%>%
  mutate(params = factor(params, levels = c(
    "Duration",
    "Mean SST",
    "SST trend",
    "Clupeoid",
    "Value",
    "Age",
    "Depletion",
    "Value by depletion",
    "Depletion by age"
  )))

#quick change of labels
# load("Outputs/2023-03-23_plots-stability-model.rda")
# fixef <- fixef %>%
#   mutate(params = factor(params, labels = c(
#     "Duration",
#     "Mean SST",
#     "SST trend",
#     "Clupeoid",
#     "Value",
#     "Survey age",
#     "Depletion",
#     "Value by \n depletion",
#     "Depletion by \n survey age"
#   )))


g1 <-
  ggplot(fixef) +
  aes(x = params, y = Estimate, color = signif) + 
  geom_hline(yintercept= 0) + 
  geom_point(size = 2.3) + 
  ylim(-0.3, 0.32) +
  xlab("") + 
  geom_linerange(aes(ymin = Q2.5,
                     ymax = Q97.5), 
                 size = 0.8) + 
  coord_flip() + 
  scale_color_manual(values = c("black", "#d41515")) + 
  theme(legend.position = "none")
g1
ggsave(g1, file = paste0("Outputs/",ivar,"/fixed-effects-before-2011-inc-clupeids-Bmax.png"))

#Point interval Fixef

x <- m1 %>%
  spread_draws(b_stock_value, b_year.diff,
               b_lnBrel_MRA, b_start.diff, 
               b_trend.50yr.coef.cap,
               b_HADISSTmean.5yr,
               b_clupeoidsClupeid,
               `b_stock_value:lnBrel_MRA`,                                            
               `b_year.diff:lnBrel_MRA`) %>%
  pivot_longer(cols = b_stock_value:`b_year.diff:lnBrel_MRA`)  %>%
  left_join(SDs_of_covariables, by = c("name" = "param")) %>%
  mutate(params = fct_recode(factor(name),
                             "Duration" = "b_start.diff",
                             "Mean SST" = "b_HADISSTmean.5yr",
                             "SST trend" = "b_trend.50yr.coef.cap",
                             "Clupeoid" = "b_clupeoidsClupeid",
                             "Value" = "b_stock_value",
                             "Age" = "b_year.diff",
                             "Depletion" = "b_lnBrel_MRA",
                             "Value by depletion" = "b_stock_value:lnBrel_MRA",
                             "Depletion by age" = "b_year.diff:lnBrel_MRA"
  ))%>%
  mutate(params = factor(params, levels = c(
    "Duration",
    "Mean SST",
    "SST trend",
    "Clupeoid",
    "Value",
    "Age",
    "Depletion",
    "Value by depletion",
    "Depletion by age"
  ))) %>%
  mutate(value_scaled = value * sd)

sig_names <- paste0("b_", fixef$rowname[sign(fixef$Q2.5) == sign(fixef$Q97.5)])
x$sig <- 'a'
x$sig[x$name %in% sig_names] <- 'b'

g1 <- x %>%
  ggplot() +
  aes(y = params, x = value_scaled, color = sig) +
  geom_vline(xintercept = 0) + 
  # stat_halfeye(normalize = "xy", fill_type = "segments", alpha = 0.8) +
  stat_pointinterval(.width = c(0.5, 0.8, 0.95),
                     interval_size_domain = c(1, 5)) +
  # scale_color_brewer(palette = 1) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("grey30", "red")) +
  # scale_color_manual(values = c("#273254", "#405427")) + 
  ylab("") + 
  xlab("Effect size (scaled)")
g1
#save figures
ggsave("Outputs/fixed-effects-posteriors-before2011-scaled.png",
       g1,
       width = 4, height =4)
# g1_2ndMRA <- g1
# save(fixef, g1_2ndMRA, file = "Outputs/2023-03-23_plots-stability-model.rda")
# 

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
  ylab(expression(Delta*'B/B'[max])) +
  xlab("Survey age (yrs)") +
  xlim(0, 15) + 
  scale_y_continuous(breaks = seq(0.5, 6, by = 0.5),
                     labels = seq(0.5, 6, by = 0.5),
                     limits = c(0.5, 6)) +
  scale_fill_manual(expression('B/B'[max]), values = c("#d41515", "black", "#0537ab"))

ggsave("Outputs/Obsolesence-value-deltas-before-2011-Bmax.png",
       g1,
       width = 4, height =3)
