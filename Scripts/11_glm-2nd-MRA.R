# GLM of bias for B/B0 ("Brel")
#
# CJ Brown 2023-02-14
#
#GLM filtering for second MRA (highest year)

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)
library(patchwork)

runmodels <- FALSE #set to true to rerun models,
# set to false to load files
saveplots <- FALSE

dat2 <- read.csv("Outputs/glm-covariates-merged.csv")
dat2$clupeids <- relevel(factor(dat2$clupeids), ref = "Other")

#
# Filter for second MRA 
#

dat3 <- dat2 %>% 
  group_by(stocklong) %>%
  mutate(maxyear = tsyear == max(tsyear)) %>%
  filter(maxyear)

#
# Setup 
#

theme_set(theme_classic())

#Set response variables to run the best model for 
response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

response_names <- c(expression(Delta*'B/B'[1]),
                    expression(Delta*'B'),
                    expression(Delta*'B'[1])
)

#
# Run model 
#

ivar <- response_vars[1]

form1 <- paste(ivar, " ~
                 (tsyear +
                 year.diff)*lnBrel_MRA")

if (runmodels){
  m1 <- brm(as.formula(form1),
            data = dat3,
            iter = 6000,
            thin = 3)
  
  save(m1, file = paste0("Outputs/",ivar,"/tsyear-model.rda"))
} else {
  load(file = paste0("Outputs/",ivar,"/tsyear-model.rda"))
}

#
#Checks 
#

plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))

#
#Table of params
#
sm1 <- summary(m1)
sm1effects <- rbind(sm1$fixed,
                    sm1$random$stocklong) %>%
  signif(2) %>%
  data.frame() %>%
  tibble::rownames_to_column("Parameter")

###
# sm1effects$Parameter[11] <- "SD Stock"
write.csv(sm1effects,
          paste0("Outputs/",ivar,"/effects-tsyear-model.csv"))


#
# Fixed effects 
#

fixef <- fixef(m1) %>% 
  data.frame() %>%
  tibble::rownames_to_column() %>%
  mutate(signif = sign(Q2.5) == sign(Q97.5)) %>%
  filter(rowname != "Intercept")  %>%
  mutate(params = fct_recode(factor(rowname),
                             "Year" = "tsyear",
                             "Obsolescence" = "year.diff",
                             "Depletion" = "lnBrel_MRA",
                             "Year by \n obsolescence" = "tsyear:lnBrel_MRA",
                             "Depletion by \n obsolescence" = "year.diff:lnBrel_MRA"
  ))%>%
  mutate(params = factor(params, levels = c(
    "Year",
    "Obsolescence",
    "Depletion",
    "Year by \n obsolescence",
    "Depletion by \n obsolescence"
  )))

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

g1_2ndMRA <- g1
save(fixef, g1_2ndMRA, file = "Outputs/2023-03-10_plots-2ndMRA-model.rda")

if(saveplots)
  ggsave(g1, file = paste0("Outputs/",ivar,"/fixed-effects-tsyear-model.png"))

#
# Response conditional on B/B1 and Years to MRA
#

newdata <- with(dat2, expand.grid(
  lnBrel_MRA = c(log(0.1), log(0.4), log(1)),
  year.diff = seq(min(year.diff), max(year.diff), by = 1),
  tsyear = mean(tsyear)
))

pdat <- posterior_epred(m1, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat$status <- exp(pdat$lnBrel_MRA)

#
# Supp figure of tsyear and obs effect
#

g1_obs <- ggplot(pdat) + 
  aes(x = year.diff, y = exp(`50%`), fill = factor(status),
      group = status)+
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7)+
  ylab(response_names[ivar == response_vars]) +
  xlab("Obsolescence (yrs)") +
  xlim(0, 15) + 
  scale_y_continuous(breaks = seq(0.5, 5, by = 0.5),
                     labels = seq(0.5, 5, by = 0.5),
                     limits = c(0.5, 5)) +
  # scale_y_log10(breaks = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
  # labels = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
  # scale_y_log10(breaks = 10^(seq(-1, 1, by = 0.5)),
  # labels = 10^(seq(-1, 1, by = 0.5)),
  # limits = c(0.5, 4)) +
  scale_fill_manual(expression('B/B'[1]), values = c("#d41515", "black", "#0537ab"))

#
# Response conditional on B/B1 and tsyear
#

newdata <- with(dat2, expand.grid(
  lnBrel_MRA = c(log(0.1), log(0.4), log(1)),
  year.diff = mean(year.diff),
  tsyear = seq(min(tsyear), max(tsyear), length.out = 100)
))

pdat <- posterior_epred(m1, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat$status <- exp(pdat$lnBrel_MRA)

g1_tsyear <- ggplot(pdat) + 
  aes(x = tsyear, y = exp(`50%`), fill = factor(status),
      group = status)+
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_ribbon(aes(ymin = exp(`2.5%`),
                  ymax = exp(`97.5%`)), 
              color = NA, alpha = 0.7)+
  scale_y_continuous(breaks = seq(0.5, 4, by = 0.5),
                     labels = seq(0.5, 4, by = 0.5),
                     limits = c(0.5, 2.5)) +
  # scale_y_log10(breaks = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
  # labels = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
  # scale_y_log10(breaks = 10^(seq(-1, 1, by = 0.5)),
  # labels = 10^(seq(-1, 1, by = 0.5)),
  # limits = c(0.5, 4)) +
  scale_fill_manual(expression('B/B'[1]), values = c("#d41515", "black", "#0537ab"))+
  ylab(response_names[ivar == response_vars]) +
  xlab("Value")


#
# Make multipanel figures
#

gall <- g1_obs + g1_tsyear +
  plot_annotation(tag_levels ="A") + 
  plot_layout(guides='collect') 

ggsave("Outputs/Obsolesence-tsyear-deltas-same-scale.png",
       gall,
       width = 8, height =3)

