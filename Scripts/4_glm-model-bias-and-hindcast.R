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

dat <- read.csv("Outputs/2022-02-11_glm-data.csv")
load("Outputs/2022-02-11_processesed-assessment-data.rda")
datcovar <- read.csv("Data/stock-covariates.csv")
theme_set(theme_classic())

dat2 <- 
  #Join  covariates
  inner_join(dat, datcovar) %>%
  #Join MRA to get Brel_MRA
  left_join(select(dat_MRA, stocklong, Brel_MRA, SSB_MRA, tsyear, Year_MRA = finish2)) %>%
  mutate(stock_value = log(SSB_MRA * dollar_per_tonne/1000000), 
         lnBrel_MRA = log(Brel_MRA),
         Delta_Brel = d.B.B0,
         Delta_B = d.B..ln.B.B.recent.,
         Delta_B1 = d.B0..ln.B0.B0.recent.,
         start.year = tsyear -start.diff,
         trend.50yr.coef.cap = ifelse(trend.50yr.coef>0.05, 0.05,
                                      trend.50yr.coef)*100)
nrow(dat2)

#Write data for supplemental file 
 write.csv(dat2, "Outputs/glm-covariates-merged.csv",
          row.names = FALSE)

response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

ivar <- response_vars[1]
  
#
# GLMM Delta_Brel
#
 

#Specify model formulas
form1 <- paste(ivar, " ~
                (start.diff +
                mean.5yr + trend.50yr.coef.cap +
                 stock_value +
                 year.diff)*lnBrel_MRA +
                (1|stocklong)")
form2 <- paste(ivar, " ~
                (mean.5yr + trend.50yr.coef.cap +
                 stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff+
                (1|stocklong)")
form3 <- paste(ivar, " ~
                (mean.5yr  +
                 stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap+
                (1|stocklong)")
form4 <- paste(ivar, " ~
                 (stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 mean.5yr +
                (1|stocklong)")
form5 <- paste(ivar, " ~
                 year.diff*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 mean.5yr +
                 stock_value +
                (1|stocklong)")
form6 <- paste(ivar, " ~
                 year.diff+lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 mean.5yr +
                 stock_value +
                (1|stocklong)")


#Function to run model given a formula
fitmods <- function(form){
  m1 <- brm(as.formula(form),
              data = dat2,
            chains = 4,
            iter = 4000)
  m1 <- add_criterion(m1, "waic")
  return(m1)
}

# Run all the models
forms <- list(form1, form2, form3, 
              form4, form5, form6)
mout <- lapply(forms, fitmods)

# smod <- summary(mint)
# sort(abs(smod$fixed[,1]/smod$fixed[,2]))

getwaic <- function(x){
  y <- tibble::rownames_to_column(data.frame(waic(x)$estimate),
                              "x")
  y$formula <- as.character(x$formula)[1]
  y
}
xout <- lapply(mout, getwaic) %>%
  do.call("rbind", .) %>%
  arrange(Estimate) %>%
  arrange(x)

write.csv(xout, paste0("Outputs/",ivar,"/model-waic.csv"))
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
  sm1effects$Parameter[8] <- "SD Group"
  sm1effects$Parameter[9] <- "SD Stock"
  sm_all <- c(sm_all, sm1effects)
}
sm_all <- do.call("rbind", sm_all)
write.csv(sm_all,
          paste0("Outputs/",ivar,"/_all_models_effects.csv"))

#save model file to look at later
# note this model is re-run in script 6
m1 <- mout[[5]]
 save(m1, file = paste0("Outputs/",ivar,"/model-fit.rda"))
# load(file = paste0("Outputs/",ivar,"/model-fit.rda"))
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
sm1effects$Parameter[8] <- "SD Group"
sm1effects$Parameter[9] <- "SD Stock"
write.csv(sm1effects,
          paste0("Outputs/",ivar,"/effects.csv"))


# ------------ 
# Select plots 
# ------------ 

#
#Plots created here for exploratory purposes. 
# Plots used in the paper are re-created in 
# 6_glm-automated-by-variable.R

#
# Fixed effects 
#

fixef <- fixef(m1) %>% 
  data.frame() %>%
  tibble::rownames_to_column() %>%
  mutate(signif = sign(Q2.5) == sign(Q97.5)) %>%
  filter(rowname != "Intercept") %>%
  mutate(params = fct_recode(factor(rowname),
                              "Duration" = "start.diff",
                              "Mean SST" = "mean.5yr",
                              "Value" = "stock_value",
                              "SST trend" = "trend.50yr.coef.cap",
                              "Obsolescence" = "year.diff",
                              "Depletion" = "lnBrel_MRA",
                              "Depletion by \n duration" = "year.diff:lnBrel_MRA"
                             ))

g1 <-
  ggplot(fixef) +
  aes(x = params, y = Estimate, color = signif) + 
  geom_hline(yintercept= 0) + 
  geom_point() + 
  # ylim(-0.1, 0.1) +
  xlab("") + 
  geom_linerange(aes(ymin = Q2.5,
                     ymax = Q97.5)) + 
  coord_flip() + 
  scale_color_manual(values = c("grey", "red")) + 
  theme(legend.position = "none")

ggsave(g1, file = paste0("Outputs/",ivar,"/fixed-effects.png"))

#
# predicted effects
#

g1 <- conditional_effects(m1, effect = "year.diff",
                    conditions = 
                      data.frame(lnBrel_MRA = c(log(0.1),
                                                log(0.5),
                                                log(0.9))),
                    plot = FALSE)
g1 <- plot(g1)[[1]]
ggsave(g1, file = paste0("Outputs/",ivar,"/Brel_yeardiff.png"))

g1 <- conditional_effects(m1, effect = "start.diff")
g1 <- plot(g1)[[1]]
ggsave(g1, file = paste0("Outputs/",ivar,"/start.diff.png"))

g1 <- conditional_effects(m1, effect = "mean.5yr")
g1 <- plot(g1)[[1]]
ggsave(g1, file = paste0("Outputs/",ivar,"/year_sst_mean.png"))


#
# Calculate and plot GLMS for years since MRA with sustainable and overfished data as separate curves 
# (using criterion B/B1 MRA <0.5). 
#


newdata <- with(dat2, expand.grid(
  lnBrel_MRA = c(log(0.1), log(0.9)),
  year.diff = seq(min(year.diff), max(year.diff), by = 1),
  start.diff = mean(start.diff),
  mean.5yr = mean(mean.5yr),
  trend.50yr.coef.cap = mean(trend.50yr.coef.cap),
  stock_value = mean(stock_value),
  stocklong = NA,
  Group = NA
))

pdat <- posterior_epred(m1, newdata = newdata,
                        re.form = NA) %>%
  apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
  t() %>%
  cbind(newdata)

pdat$status <- exp(pdat$lnBrel_MRA)
g1 <- ggplot(pdat) + 
  aes(x = year.diff, y = `50%`, fill = factor(status),
      group = status)+
  geom_line() +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`), 
              color = NA, alpha = 0.5)+
  ylab("Delta B/B1") +
  xlab("Years to MRA") +
  scale_fill_manual("B/B1", values = c("red", "darkblue"))
ggsave(g1, file =paste0("Outputs/",ivar,"/years-to-MRA-Brelative.png"))

