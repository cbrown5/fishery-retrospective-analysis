# GLMM for each bias statistic 
# CJ Brown 
#2022-11-11


# Note: This saves outputs to folders Outputs/Brel/, Outputs/B and Outputs/B1
# So make sure you create those folders locally (Outputs is in the gitignore so isn't
# on github). 

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)
library(patchwork)

runmodels <- FALSE #set to true to rerun models,
# set to false to load files
saveplots <- TRUE

dat2 <- read.csv("Outputs/glm-covariates-merged.csv")
dat2$clupeoid <- relevel(factor(dat2$clupeoid), ref = "Other")

theme_set(theme_classic())

#Set response variables to run the best model for 
response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

response_names <- c(expression(Delta*'B/B'[1]),
                    expression(Delta*'B'),
                    expression(Delta*'B'[1])
)

gfixie <- fixef_save <- gpreds <- gpredsvalue <- NULL #lists to save key plots

# ------------ 
# Runs models for the three bias stats
# ------------ 

#Produces lots of plots to check results as a side-effect
# also produces and patchwork's plots for paper 

for (ivar in response_vars){
  # ivar <- response_vars[1]
  
  #
  # GLMM Delta_Brelm- just running the best model
  # from the bias analysis, to be consistent
  #
  
  form1 <- paste(ivar, " ~
                 (stock_value +
                 year.diff)*lnBrel_MRA +
                 start.diff +
                 trend.50yr.coef.cap +
                 HADISSTmean.5yr +
                 clupeoid + 
                (1|stocklong)")

  if (runmodels){
    m1 <- brm(as.formula(form1),
              data = dat2,
              iter = 6000,
              thin = 3)
    
    save(m1, file = paste0("Outputs/",ivar,"/best-model-fit.rda"))
  } else {
    load(file = paste0("Outputs/",ivar,"/best-model-fit.rda"))
  }
  
  #
  #Checks 
  #
  # plot(m1)
  # qqnorm(resid(m1))
  # qqline(resid(m1))
  
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
  sm1effects$Parameter[11] <- "SD Stock"
  write.csv(sm1effects,
            paste0("Outputs/",ivar,"/effects.csv"))
  
  
  #
  # Fixed effects 
  #
  
  fixef <- fixef(m1) %>% 
    data.frame() %>%
    tibble::rownames_to_column() %>%
    mutate(signif = sign(Q2.5) == sign(Q97.5)) %>%
    filter(rowname != "Intercept")  %>%
    mutate(params = fct_recode(factor(rowname),
                               "Duration" = "start.diff",
                               "Mean SST" = "HADISSTmean.5yr",
                               "SST trend" = "trend.50yr.coef.cap",
                               "Clupeoid" = "clupeoidClupeid",
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
    ylim(-0.3, 0.33) +
    xlab("") + 
    geom_linerange(aes(ymin = Q2.5,
                       ymax = Q97.5), 
                   size = 0.8) + 
    coord_flip() + 
    scale_color_manual(values = c("black", "#d41515")) + 
    theme(legend.position = "none")
  
  gfixie <- c(gfixie, list(g1))
  fixef_save <- c(fixef_save, list(fixef))
  
  if(saveplots)
    ggsave(g1, file = paste0("Outputs/",ivar,"/fixed-effects.png"))
  
  #
  # predicted effects
  #
  
  g1 <- conditional_effects(m1, effect = "year.diff",
                            conditions = 
                              data.frame(lnBrel_MRA = c(log(0.1),
                                                        log(0.4),
                                                        log(1))),
                            plot = FALSE)
  g1 <- plot(g1)[[1]]
  if(saveplots)
    ggsave(g1, file = paste0("Outputs/",ivar,"/Brel_yeardiff.png"))
  
  g1 <- conditional_effects(m1, effect = "stock_value",
                            conditions = 
                              data.frame(lnBrel_MRA = c(log(0.1),
                                                        log(0.4),
                                                        log(1))),
                            plot = FALSE)
  g1 <- plot(g1)[[1]]
  if(saveplots)
    ggsave(g1, file = paste0("Outputs/",ivar,"/Brel_value.png"))
  
  
  g1 <- conditional_effects(m1, effect = "start.diff")
  g1 <- plot(g1)[[1]]
  if(saveplots)
    ggsave(g1, file = paste0("Outputs/",ivar,"/start.diff.png"))
  
  g1 <- conditional_effects(m1, effect = "HADISSTmean.5yr")
  g1 <- plot(g1)[[1]]
  if(saveplots)
    ggsave(g1, file = paste0("Outputs/",ivar,"/year_sst_mean.png"))
  
  #
  # Response conditional on B/B1 and Years to MRA
  #
  
  newdata <- with(dat2, expand.grid(
    lnBrel_MRA = c(log(0.1), log(0.4), log(1)),
    year.diff = seq(min(year.diff), max(year.diff), by = 1),
    start.diff = mean(start.diff),
    HADISSTmean.5yr = mean(HADISSTmean.5yr),
    trend.50yr.coef.cap = mean(trend.50yr.coef.cap),
    stock_value = mean(stock_value),
    clupeoid = "Other",
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
    ylab(response_names[ivar == response_vars]) +
    xlab("Obsolescence (yrs)") +
    xlim(0, 15) + 
    scale_y_continuous(breaks = seq(0.5, 3, by = 0.5),
                       labels = seq(0.5, 3, by = 0.5),
                       limits = c(0.5, 3.2)) +
    # scale_y_log10(breaks = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
                  # labels = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
    # scale_y_log10(breaks = 10^(seq(-1, 1, by = 0.5)),
                  # labels = 10^(seq(-1, 1, by = 0.5)),
                  # limits = c(0.5, 4)) +
    scale_fill_manual(expression('B/B'[1]), values = c("#d41515", "black", "#0537ab"))
  
  
  gpreds <- c(gpreds, list(g1))
  
  if(saveplots)
    ggsave(g1, file =paste0("Outputs/",ivar,"/years-to-MRA-Brelative.png"))
  
  
  #
  # Response conditional on B/B1 and value
  #
  
  newdata <- with(dat2, expand.grid(
    lnBrel_MRA = c(log(0.1), log(0.4), log(1)),
    year.diff = mean(year.diff),
    start.diff = mean(start.diff),
    HADISSTmean.5yr = mean(HADISSTmean.5yr),
    trend.50yr.coef.cap = mean(trend.50yr.coef.cap),
    stock_value = seq(min(stock_value), max(stock_value), length.out = 100),
    clupeoid = "Other",
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
    aes(x = stock_value, y = exp(`50%`), fill = factor(status),
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
  
  gpredsvalue <- c(gpredsvalue, list(g1))
  

  if(saveplots)
    ggsave(g1, file =paste0("Outputs/",ivar,"/value-Brelative.png"))
  
  
}

#
# Make multipanel figures
#

save(gpreds, gfixie, fixef_save,
     file = "Outputs/2023-03-10_plots-main-models.rda")

gall <- gpreds[[1]] + gpreds[[2]] +
  gpreds[[3]] + 
  plot_annotation(tag_levels ="a",
                  tag_prefix = "(",
                  tag_suffix = ")") + 
  plot_layout(guides='collect') 

ggsave("Outputs/Obsolesence-deltas-same-scale.png",
       gall,
       width = 8, height =3)


gallfix <- gfixie[[1]] + 
  (gfixie[[2]] + theme(axis.text.y = element_blank())) +
  (gfixie[[3]]+ theme(axis.text.y = element_blank())) + 
  plot_annotation(tag_levels ="a",
                  tag_prefix = "(",
                  tag_suffix = ")") +   plot_layout(guides='collect') 

ggsave("Outputs/fixed-effects-deltas.png",
       gallfix,
       width = 8, height =3)

gall <- gpredsvalue[[1]] + gpredsvalue[[2]] +
  gpredsvalue[[3]] + 
  plot_annotation(tag_levels ="a",
                  tag_prefix = "(",
                  tag_suffix = ")") +   plot_layout(guides='collect') 

ggsave("Outputs/Obsolesence-value-deltas-same-scale.png",
       gall,
       width = 8, height =3)

#
# Table of all parameters
#

small <- NULL
for (ivar in response_vars){
  dtemp <- read.csv(paste0("Outputs/",ivar,"/effects.csv"))
  dtemp$response <- ivar
  small <- c(small, list(dtemp))
}

small <- do.call("rbind", small)

write.csv(small, "Outputs/effects-all-response-vars.csv")

