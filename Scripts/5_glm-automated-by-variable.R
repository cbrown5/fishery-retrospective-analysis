# GLMM for each bias statistic 
# CJ Brown 
#2024-02-02


# Note: This saves outputs to folders Outputs/Brel/, Outputs/B and Outputs/B1
# So make sure you create those folders locally (Outputs is in the gitignore so isn't
# on github). 

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)
library(patchwork)
library(tidybayes)
library(tidyr)


runmodels <- FALSE #set to true to rerun models,
# set to false to load files
saveplots <- FALSE

dat2 <- read.csv("Outputs/glm-covariates-merged-Bmax.csv")
dat2$clupeoid <- relevel(factor(dat2$clupeoid), ref = "Other")

theme_set(theme_classic())

#Set response variables to run the best model for 
response_vars <- c("Delta_Brel",
                   "Delta_B",
                   "Delta_B1")

response_names <- c(expression(Delta*'B/B'[max]),
                    expression(Delta*'B'),
                    expression(Delta*'B'[max])
)

gfixie <- fixef_save <- gpreds <- gpredsvalue <- gpostdists <- gpostdists_scaled <- NULL #lists to save key plots


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
    "b_clupeoidClupeid",
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
  
  x = data.frame(ranef(m1, groups="stocklong", probs = 0.5)) %>%
    arrange(stocklong.Q50.Intercept)
  
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
                               "Survey age" = "year.diff",
                               "Depletion" = "lnBrel_MRA",
                               "Value by \n depletion" = "stock_value:lnBrel_MRA",
                               "Depletion by \n survey age" = "year.diff:lnBrel_MRA"
    ))%>%
    mutate(params = factor(params, levels = c(
      "Duration",
      "Mean SST",
      "SST trend",
      "Clupeoid",
      "Value",
      "Survey age",
      "Depletion",
      "Value by \n depletion",
      "Depletion by \n survey age"
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
  # Fixed effects as distributions
  #
  
  x <- m1  %>%
    spread_draws(b_stock_value, b_year.diff,
                 b_lnBrel_MRA, b_start.diff, 
                 b_trend.50yr.coef.cap,
                 b_HADISSTmean.5yr,
                 b_clupeoidClupeid,
                 `b_stock_value:lnBrel_MRA`,                                            
                 `b_year.diff:lnBrel_MRA`) %>%
    pivot_longer(cols = b_stock_value:`b_year.diff:lnBrel_MRA`)  %>% 
    left_join(SDs_of_covariables, by = c("name" = "param")) %>%
    mutate(params = fct_recode(factor(name),
                               "Duration" = "b_start.diff",
                               "Mean SST" = "b_HADISSTmean.5yr",
                               "SST trend" = "b_trend.50yr.coef.cap",
                               "Clupeoid" = "b_clupeoidClupeid",
                               "Value" = "b_stock_value",
                               "Age" = "b_year.diff",
                               "Depletion" = "b_lnBrel_MRA",
                               "Value x depletion" = "b_stock_value:lnBrel_MRA",
                               "Age x depletion" = "b_year.diff:lnBrel_MRA"
    ))%>%
    mutate(params = factor(params, levels = c(
      "Duration",
      "Mean SST",
      "SST trend",
      "Clupeoid",
      "Value",
      "Age",
      "Depletion",
      "Value x depletion",
      "Age x depletion"
    ))) %>%
    mutate(value_scaled = value * sd)
  
  sig_names <- paste0("b_", fixef$rowname[sign(fixef$Q2.5) == sign(fixef$Q97.5)])
  x$sig <- 'a'
  x$sig[x$name %in% sig_names] <- 'b'
  
  g1 <- x %>%
    ggplot() +
    aes(y = params, x = value, color = sig) +
    geom_vline(xintercept = 0) + 
    # stat_halfeye(normalize = "xy", fill_type = "segments", alpha = 0.8) +
    stat_pointinterval(.width = c(0.5, 0.8, 0.95),
                       interval_size_domain = c(1, 4),
                       fatten_point = 0.9) +
    # scale_color_brewer(palette = 1) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("grey30", "red")) +
    # scale_color_manual(values = c("#273254", "#405427")) + 
    ylab("") + 
    xlab("Effect size")
  
  gpostdists <- c(gpostdists, list(g1))
  
  
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
  
  gpostdists_scaled <- c(gpostdists_scaled, list(g1))
  
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
    xlab("Assessment age (years)") +
    xlim(0, 15) + 
    scale_y_continuous(breaks = seq(0.5, 2.75, by = 0.5),
                       labels = seq(0.5, 2.75, by = 0.5),
                       limits = c(0.5, 2.75)) +
    # scale_y_log10(breaks = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
    # labels = c(0.6, 0.8, 1, 1.2,1.5, 2, 2.5,3,4),
    # scale_y_log10(breaks = 10^(seq(-1, 1, by = 0.5)),
    # labels = 10^(seq(-1, 1, by = 0.5)),
    # limits = c(0.5, 4)) +
    scale_fill_manual(expression('B/B'[max]), values = c("#d41515", "black", "#0537ab"))
  
  
  gpreds <- c(gpreds, list(g1))
  
  if(saveplots)
    ggsave(g1, file =paste0("Outputs/",ivar,"/years-to-MRA-Brelative.png"))
  
  #
  #Mean delta Brel for 12 years, current status 0.05
  #
  newdata2 <- with(dat2, expand.grid(
    lnBrel_MRA = c(log(0.1)),
    year.diff = 12,
    start.diff = mean(start.diff),
    HADISSTmean.5yr = mean(HADISSTmean.5yr),
    trend.50yr.coef.cap = mean(trend.50yr.coef.cap),
    stock_value = mean(stock_value),
    clupeoid = "Other",
    stocklong = NA,
    Group = NA
  ))
  print("Bias for stock 12 years ago")
  posterior_epred(m1, newdata = newdata2,
                  re.form = NA) %>%
    apply(2,quantile, c(0.025, 0.5, 0.975)) %>%
    t() %>%
    exp() %>%
    print()
  
  
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
    scale_fill_manual(expression('B/B'[max]), values = c("#d41515", "black", "#0537ab"))+
    ylab(response_names[ivar == response_vars]) +
    xlab("Value")
  
  gpredsvalue <- c(gpredsvalue, list(g1))
  
  
  if(saveplots)
    ggsave(g1, file =paste0("Outputs/",ivar,"/value-Brelative.png"))
  
  
}

#
# Make multipanel figures
#

save(gpreds, gfixie, fixef_save,gpostdists,
     file = "Outputs/2024-08-02_plots-main-models.rda")

gall <- 
  (gpreds[[1]] + 
     theme(axis.text.y = element_text(size = 9),
           axis.title=element_text(size=9),
           axis.text = element_text(color = "black"))) + 
  (gpreds[[2]] + 
     theme(axis.text.y = element_text(size = 9),
           axis.title=element_text(size=9),
           axis.text = element_text(color = "black"))) +
  (gpreds[[3]] + 
     theme(axis.text.y = element_text(size = 9),
           axis.title=element_text(size=9),
           axis.text = element_text(color = "black"))) + 
  plot_layout(guides='collect', ncol = 3) + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = 'bold')) 

gall
ggsave("Outputs/Obsolesence-deltas-same-scale.pdf", gall,
       width = 18.3, height = 18.3*3/8, units = "cm")

ggsave("Outputs/Obsolesence-deltas-same-scale.png",
       gall,
       width = 8, height =3)


gallfix <- gfixie[[1]] + 
  (gfixie[[2]] + theme(axis.text.y = element_blank())) +
  (gfixie[[3]]+ theme(axis.text.y = element_blank())) + 
  plot_annotation(tag_levels ="A") +   plot_layout(guides='collect') 

ggsave("Outputs/fixed-effects-deltas.png",
       gallfix,
       width = 8, height =3)

gall_postdists <- 
  (gpostdists[[1]] + 
     theme(axis.text.y = element_text(size = 9),
           axis.title=element_text(size=9),
           axis.text = element_text(color = "black"))
  ) + 
  (gpostdists[[2]] + theme(axis.text.y = element_blank(),
                           axis.title=element_text(size=9),
                           axis.text = element_text(color = "black"))) +
  (gpostdists[[3]]+ theme(axis.text.y = element_blank(),
                          axis.title=element_text(size=9),
                          axis.text = element_text(color = "black"))) + 
  plot_annotation(tag_levels ="A") +   plot_layout(guides='collect') 

ggsave("Outputs/fixed-effects-posteriors.png",
       gall_postdists,
       width = 12, height =4)
+ 
  theme( axis.title=element_text(size=9),
         axis.text = element_text(color = "black"),
         legend.position = "none")

gall_postdists_scaled <- 
  (gpostdists_scaled[[1]] + 
     coord_cartesian(ylim = c(0, 9), # This focuses the x-axis on the range of interest
                     clip = 'off')+
     annotate("text", x = -0.08, y = 9.8,
              label = as.character(response_names[1]),
              color = "black", size = 10, fontface = "plain",
              size.unit = "pt",parse = T,check_overlap = TRUE) +
     theme(axis.text.y = element_text(size = 9),
           axis.title=element_text(size=9),
           axis.text = element_text(color = "black"),
           legend.position = "none")) + 
  (gpostdists_scaled[[2]] + 
     coord_cartesian(ylim = c(0, 9), # This focuses the x-axis on the range of interest
                     clip = 'off')+
     annotate("text", x = -0.08, y = 9.8,
              label = as.character(response_names[2]),
              color = "black", size = 10, fontface = "plain",
              size.unit = "pt",parse = T,check_overlap = TRUE) +
     theme(axis.text.y = element_blank(),
           axis.title=element_text(size=9),
           axis.text = element_text(color = "black"),
           legend.position = "none")) +
  (gpostdists_scaled[[3]]+ 
     coord_cartesian(ylim = c(0, 9), # This focuses the x-axis on the range of interest
                     clip = 'off') + 
     annotate("text", x = -0.08, y = 9.8,
              label = as.character(response_names[3]),
              color = "black", size = 10, fontface = "plain",
              size.unit = "pt",parse = T,check_overlap = TRUE) +
     theme(axis.text.y = element_blank(),
           axis.title=element_text(size=9),
           axis.text = element_text(color = "black"),
           legend.position = "none")) + 
  plot_annotation(tag_levels ="A") +   plot_layout(guides='collect') &
  theme(plot.tag = element_text(face = 'bold'))#&
  # scale_colour_manual(values = c("grey80", "grey30")) 
gall_postdists_scaled

ggsave("Outputs/fixed-effects_scaled-posteriors.pdf", gall_postdists_scaled,
       width = 18.3, height = 18.3/2.5, units = "cm")

ggsave("Outputs/fixed-effects_scaled-posteriors.png",
       gall_postdists_scaled,
       width = 12, height =4)

gall <- gpredsvalue[[1]] + gpredsvalue[[2]] +
  gpredsvalue[[3]] + 
  plot_annotation(tag_levels ="A") +   plot_layout(guides='collect') 

ggsave("Outputs/Obsolesence-value-deltas-same-scale.png",
       gall,
       width = 8, height =3)

#
# Fixed effects - distributions plot 
#



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

