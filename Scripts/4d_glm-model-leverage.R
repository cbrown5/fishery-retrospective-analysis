# GLM of bias for B/B0 ("Brel")
#
# CJ Brown 2023-05-12
#
#4c: Influence of individual data points on final results
#
rm(list =ls())

library(dplyr)
library(ggplot2)
library(brms)
library(forcats)

load("Outputs/Delta_Brel/best-model-fit.rda")
  theme_set(theme_classic())

#
# GLMM Delta_Brel
#

l1 <- loo(m1)
# save(l1, file = "Outputs/loo-m1-reloo.rda")

#Identify observations with high influence: 
high_pareto <- which(l1$pointwise[,"influence_pareto_k"]>0.7)
#rerun model excluding obs one by one 
# where pareto_k > 0.7

#
# Rerun model, excluding one obs at a time
#
mall <- NULL
for (i in high_pareto){
  mtemp <- update(m1, 
                  newdata = m1$data[-i,],
                  cores = 4)
  mall <- c(mall, list(mtemp))
}

save(mall, high_pareto, file = "Outputs/Brel_leverage-models.rda")

#
# Do plot of coefs from each model rep
#
#add full model
mall2 <- c(list(m1), mall)
#function to extract params from each model rep
getparams <- function(m1){
  sm1 <- summary(m1)
  d <- rbind(sm1$fixed,
        sm1$random$stocklong) %>%
    signif(2) %>%
    data.frame() %>%
    tibble::rownames_to_column("Parameter")
  d
}

allparams <- lapply(mall2, getparams) %>%
  do.call("rbind",.)

allparams2 <- filter(allparams, Parameter %in% c("year.diff",
                                                 "lnBrel_MRA", 
                                                 "year.diff:lnBrel_MRA"))
allparams2$rep <- rep(1:(length(high_pareto)+1), each = 3)
  
g1 <- ggplot(allparams2) + 
  aes(x = rep, y = Estimate) + 
  geom_hline(yintercept = 0) +
  geom_point() + 
  geom_point(data = filter(allparams2, rep ==1),
             color = "red")+
  geom_linerange(aes(ymin = l.95..CI, ymax = u.95..CI)) +
  geom_linerange(data = filter(allparams2, rep ==1),
                 color = "red",aes(ymin = l.95..CI, ymax = u.95..CI)) +
  facet_wrap(~Parameter)
g1

ggsave("Outputs/influence-outliers-on-parameters.png",
       g1,
       width = 8, height =3) 
