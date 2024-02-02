# R code for Complex stock assessment models overstate sustainability of the world’s fisheries

CJ Brown, GJ Edgar

2023-02-09

Repeatable code for the study "Complex stock assessment models overstate sustainability of the world’s fisheries"


Thanks. Now if you can change the axis label to Bmax rather than B1 then it will be perfect.

Wrt repeating figures with B1 as a consistency check with Bmax, then if you can do figs 3-5 with B1 rather than Bmax for the supps then that would be great.

Not important, but for consistency with Fig 4 and a comparable fig with B1, then it is probably good to have Fig S4 with B and Bmax also included as well as B/Bmax. Oh, and the model fit plot at the end of supps with regression or no line.

## Data

Data File 1: RLSADB v4.44 bioparams-view.csv Data
File 1 Description: The 'bioparams view' sheet from the [Ram Legacy database](https://www.ramlegacy.org/). Provided here so data workflow is repeatable. This is used just to compare B1 (initial biomass) to unfished biomass for stocks where that stat is available.  

Data File 2: stock-covariates.csv
Data File 2 Description: Covariates for the GLMM.

Data File 3: stock-timeseries-databases.csv

Data File 3 Description: Timeseries of spawning stock biomass from all stocks and all assessments.
See Scripts/1_data-processing.R for description of variables.

## Scripts

Scripts/ folder contains files numbered in order of how they are intended to be run.

**1_data-processing**

Takes raw stock assessment database and processes into tidy datasets for analysis.

**2_pacific-cod-plot**

Makes figure 1, hindcast trends for Pacific cod.

**3_mean-depletion-figures**  

Makes figure 2, mean depletion trends over time.

**3_mean-depletion-figures-sensitivity**  
Alt versions of figure 2 where we used different subsets of the data
to check for biased caused by unequal sample sizes across years. 

**4_glm-model-bias-and-hindcast**

Fits GLMM for analysis of delta B/B1 in relation to covariates. Fits all models and calculates the WAIC statistic.

**5_status_stats_bias-corrected**

Calculates status in 2020, based on the fitted GLMM and assuming retrospective analysis from 2030.

**6_glm-automated-by-variable**  

Fits the GLMM for each delta statistic (i.e. bias for delta B/B1, B and B1). Creates plots for each.
