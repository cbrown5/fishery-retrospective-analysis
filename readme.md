# R code for Complex stock assessment models overstate sustainability of the world’s fisheries

CJ Brown, GJ Edgar

2024-02-02

Repeatable code for the study "Complex stock assessment models overstate sustainability of the world’s fisheries"

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

Reviewers requested many alterations, but most especially to the threshold for overfishing (variously used 0.2, 0.3, 0.4 and 0.5) and to the statistic for B0 (either B1 or Bmax). (Curiously some reviewers contested the findings of bias on the basis that 0.4 was an inappropriate threshold and instead asked we use 0.3 or 0.2 - thresholds that give even stronger bias results). So when running through these scripts be sure to use consistent statistics. In the revision 2/2/2024 we used Bmax and 0.4, but with supplemental analyses for B1. 

Reviewers also requested many supplemental analyses, which are preserved in this code. Not all of these supplemental analyses made it into the final paper, for the sake of not overwhelming our readers. Regardless, the conclusions of the study were incredibly robust to any requests for alternative processing or analyses that reviewers asked for. 

Feel free to contact me if you have questions. 

**1_data-processing**

Takes raw stock assessment database and processes into tidy datasets for analysis.

**2_pacific-cod-plot**

Makes figure 1, hindcast trends for Pacific cod.

**3_mean-depletion-figures**  

Makes figure 2, mean depletion trends over time.

**3_mean-depletion-figures-perc-diff**  

Part of figure 3, mean depletion trends over time as percentage difference from MRA. 

**3_mean-depletion-figures-sensitivity**  
Alt versions of figure 2 where we used different subsets of the data
to check for biased caused by unequal sample sizes across years. 

**4_glm-model-selection**

Fits GLMM for analysis of delta B/B1 in relation to covariates. Fits all models and calculates the WAIC statistic. Makes Supp figures for residuals. 

**4b_glm-model-selection**

Fits GLMM for analysis of delta B/B1 in relation to covariates. Includes year of MRA as a covariate, to test teh effect of this covariate. Year of MRA was not included in the main model because a-priori there is a stronger case for other variables and year of MRA was partly confounded with some other variables. 

**4c_glm-model-selection, 4d_glm-model-leverage**

Other analyses done at reviewer request. Nothing of note found. 

**5_glm-automated-by-variable**  

Fits the GLMM for each delta statistic (i.e. bias for delta B/B1, B and B1). Creates plots for each. Makes figures 4 and 5 (GLM results figures). 

**6_status_stats_bias-corrected**

Calculates status in the MRA MRY, based on the fitted GLMM and assuming retrospective analysis from 10 years in the future.

**7_assessment-plot-all-species**
Makes panels of B relative for all species

**8_data-processing-stability**
Same as script 1_, except we remove data after 2010, as a kind of sensitivity test

**9_glm-stability-analysis-before-2011**  
Run the GLM but only using data up until 2010. This is to check if we get fundamentally the same results on an earlier subset of the data

**10_two-year-trend-bias**
Bias in trends

**11_figure3**
Creates figure three from output of 3_mean-depletion-figures-perc-diff and 10_two-year-trend-bias. 

