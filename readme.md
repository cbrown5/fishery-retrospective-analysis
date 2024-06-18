# Stock assessment models overstate sustainability of the world’s fisheries

### R code and datasets used for the study "Stock assessment models overstate sustainability of the world’s fisheries"

CJ Brown, GJ Edgar

Institute of Marine and Antarctic Studies, University of Tasmania, Hobart, Tasmania, Australia. 

2024-06-17

Repeatable code for the study "Stock assessment models overstate sustainability of the world’s fisheries"

Contact: Dr Chris Brown c.j.brown@utas.edu.au

### Recommended citation: 

It is recommended you look up the peer-reviewed study by the same title and cite that. 

Brown, C.J., Edgar, G.J., 2024. R code to support the study: stock assessment models overstate sustainability of the world’s fisheries. Zenodo. DOI: 10.5281/zenodo.12043237


[![DOI](https://zenodo.org/badge/816561171.svg)](https://zenodo.org/doi/10.5281/zenodo.12043236)


## Overview and Aims

Fish stock biomass estimates are fundamental to defining the status of fish stocks and informing on sustainable management of fisheries. Fish stock assessments are repeated regularly, providing an opportunity to see how stock status for a given year has changed in more recent assessments. We aimed to quantify the level of variability and bias in stock biomass and stock status estimates. We did this by conducting a retrospective study, where we looked at status and biomass estimates for the same year in different assessments. We then used a statistical model to investigate the factors that influence the bias in stock status estimates.

## Data sources

Data used in analyses that describe modeled hindcast trends in spawning stock biomass were obtained from three sources:

Version 1.0 (downloaded 15 Aug 2011) and Version 4.46 (downloaded on 2 July 2020) extracts from the RAM Legacy database—the global repository of fisheries data  (https://www.ramlegacy.org/; Ricard, D., Minto, C., Jensen, O.P., Baum, J.K., 2012.
Examining the knowledge base and status of commercially exploited marine species with the RAM Legacy Stock Assessment Database. Fish and Fisheries 13, 380-398);
the SMART compilation of data for USA fisheries NOAA Fisheries, 2020. Assessment Time Series Data. Retrieved from www.st.nmfs.noaa.gov/stocksmart on 30 June 2020; and
modeled annual biomass trends for 16 stocks investigated in the Australian South East Scalefish and Shark Fishery retrospective analysis (data plotted as Fig. 1 of Punt, A.E., Day, J., Fay, G., Haddon, M., Klaer, N., Little, L.R., Privitera-Johnson, K., Smith, A.D., Smith, D.C., Sporcic, M., 2018. Retrospective investigation of assessment uncertainty for fish stocks off southeast Australia. Fisheries Research 198, 117-128).

Please see those sources for original data. A .csv of all stock timeseries used in analysis is provided here so that the workflow is repeatable. 

## Data

Data File 1: RLSADB v4.44 bioparams-view.csv Data
File 1 Description: The 'bioparams view' sheet from the [Ram Legacy database](https://www.ramlegacy.org/). Provided here so data workflow is repeatable. This is used just to compare B1 (initial biomass) to unfished biomass for stocks where that stat is available.  

Data File 2: glm-covariates-Hadley.csv
Data File 2 Description: Covariates for the GLMMs.

Data File 3: stock-timeseries-databases.csv

Data File 3 Description: Timeseries of spawning stock biomass from all stocks and all assessments.
See Scripts/1_data-processing.R for description of variables.

Data File 4: stock_groups.csv
Data File 4 Description: Groupings of stocks into taxonomic categories.

Data File 5: regions.csv
Data File 5 Description: Groupings of stocks into regions.

Data File 6: stock_groups.csv
Data File 6 Description: Groupings of stocks into taxonomic categories.


## Scripts

Scripts/ folder contains files numbered in order of how they are intended to be run. Scripts that are prefixed with the same number are different versions (sensitivity analyses) of the same analysis task. 

**1_data-processing**

Takes raw stock assessment database and processes into tidy datasets for analysis.

**2_pacific-cod-plot**

Makes figure 1, hindcast trends for Pacific cod.

**3_mean-depletion-figure**  

Mean depletion trends over time.

**3_mean-depletion-figures-perc-diff**  

Mean depletion trends over time as percentage difference from MRA. 

**3_mean-depletion-figures-sensitivity**  
Alt versions of mean depletion figures where we used different subsets of the data to check for biases caused by unequal sample sizes (assessment by year combinations) across years. No changes in conclusions were found. 


**3_mean-depletion-figures-newcategories**  
In this version of the mean depletion figures we tested different categories for defining depletion. Bias in older assessments was higher if lower thresholds for defining depletion (e.g. 0.3 instead of 0.4) were used. The 0.4 threshold used in the manuscript is therefore conservative in that it understates bias in depletion ratios. 

**3b_figure2-supp-timeseries**
Brings together mean depletion figures into a single panel.

**3c_overlay-fig2-completeobs** 
Mean depletion figure , but with the version with complete obs 1980-2016 overlayed over the top. Results are similar and conclusions the same as the figures used in the paper. 


**4_glm-model-selection**

Fits GLMM for analysis of delta B/Bmax in relation to covariates. Fits all models and calculates the WAIC statistic. Makes Supp figures for residuals. 

**4b_glm-model-selection**

Fits GLMM for analysis of delta B/Bmax in relation to covariates. Includes year of MRA as a covariate, to test the effect of this covariate. Year of MRA was not included in the main model because a-priori there is a stronger case for other variables and year of MRA was partly confounded with some other variables. 

**4c_glm-model-selection, 4d_glm-model-leverage**

Other analyses done to check robustness of results. first one excludes rebuilding stocks, second one looks at leverage of individual data points. Nothing of note found. 

**5_glm-automated-by-variable**  

Fits the GLMM for each bias (delta) statistic (i.e. bias for delta B/B1, B and B1). Creates plots for each. Makes figures 4 and 5 (GLM results figures). 

**6_status_stats_bias-corrected**

Calculates status in the MRA MRY, based on the fitted GLMM and assuming retrospective analysis from 10 years in the future.

**7_assessment-plot-all-species**
Makes panels of B relative for all species

**8_data-processing-stability**
Same as script 1_, except we remove data after 2010, as sensitivity test. The ouptut is then input into script 9. 

**9_glm-stability-analysis-before-2011**  
Run the GLM but only using data up until 2010. This is to check if we get fundamentally the same results on an earlier subset of the data. Results show the main interactive effect of bias increasing for older assessments and more depleted stocks was robust if only pre 2010 data were used

**10_two-year-trend-bias**
Bias in SSB trends over a 2 year period. 

**11_mean-depletion-overlay**
Creates figure from output of 3_mean-depletion-figures-perc-diff and 10_two-year-trend-bias. 

## Data specific information:

NA: missing data codes

### Stock-timeseries-databases.csv

Variable definitions: 

Stock Name: Name of the stock

assesid: Assessment ID (multiple assesments per stock)

ram: Data source (RAM Legacy or SMART database)

managementauthority: Management authority

country: Country

initial: First year of assessment

finish2: Last year of assessment

max finish2: Last year of most recent assessment

region: Stock region

scientificname: Species name

stocklong: Stock name. Use this to join by stocks across different datasets. 

tsid: Time series ID

tsyear: Year of assessment. 

SSB: Spawning stock biomass/Total biomass

commonname: Species common name

### stock_groups.csv

Variable definitions: 

stocklong: Stock name

Species: Species common name

Fishery group: Types of fish species

### glm-covariates-Hadley.csv

Variable definitions: 

country: Country

region: Stock region

stocklong: Stock name

Common.name: Species common name

Species.name: Scientific name

dollar_per_tonne: Price per tonne for this species

Lat: Latitude of stock (degrees)

long: Longitude of stock (degrees)

SSTmeanBO: Mean sea surface temperature for the stock. 

HADISSTmean.5yr: 5 year mean of Hadley SST data

HADISSTtrend.50yr.coef: Coefficient of the trend in Hadley SST data

### regions.csv

Variable definitions:

Region: Stock region

stocklong: Stock name

