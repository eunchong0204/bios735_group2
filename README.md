BIOS 735 Final Project - Heart Disease
--------------
Group 2 - Mingwei, Di, Wanting, Eunchong, Andrew  
April 30 2022

  In 2020, heart disease was the leading cause of death in the United States with 696,962 deaths
attributed (followed by cancer & COVID-19) according to the final 2020 U.S. mortality data from
the CDC. Controlling for population, there were 168.2 deaths attributed to heart disease per 100,000
individuals in the United States. Given the seriousness of this cause of death, it is imperative that
we understand the underlying factors that contribute to increased risk of heart disease which leads
to reduced quality of life and increased risk of mortality. Binary outcome statistical models were fit
to estimate the relationship between heart disease occurrence and relevant clinical & demographic
covariates. The greatest contributing risk factors for heart disease are old age, stroke occurrence,
diabetes, and poor general health. These statistical models achieved a 99.2% specificity (true neg-
ative) rate, but only a 10.8% sensitivity (true positive) rate which corresponds to a 54.9% positive
predictive value such that the chances of correctly classifying a case of heart disease are only slightly
better than a coin flip. Thus, it is clear that it is challenging to predict positive cases of heart disease
in part to its relatively low occurrence rate in the greater population.

  The dataset we investigated comes from the 2020 CDC annual survey data and is a major part of the Behavioral Risk Factor Surveillance System (BRFSS), which conducts annual telephone surveys to gather data on the health status of U.S. residents. The dataset contains 319,795 observations in total. The occurrence of Heart Disease is the outcome for our classification tasks and 17 key indicators for health status are included as potential predictors. 

  Methods employed to analyze and make predictions on the heart disease data include: decision trees, logistic regression, support vector machines, and random forests

  Prior to executing the [code](code/primary_project_code.Rmd), the user must first install the following R packages: "tidyverse", "caret", "MASS", "optimx", "dplyr", "ROCR", "patchwork", "mosaic", "rpart", "rpart.plot", "Rcpp", "RcppArmadillo", "pROC", "ranger", "e1071" and "glmLogistic". A user must also [download](https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease) the data file "heart_2020_cleaned.csv" and alter the filepath assigned to the object "heart.data <- read.csv(filepath, sep="")" with the new data location. No other modifications should be required prior to executing the code. Executing `Primary_Project_Code.Rmd` will produce all relevant results and figures from the analysis. Furthermore, a formal project report can be accessed [here](reports/BIOS735_ProjectReport_Group2.pdf)
