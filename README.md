BIOS 735 Final Project - Heart Disease
--------------
Group 2 - Mingwei, Di, Wanting, Eunchong, Andrew  
April 30 2022

According to the CDC, heart disease is one of the leading causes of death for people of most races in the US (African Americans, American Indians and Alaska Natives, and white people). Many health status indicators are found related to heart disease such as  high blood pressure, high cholesterol, smoking, diabetic status, obesity (high BMI), not getting enough physical activity or drinking too much alcohol. Detecting and preventing the factors that have the greatest impact on heart disease is very important in healthcare.

The dataset to be investigated comes from the 2020 CDC annual survey data and is a major part of the Behavioral Risk Factor Surveillance System (BRFSS), which conducts annual telephone surveys to gather data on the health status of U.S. residents. The dataset contains 319,795 observations in total. We choose the presence of Heart Disease as the outcome for our classification problem and 17 key indicators for health status are included as potential predictors. 

Methods employed to analyze the heart disease data include: logistic regression classification, support vector machines

Prior to executing the code for this project, the user must first install the following R packages: "tidyverse", "caret", "MASS", "optimx", "dplyr", and "OUR R PACKAGE". A user must also download the data file "heart_2020_cleaned.csv" from https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease and alter the filepath assigned to the object "heart.data <- read.csv(filepath, sep="")" with the new data location. No other modifications should be required prior to executing the code.
