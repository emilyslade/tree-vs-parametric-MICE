# tree-vs-parametric-MICE
This repository contains files relevant to the manuscript: Slade E. & Naylor MG. (2020). A fair comparison of tree-based and parametric methods in multiple imputation by chained equations. Statistics in Medicine, 39(8): 1156-1166.
An overview of each file is below.


data_generation.R
 - Simulates data according to Scenarios 1 and 2
 - Start here and simulate data before moving on to Scenario_1.R or Scenario_2.R
 
Scenario_1.R
 - Corresponds to Scenario 1, data with an interaction
 - Imputes missing values according to PMM-Naive, PMM-Int, CART, and RF methods
 - Fits final analysis model and pools results
 - Summarizes results in terms of bias, coverage, and 95% CI width

Scenario_2.R
 - Corresponds to Scenario 2, data with no interaction
 - Imputes missing values according to PMM-Naive, PMM-Int, CART, and RF methods
 - Fits final analysis model and pools results
 - Summarizes results in terms of bias, coverage, and 95% CI width
