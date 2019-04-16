# tree-vs-parametric-MICE
This repository contains files relevant to the manuscript: Slade E. & Naylor MG. (in review). A Fair Comparison of Tree-based and Parametric Methods in Multiple Imputation by Chained Equations. An overview of each file is below.


data_generation.R
 - Simulate data according to Scenarios 1 and 2
 - Start here and simulate data before moving on to Scenario_1.R or Scenario_2.R
 
Scenario_1.R
 - Corresponds to Scenario 1, data with an interaction
 - Impute missing values according to PMM-Naive, PMM-Int, CART, and RF methods
 - Fit final analysis model and pool results
 - Summarize results in terms of bias, coverage, and 95% CI width

Scenario_2.R
 - Corresponds to Scenario 2, data with no interaction
 - Impute missing values according to PMM-Naive, PMM-Int, CART, and RF methods
 - Fit final analysis model and pool results
 - Summarize results in terms of bias, coverage, and 95% CI width
