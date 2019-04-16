# This script corresponds to Scenario 2 in Slade & Naylor (in review), for data with no interaction term.
# Must first generate data (see data_generation.R). This script loads data, imputes missing values using MICE
# with the PMM-Naive, PMM-Int, CART, and RF methods. Next, the final analysis model is fit and results from
# the each imputed dataset are combined. Finally, bias, coverage, and 95% confidence interval width are 
# computed for each method.


## Packages

library(mice)
library(rpart)
library(randomForest)


## Functions

micePool <- function(impdata)  return(summary(pool(with(impdata, lm(y~x1+x2+x3+x4))), conf.int=T))
getBias <- function(summary, trueBeta) return(summary$estimate - trueBeta)
getCoverage <- function(summary, trueBeta) return((summary$'2.5 %'<=trueBeta & summary$'97.5 %'>=trueBeta)*1)
getCIwidth <- function(summary) return((summary$'97.5 %' - summary$'2.5 %'))


## Number of simulation repetitions (number of datasets)
## Note: Run time is long for large values of R; parallelizing computation is recommended

R <- 10000


## Load data

load("scen2.data.miss.RData")
load("scen2.data.int.miss.RData")


## Initialize objects

imp.pmm.naive <- imp.pmm.int <- imp.CART <- imp.RF <- list()


for(i in 1:R){
  
  ## Fit imputation models
  
  imp.pmm.naive[[i]] <- mice(data=scen2.data.miss[[i]], m=10, maxit=10, defaultMethod=c("pmm","logreg","polyreg"), printFlag=F)
  imp.pmm.int[[i]] <- mice(data=scen2.data.int.miss[[i]], m=10, maxit=10, defaultMethod=c("pmm","logreg","polyreg"), printFlag=F)
  imp.CART[[i]] <- mice(data=scen2.data.miss[[i]], m=10, maxit=10, method='cart', minbucket=5, cp=1e-04, printFlag=F)
  imp.RF[[i]] <- mice(data=scen2.data.miss[[i]], m=10, maxit=10, method='rf', ntree=10, printFlag=F)
  
  ## Print progress every 5%
  
  if(i%%(R/20)==0) print(paste0((i/R)*100,"%"))
}


## Fit final analysis model on each imputed dataset, pool results into one analysis, and summarize

summary.pmm.naive <- lapply(imp.pmm.naive, micePool)
summary.pmm.int <- lapply(imp.pmm.int, micePool)
summary.CART <- lapply(imp.CART, micePool)
summary.RF <- lapply(imp.RF, micePool)


##
## COMPUTE BIAS
## Each object is Rx5 matrix of bias for each of the 5 coefficients from each of the simulated datasets
##

scen2.trueBeta <- c(0,0.317,0.317,0.317,0.317)

bias.pmm.naive <- matrix(unlist(lapply(summary.pmm.naive, getBias, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
bias.pmm.int <- matrix(unlist(lapply(summary.pmm.int, getBias, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
bias.CART <- matrix(unlist(lapply(summary.CART, getBias, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
bias.RF <- matrix(unlist(lapply(summary.RF, getBias, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
colnames(bias.pmm.naive) <- colnames(bias.pmm.int) <- colnames(bias.CART) <- colnames(bias.RF) <- c("beta0","beta1","beta2","beta3","beta4")

# Display mean bias
apply(bias.pmm.naive, 2, mean)
apply(bias.pmm.int, 2, mean)
apply(bias.CART, 2, mean)
apply(bias.RF, 2, mean)

# Display mean absolute error
apply(abs(bias.pmm.naive), 2, mean)
apply(abs(bias.pmm.int), 2, mean)
apply(abs(bias.CART), 2, mean)
apply(abs(bias.RF), 2, mean)


##
## COMPUTE COVERAGE
## Each object is Rx5 matrix of 0's and 1's indicating coverage for each of the 6 coefficients from each of the simulated datasets
##

coverage.pmm.naive <- matrix(unlist(lapply(summary.pmm.naive, getCoverage, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
coverage.pmm.int <- matrix(unlist(lapply(summary.pmm.int, getCoverage, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
coverage.CART <- matrix(unlist(lapply(summary.CART, getCoverage, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
coverage.RF <- matrix(unlist(lapply(summary.RF, getCoverage, trueBeta=scen2.trueBeta)), nrow=R, ncol=5, byrow=T)
colnames(coverage.pmm.naive) <- colnames(coverage.pmm.int) <- colnames(coverage.CART) <- colnames(coverage.RF) <- c("beta0","beta1","beta2","beta3","beta4")

# Display coverage
apply(coverage.pmm.naive, 2, mean)
apply(coverage.pmm.int, 2, mean)
apply(coverage.CART, 2, mean)
apply(coverage.RF, 2, mean)


##
## COMPUTE 95% CI WIDTH
## Each object is Rx5 matrix of 95% CI width for each of the 6 coefficients from each simulated dataset
##

CIwidth.pmm.naive <- matrix(unlist(lapply(summary.pmm.naive, getCIwidth)), nrow=R, ncol=5, byrow=T)
CIwidth.pmm.int <- matrix(unlist(lapply(summary.pmm.int, getCIwidth)), nrow=R, ncol=5, byrow=T)
CIwidth.CART <- matrix(unlist(lapply(summary.CART, getCIwidth)), nrow=R, ncol=5, byrow=T)
CIwidth.RF <- matrix(unlist(lapply(summary.RF, getCIwidth)), nrow=R, ncol=5, byrow=T)
colnames(CIwidth.pmm.naive) <- colnames(CIwidth.pmm.int) <- colnames(CIwidth.CART) <- colnames(CIwidth.RF) <- c("beta0","beta1","beta2","beta3","beta4")

# Display average 95% CI width
apply(CIwidth.pmm.naive, 2, mean)
apply(CIwidth.pmm.int, 2, mean)
apply(CIwidth.CART, 2, mean)
apply(CIwidth.RF, 2, mean)

