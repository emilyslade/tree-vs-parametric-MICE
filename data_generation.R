# This R script generates data according to Scenarios 1 and 2 as described in
# Slade & Naylor (2020). Scenario 1 represents a setting with a true underlying
# interaction, and Scenario 2 represents a setting with no underlying interaction.
# Please refer to Slade & Naylor (2020) for a more detailed description.

# This code generates two data files for each scenario. The first, named
# scen#.data.miss, contains the simulated data for each variable after missingness
# has been simulated. The second, named scen#.data.int.miss, contains the same
# data with an extra column for the hard-coded interaction term. These two data
# files are necessary for fitting the various imputation models.



library(mvtnorm)


##############################
########  SCENARIO 1 #########
##############################

scen1.data.miss <- list()
scen1.data.int.miss <- list()

## Set parameters

n <- 200                                  ## sample size
SigmaX <- matrix(0.5,4,4)                 ## covariance matrix for X1, X2, X3, X4
diag(SigmaX) <- 1                         ## covariance matrix for X1, X2, X3, X4
muX <- rep(0,4)                           ## mean vector for X1, X2, X3, X4
beta0 <- 0                                ## true intercept  
beta1 <- beta2 <- beta3 <- beta4 <- 0.3   ## true main effects
betaint <- 0.3                            ## true interaction effect
R <- 10000                                ## number of simulation repetitions/datasets simulated


for(i in 1:R){
  
  ## Generate complete data
  
  X <- rmvnorm(n, muX, SigmaX)
  x1 <- X[,1]
  x2 <- X[,2]
  x3 <- X[,3]
  x4 <- X[,4]
  epsilon <- rnorm(n, 0, 1)
  y <- beta0 + beta1*x1 + beta2*x2 + beta3*x3 + beta4*x4 + betaint*x1*x2 + epsilon 
  data_full <- data.frame(y,x1,x2,x3,x4)
  
  
  ## Induce missingness
  
  data_miss <- data_full
  data_miss$y[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$y[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  data_miss$x1[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$x1[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  data_miss$x2[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$x2[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  data_miss$x3[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$x3[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  scen1.data.miss[[i]] <- data_miss
  
  scen1.data.int.miss[[i]] <- data.frame(data_miss, data_miss$x1*data_miss$x2)
  colnames(scen1.data.int.miss[[i]]) <- c(colnames(data_miss),"int")
  
}

save(scen1.data.miss, file="scen1.data.miss.RData")
save(scen1.data.int.miss, file="scen1.data.int.miss.RData")




##############################
########  SCENARIO 2 #########
##############################

scen2.data.miss <- list()
scen2.data.int.miss <- list()

## Set parameters

n <- 200                                   ## sample size
SigmaX <- matrix(0.5,4,4)                  ## covariance matrix for X1, X2, X3, X4
diag(SigmaX) <- 1                          ## covariance matrix for X1, X2, X3, X4
muX <- rep(0,4)                            ## mean vector for X1, X2, X3, X4
beta0 <- 0                                 ## true intercept  
beta1 <- beta2 <- beta3 <- beta4 <- 0.317  ## true main effects
betaint <- 0                               ## true interaction effect
R <- 10000                                 ## number of simulation repetitions/datasets simulated


for(i in 1:R){
  
  ## Generate complete data
  
  X <- rmvnorm(n, muX, SigmaX)
  x1 <- X[,1]
  x2 <- X[,2]
  x3 <- X[,3]
  x4 <- X[,4]
  epsilon <- rnorm(n, 0, 1)
  y <- beta0 + beta1*x1 + beta2*x2 + beta3*x3 + beta4*x4 + betaint*x1*x2 + epsilon 
  data_full <- data.frame(y,x1,x2,x3,x4)
  
  
  ## Induce missingness
  
  data_miss <- data_full
  data_miss$y[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$y[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  data_miss$x1[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$x1[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  data_miss$x2[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$x2[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  data_miss$x3[rbinom(n,1,0.35)==1 & data_miss$x4>=0] <- NA
  data_miss$x3[rbinom(n,1,0.65)==1 & data_miss$x4<0] <- NA
  scen2.data.miss[[i]] <- data_miss
  
  scen2.data.int.miss[[i]] <- data.frame(data_miss, data_miss$x1*data_miss$x2)
  colnames(scen2.data.int.miss[[i]]) <- c(colnames(data_miss),"int")
  
}

save(scen2.data.miss, file="scen2.data.miss.RData")
save(scen2.data.int.miss, file="scen2.data.int.miss.RData")

