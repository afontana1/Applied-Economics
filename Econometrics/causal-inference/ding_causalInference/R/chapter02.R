rm(list=ls())

## potential outcomes and treatment assignment - Chapter 2.3
n  = 500
Y0 = rnorm(n)
tau = - 0.5 + Y0
Y1 = Y0 + tau
 
## perfect doctor 
Z = (tau >= 0)
Y = Z*Y1 + (1 - Z)*Y0
mean(Y[Z==1]) - mean(Y[Z==0])
 
## clueless doctor: randomized trial
Z = rbinom(n, 1, 0.5)
Y = Z*Y1 + (1 - Z)*Y0
mean(Y[Z==1]) - mean(Y[Z==0])
