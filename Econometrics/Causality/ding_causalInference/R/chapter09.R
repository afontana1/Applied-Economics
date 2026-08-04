rm(list=ls())

## Chapter 9.2
## Lin's estimator with EHW and super population variance estimators
library("car")
linestimator = function(Z, Y, X){
  ## standardize X
  X      = scale(X)
  n      = dim(X)[1]
  p      = dim(X)[2]
  
  ## fully interacted OLS
  linreg = lm(Y ~ Z*X)
  est    = coef(linreg)[2]
  vehw   = hccm(linreg)[2, 2]
  
  ## super population correction
  inter  = coef(linreg)[(p+3):(2*p+2)]
  vsuper = vehw + sum(inter*(cov(X)%*%inter))/n
  
  c(est, sqrt(vehw), sqrt(vsuper))
}


## simulation with true effect 0
res = replicate(2000, {
  n  = 500
  X  = matrix(rnorm(n*2), n, 2)
  Y1 = X[, 1] + X[, 1]^2 + runif(n, -0.5, 0.5)
  Y0 = X[, 2] + X[, 2]^2 + runif(n, -1, 1)
  Z  = rbinom(n, 1, 0.6)
  Y  = Z*Y1 + (1-Z)*Y0
  linestimator(Z, Y, X)
})
## bias
mean(res[1, ])
## empirical standard deviation
sd(res[1, ])
## estimated EHW standard error
mean(res[2, ])
## coverage based on EHW standard error
mean((res[1, ]-1.96*res[2, ])*(res[1, ]+1.96*res[2, ])<=0)
## estimated super population standard error
mean(res[3, ])
## coverage based on population standard error
mean((res[1, ]-1.96*res[3, ])*(res[1, ]+1.96*res[3, ])<=0)
