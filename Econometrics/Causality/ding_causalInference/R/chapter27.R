rm(list=ls())
library(parallel)
numCores = detectCores()


## Chapter 27.4.2
## Baron-Kenny method
library("car")
BKmediation = function(Z, M, Y, X)
{
  ## two regressions and coefficients
  mediator.reg   = lm(M ~ Z + X)
  mediator.Zcoef = mediator.reg$coef[2]
  mediator.Zse   = sqrt(hccm(mediator.reg)[2, 2])
  
  outcome.reg    = lm(Y ~ Z + M + X)
  outcome.Zcoef  = outcome.reg$coef[2]
  outcome.Zse    = sqrt(hccm(outcome.reg)[2, 2])
  outcome.Mcoef  = outcome.reg$coef[3]
  outcome.Mse    = sqrt(hccm(outcome.reg)[3, 3])
  
  ## Baron-Kenny point estimates
  NDE = outcome.Zcoef
  NIE = outcome.Mcoef*mediator.Zcoef
  
  ## Sobel's variance estimate based the delta method
  NDE.se = outcome.Zse
  NIE.se = sqrt(outcome.Mse^2*mediator.Zcoef^2 + 
                  outcome.Mcoef^2*mediator.Zse^2)
  
  res = matrix(c(NDE, NIE, 
                 NDE.se, NIE.se,
                 NDE/NDE.se, NIE/NIE.se), 
               2, 3)
  rownames(res) = c("NDE", "NIE")
  colnames(res) = c("est", "se", "t")
  
  res
}




## simulation showing that the estimate of the NIE may not be Normal
par(mfrow = c(3 ,2))
simulation11 = function(mc)
{
     n = 200
     Z = rbinom(n, 1, 0.5)
     X = rnorm(n)
     M = Z + X + rnorm(n)
     Y = Z + M + X + rnorm(n)
     
     BKmediation(Z, M, Y, X)
}

res11 = mclapply(1:1000, simulation11, mc.cores = numCores)
res11 = simplify2array(res11)
mean(res11[1, 1, ])
sd(res11[1, 1, ])
mean(res11[1, 2, ])
mean(res11[2, 1, ])
sd(res11[2, 1, ])
mean(res11[2, 2, ])
hist(res11[1, 1, ], xlab = "NDE", ylab = "density",
     main = "", freq = FALSE, breaks = 20)
hist(res11[2, 1, ], xlab = "NIE", ylab = "density",
     main = "", freq = FALSE, breaks = 20)
 

simulation10 = function(mc)
{
  n = 200
  Z = rbinom(n, 1, 0.5)
  X = rnorm(n)
  M = Z + X + rnorm(n)
  Y = Z + X + rnorm(n)
  
  BKmediation(Z, M, Y, X)
}

res10 = mclapply(1:1000, simulation10, mc.cores = numCores)
res10 = simplify2array(res10)
mean(res10[1, 1, ])
sd(res10[1, 1, ])
mean(res10[1, 2, ])
mean(res10[2, 1, ])
sd(res10[2, 1, ])
mean(res10[2, 2, ])
hist(res10[1, 1, ], xlab = "NDE", ylab = "density",
     main = "", freq = FALSE, breaks = 20)
hist(res10[2, 1, ], xlab = "NIE", ylab = "density",
     main = "", freq = FALSE, breaks = 20)
 

simulation00 = function(mc)
{
  n = 200
  Z = rbinom(n, 1, 0.5)
  X = rnorm(n)
  M = X + rnorm(n)
  Y = Z + X + rnorm(n)
  
  BKmediation(Z, M, Y, X)
}

res00 = mclapply(1:1000, simulation00, mc.cores = numCores)
res00 = simplify2array(res00)
mean(res00[1, 1, ])
sd(res00[1, 1, ])
mean(res00[1, 2, ])
mean(res00[2, 1, ])
sd(res00[2, 1, ])
mean(res00[2, 2, ])
hist(res00[1, 1, ], xlab = "NDE", ylab = "density",
     main = "", freq = FALSE, breaks = 20)
hist(res00[2, 1, ], xlab = "NIE", ylab = "density",
     main = "", freq = FALSE, breaks = 20)




## jobs data
jobsdata = read.csv("jobsdata.csv")
Z = jobsdata$treat
M = jobsdata$job_seek
Y = jobsdata$depress2
getX     = lm(treat ~ econ_hard + depress1 + 
                sex + age + occp + marital + 
                nonwhite + educ + income,
              data = jobsdata)
X = model.matrix(getX)[, -1]
res = BKmediation(Z, M, Y, X)
round(res, 3)
 