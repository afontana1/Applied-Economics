rm(list=ls())

## for parallel computing
library("car")
library(parallel)
numCores = detectCores()


## Chapter 21.2.2
## IV point estimator
IV_Wald = function(Z, D, Y)
{
       tau_D = mean(D[Z==1]) - mean(D[Z==0])
       tau_Y = mean(Y[Z==1]) - mean(Y[Z==0])
       CACE  = tau_Y/tau_D
       
       c(tau_D, tau_Y, CACE)
}

## IV se via the delta method
IV_Wald_delta = function(Z, D, Y)
{
       est         = IV_Wald(Z, D, Y)
       AdjustedY   = Y - D*est[3]
       VarAdj      = var(AdjustedY[Z==1])/sum(Z) + 
                          var(AdjustedY[Z==0])/sum(1 - Z)
       
       c(est[3], sqrt(VarAdj)/abs(est[1]))
}

##IV se via the bootstrap
IV_Wald_bootstrap = function(Z, D, Y, n.boot = 200)
{
       est      = IV_Wald(Z, D, Y)
       
       CACEboot  = replicate(n.boot, {
         id.boot = sample(1:length(Z), replace = TRUE)
         IV_Wald(Z[id.boot], D[id.boot], Y[id.boot])[3]
       })
       
       c(est[3], sd(CACEboot))
}

## Chapter 21.3.1
## covariate adjustment in IV analysis
IV_Lin = function(Z, D, Y, X)
{
  X     = scale(as.matrix(X))
  tau_D = lm(D ~ Z + X + Z*X)$coef[2]
  tau_Y = lm(Y ~ Z + X + Z*X)$coef[2]
  CACE  = tau_Y/tau_D
  
  c(tau_D, tau_Y, CACE)
}


##IV_adj se via the bootstrap
IV_Lin_bootstrap = function(Z, D, Y, X, n.boot = 200)
{
  X         = scale(as.matrix(X))
  est       = IV_Lin(Z, D, Y, X)
  CACEboot  = replicate(n.boot, {
    id.boot = sample(1:length(Z), replace = TRUE)
    IV_Lin(Z[id.boot], D[id.boot], Y[id.boot], X[id.boot, ])[3]
  })
  
  c(est[3], sd(CACEboot))
}

## Chapter 21.3.2
## R code relegated to a homework problem 


## Chapter 21.4.1
## without covariates 
MC       = 2000
pdf("wald_simulation.pdf", height = 4, width = 8.5)
par(mfrow = c(1, 3), mai = c(0.8, 0.3, 0.3, 0.3))
## strong IV simulation
strongivsim = function(mc)
{
  n  = 200
  ## "c" n/2; "a" n/4; "n" n/4
  D0 = c(rep(0, n/2), rep(1, n/4), rep(0, n/4))
  D1 = c(rep(1, n/2), rep(1, n/4), rep(0, n/4))
  Y0 = c(rnorm(n/2, 1), rnorm(n/4, 0), rnorm(n/4, 2))
  Y1 = Y0
  Y1[1:(n/2)] = rnorm(n/2, 3)
  Z  = rbinom(n, 1, 0.5)
  D  = Z*D1 + (1 - Z)*D0
  Y  = Z*Y1 + (1 - Z)*Y0
  
  c(IV_Wald(Z, D, Y)[3],
    IV_Wald_delta(Z, D, Y)[2],
    IV_Wald_bootstrap(Z, D, Y)[2])
}

strongivres = mclapply(1:MC, strongivsim, mc.cores = numCores)
strongivres = do.call(rbind, strongivres)

mean(strongivres[, 1])
sd(strongivres[, 1])
mean(strongivres[, 2])
mean(strongivres[, 3])

hist(strongivres[, 1], breaks = 15, freq = FALSE,
     xlab = expression(hat(tau)[c]), ylab = "",
     main = "strong IV: pi_c = 0.5",      
     border = FALSE, col = "grey")
abline(v = 2)
x = seq(0, 5, 0.01)
y = dnorm(x, mean(strongivres[, 1]), sd(strongivres[, 1]))
lines(y ~ x)



## weak IV
weakivsim = function(mc)
{
  n  = 200
  ## "c" n/5; "a" n*2/5; "n" n*2/5
  D0 = c(rep(0, n/5), rep(1, n*2/5), rep(0, n*2/5))
  D1 = c(rep(1, n/5), rep(1, n*2/5), rep(0, n*2/5))
  Y0 = c(rnorm(n/5, 1), rnorm(n*2/5, 0), rnorm(n*2/5, 2))
  Y1 = Y0
  Y1[1:(n/5)] = rnorm(n/5, 3)
  Z  = rbinom(n, 1, 0.5)
  D  = Z*D1 + (1 - Z)*D0
  Y  = Z*Y1 + (1 - Z)*Y0
  
  c(IV_Wald(Z, D, Y)[3],
    IV_Wald_delta(Z, D, Y)[2],
    IV_Wald_bootstrap(Z, D, Y)[2])
}

weakivres = mclapply(1:MC, weakivsim, mc.cores = numCores)
weakivres = do.call(rbind, weakivres)

mean(weakivres[, 1])
sd(weakivres[, 1])
mean(weakivres[, 2])
mean(weakivres[, 3])

hist(weakivres[, 1], breaks = 30, freq = FALSE,
     xlab = expression(hat(tau)[c]), ylab = "",
     main = "weak IV: pi_c = 0.2",      
     border = FALSE, col = "grey")
abline(v = 2)
x = seq(-40, 40, 0.01)
y = dnorm(x, mean(weakivres[, 1]), sd(weakivres[, 1]))
lines(y ~ x)


## weak IV
weakivsim = function(mc)
{
  n  = 200
  ## "c" n/10; "a" n*2/5; "n" n/2
  D0 = c(rep(0, n/10), rep(1, n*2/5), rep(0, n/2))
  D1 = c(rep(1, n/10), rep(1, n*2/5), rep(0, n/2))
  Y0 = c(rnorm(n/10, 1), rnorm(n*2/5, 0), rnorm(n/2, 2))
  Y1 = Y0
  Y1[1:(n/10)] = rnorm(n/10, 3)
  Z  = rbinom(n, 1, 0.5)
  D  = Z*D1 + (1 - Z)*D0
  Y  = Z*Y1 + (1 - Z)*Y0
  
  c(IV_Wald(Z, D, Y)[3],
    IV_Wald_delta(Z, D, Y)[2],
    IV_Wald_bootstrap(Z, D, Y)[2])
}

weakivres = mclapply(1:MC, weakivsim, mc.cores = numCores)
weakivres = do.call(rbind, weakivres)

mean(weakivres[, 1])
sd(weakivres[, 1])
mean(weakivres[, 2])
mean(weakivres[, 3])


hist(weakivres[, 1], breaks = 30, freq = FALSE,
     xlab = expression(hat(tau)[c]), ylab = "",
     main = "weak IV: pi_c = 0.1",      
     border = FALSE, col = "grey")
abline(v = 2)
x = seq(-400, 600, 1)
y = dnorm(x, mean(weakivres[, 1]), sd(weakivres[, 1]))
lines(y ~ x)
dev.off()






## with covariates 
pdf("waldX_simulation.pdf", height = 4, width = 8.5)
par(mfrow = c(1, 3), mai = c(0.8, 0.3, 0.3, 0.3))
MC       = 200
## strong IV simulation
strongivsim = function(mc)
{
  n  = 200
  X  = matrix(rnorm(n*2), n, 2)
  ## "c" n/2; "a" n/4; "n" n/4
  D0 = c(rep(0, n/2), rep(1, n/4), rep(0, n/4))
  D1 = c(rep(1, n/2), rep(1, n/4), rep(0, n/4))
  Y0 = c(rnorm(n/2, 1), rnorm(n/4, 0), rnorm(n/4, 2)) + 
    X%*%c(1, -1)
  Y1 = Y0
  Y1[1:(n/2)] = rnorm(n/2, 3) + X[1:(n/2), ]%*%c(-1, 1)
  Z  = rbinom(n, 1, 0.5)
  D  = Z*D1 + (1 - Z)*D0
  Y  = Z*Y1 + (1 - Z)*Y0
  
  IV_Lin_bootstrap(Z, D, Y, X)
}

strongivres = mclapply(1:MC, strongivsim, mc.cores = numCores)
strongivres = do.call(rbind, strongivres)

mean(strongivres[, 1])
sd(strongivres[, 1])
mean(strongivres[, 2])


hist(strongivres[, 1], breaks = 10, freq = FALSE,
     xlab = expression(hat(tau)[c]), ylab = "",
     main = "strong IV: pi_c = 0.5",      
     border = FALSE, col = "grey")
abline(v = 2)
x = seq(0, 5, 0.01)
y = dnorm(x, mean(strongivres[, 1]), sd(strongivres[, 1]))
lines(y ~ x)



## weak IV
weakivsim = function(mc)
{
  n  = 200
  X  = matrix(rnorm(n*2), n, 2)
  ## "c" n/5; "a" n*2/5; "n" n*@/5
  D0 = c(rep(0, n/5), rep(1, n*2/5), rep(0, n*2/5))
  D1 = c(rep(1, n/5), rep(1, n*2/5), rep(0, n*2/5))
  Y0 = c(rnorm(n/5, 1), rnorm(n*2/5, 0), rnorm(n*2/5, 2)) + 
    X%*%c(1, -1)
  Y1 = Y0
  Y1[1:(n/5)] = rnorm(n/5, 3) + X[1:(n/5), ]%*%c(-1, 1)
  Z  = rbinom(n, 1, 0.5)
  D  = Z*D1 + (1 - Z)*D0
  Y  = Z*Y1 + (1 - Z)*Y0
  
  IV_Lin_bootstrap(Z, D, Y, X)
}

weakivres = mclapply(1:MC, weakivsim, mc.cores = numCores)
weakivres = do.call(rbind, weakivres)

mean(weakivres[, 1])
sd(weakivres[, 1])
mean(weakivres[, 2])
 

hist(weakivres[, 1], breaks = 20, freq = FALSE,
     xlab = expression(hat(tau)[c]), ylab = "",
     main = "weak IV: pi_c = 0.2",      
     border = FALSE, col = "grey")
abline(v = 2)
x = seq(0, 10, 0.01)
y = dnorm(x, mean(weakivres[, 1]), sd(weakivres[, 1]))
lines(y ~ x)


## weak IV
weakivsim = function(mc)
{
  n  = 200
  X  = matrix(rnorm(n*2), n, 2)
  ## "c" n/10; "a" n*2/5; "n" n/2
  D0 = c(rep(0, n/10), rep(1, n*2/5), rep(0, n/2))
  D1 = c(rep(1, n/10), rep(1, n*2/5), rep(0, n/2))
  Y0 = c(rnorm(n/10, 1), rnorm(n*2/5, 0), rnorm(n/2, 2)) + 
    X%*%c(1, -1)
  Y1 = Y0
  Y1[1:(n/10)] = rnorm(n/10, 3) + X[1:(n/10), ]%*%c(-1, 1)
  Z  = rbinom(n, 1, 0.5)
  D  = Z*D1 + (1 - Z)*D0
  Y  = Z*Y1 + (1 - Z)*Y0
  
  IV_Lin_bootstrap(Z, D, Y, X)
}

weakivres = mclapply(1:MC, weakivsim, mc.cores = numCores)
weakivres = do.call(rbind, weakivres)

mean(weakivres[, 1])
sd(weakivres[, 1])
mean(weakivres[, 2])
 
hist(weakivres[, 1], breaks = 15, freq = FALSE,
     xlab = expression(hat(tau)[c]), ylab = "",
     main = "weak IV: pi_c = 0.1",      
     border = FALSE, col = "grey")
abline(v = 2)
x = seq(-600, 600, 1)
y = dnorm(x, mean(weakivres[, 1]), sd(weakivres[, 1]))
lines(y ~ x)
dev.off()



 





## Chapter 21.4.3
## Fieller-Anderson-Rubin confidence interval
## without covariates
FARci = function(Z, D, Y, Lower, Upper, grid)
{
  CIrange = seq(Lower, Upper, grid)
  Pvalue  = sapply(CIrange, function(t){
    Y_t       = Y - t*D
    Tauadj    = mean(Y_t[Z==1]) - mean(Y_t[Z==0])
    VarAdj    = var(Y_t[Z==1])/sum(Z) + 
      var(Y_t[Z==0])/sum(1 - Z)
    Tstat     = Tauadj/sqrt(VarAdj)
    (1 - pnorm(abs(Tstat)))*2
  })
  
  return(list(CIrange = CIrange, Pvalue  = Pvalue))
}

## super population Lin estimator with corrected EHW - from Chapter 9
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

FARciX = function(Z, D, Y, X, Lower, Upper, grid)
{
  CIrange = seq(Lower, Upper, grid)
  X       = scale(X)
  Pvalue  = sapply(CIrange, function(t){
    Y_t       = Y - t*D
    linest    = linestimator(Z, Y_t, X)
    Tstat     = linest[1]/linest[3]
    (1 - pnorm(abs(Tstat)))*2
  })
  
  return(list(CIrange = CIrange, Pvalue  = Pvalue))
}


pdf("FARci_simulation.pdf", height = 8, width = 4)
par(mfrow = c(3, 1), mai = c(0.8, 0.3, 0.3, 0.3))
## FAR confidence interval for strong IV
## "c" n/2; "a" n/4; "n" n/4
n  = 200
D0 = c(rep(0, n/2), rep(1, n/4), rep(0, n/4))
D1 = c(rep(1, n/2), rep(1, n/4), rep(0, n/4))
Y0 = c(rnorm(n/2, 1), rnorm(n/4, 0), rnorm(n/4, 2))
Y1 = Y0
Y1[1:(n/2)] = rnorm(n/2, 3)
Z  = rbinom(n, 1, 0.5)
D  = Z*D1 + (1 - Z)*D0
Y  = Z*Y1 + (1 - Z)*Y0

FARplot = FARci(Z, D, Y, Lower = 0, Upper = 5, grid = 0.001)
plot(FARplot$Pvalue ~ FARplot$CIrange,
     type = "l", 
     xlab = expression(tau[c]), 
     ylab = "p-value",
     main = "strong IV: pi_c = 0.5")
abline(h = 0.05, lty = 2)


## FAR confidence interval for weak IV
## "c" n/5; "a" n*2/5; "n" n*2/5
n  = 200
D0 = c(rep(0, n/5), rep(1, n*2/5), rep(0, n*2/5))
D1 = c(rep(1, n/5), rep(1, n*2/5), rep(0, n*2/5))
Y0 = c(rnorm(n/5, 1), rnorm(n*2/5, 0), rnorm(n*2/5, 2))
Y1 = Y0
Y1[1:(n/5)] = rnorm(n/5, 3)
Z  = rbinom(n, 1, 0.5)
D  = Z*D1 + (1 - Z)*D0
Y  = Z*Y1 + (1 - Z)*Y0

FARplot = FARci(Z, D, Y, Lower = -4, Upper = 30, grid = 0.001)
plot(FARplot$Pvalue ~ FARplot$CIrange,
     type = "l", 
     xlab = expression(tau[c]), 
     ylab = "p-value",
     main = "weak IV: pi_c = 0.2")
abline(h = 0.05, lty = 2)



## FAR confidence interval for weak IV
## "c" n/10; "a" n*2/5; "n" n/2
n  = 200
D0 = c(rep(0, n/10), rep(1, n*2/5), rep(0, n/2))
D1 = c(rep(1, n/10), rep(1, n*2/5), rep(0, n/2))
Y0 = c(rnorm(n/10, 1), rnorm(n*2/5, 0), rnorm(n/2, 2))
Y1 = Y0
Y1[1:(n/10)] = rnorm(n/10, 3)
Z  = rbinom(n, 1, 0.5)
D  = Z*D1 + (1 - Z)*D0
Y  = Z*Y1 + (1 - Z)*Y0

FARplot = FARci(Z, D, Y, Lower = -10, Upper = 25, grid = 0.001)
plot(FARplot$Pvalue ~ FARplot$CIrange,
     type = "l", 
     xlab = expression(tau[c]), 
     ylab = "p-value",
     main = "weak IV: pi_c = 0.1")
abline(h = 0.05, lty = 2)
dev.off()



## Chapter 21.5 
## jobs data in "mediation package"
## one sided noncompliance: D(0) = 0
jobsdata = read.csv("jobsdata.csv")
Z = jobsdata$treat
D = jobsdata$comply
Y = jobsdata$job_seek
getX     = lm(treat ~ sex + age + marital 
              + nonwhite + educ + income,
              data = jobsdata)
X = model.matrix(getX)[, -1]
## without covariates
res = rbind(IV_Wald_delta(Z, D, Y),
            IV_Wald_bootstrap(Z, D, Y, n.boot = 10^3))
## with covariates 
res = rbind(res, 
            IV_Lin_bootstrap(Z, D, Y, X, n.boot = 10^3)) 
res = cbind(res, res[, 1] - 1.96*res[, 2],
            res[, 1] + 1.96*res[, 2])
row.names(res) = c("delta", "bootstrap", "with covariates")
colnames(res)  = c("est", "se", "lower CI", "upper CI")
round(res, 3)


## FAR CI
pdf("farci_jobs.pdf", height = 8.5, width = 8.5)
par(mfrow = c(2, 1), mai = c(0.8, 0.8, 0.3, 0.3))
FARplot = FARci(Z, D, Y, Lower = -0.2, Upper = 0.4, grid = 0.001)
plot(FARplot$Pvalue ~ FARplot$CIrange,
     type = "l", 
     xlab = expression(tau[c]), 
     ylab = "p-value",
     main = "without covariates")
abline(h = 0.05, lty = 2)  
ARCI = range(FARplot$CIrange[FARplot$Pvalue >= 0.05])

FARplot = FARciX(Z, D, Y, X, Lower = -0.2, Upper = 0.4, grid = 0.001)
plot(FARplot$Pvalue ~ FARplot$CIrange,
     type = "l", 
     xlab = expression(tau[c]), 
     ylab = "p-value",
     main = "with covariates")
abline(h = 0.05, lty = 2)  
ARCI.x = range(FARplot$CIrange[FARplot$Pvalue >= 0.05])

ARCIs = rbind(ARCI, ARCI.x)
row.names(ARCIs) = c("without covariates", "with covariates")
colnames(ARCIs) = c("lower CI", "upper CI")
round(ARCIs, 3)
dev.off()


