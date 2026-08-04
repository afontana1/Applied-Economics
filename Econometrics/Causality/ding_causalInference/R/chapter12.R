rm(list=ls())

## Chapter 12.3.1
OS_est = function(z, y, x, out.family = gaussian, 
                  truncps = c(0, 1))
{
     ## fitted propensity score
     pscore   = glm(z ~ x, family = binomial)$fitted.values
     pscore   = pmax(truncps[1], pmin(truncps[2], pscore))
     
     ## fitted potential outcomes
     outcome1 = glm(y ~ x, weights = z, 
                    family = out.family)$fitted.values
     outcome0 = glm(y ~ x, weights = (1 - z), 
                    family = out.family)$fitted.values
     
     ## outcome regression estimator
     ace.reg  = mean(outcome1 - outcome0) 
     ## IPW estimators
     y.treat     = mean(z*y/pscore)
     y.control   = mean((1 - z)*y/(1 - pscore))
     one.treat   = mean(z/pscore)
     one.control = mean((1 - z)/(1 - pscore))
     ace.ipw0    = y.treat - y.control
     ace.ipw     = y.treat/one.treat - y.control/one.control
     ## doubly robust estimator
     res1      = y - outcome1
     res0      = y - outcome0
     r.treat   = mean(z*res1/pscore)
     r.control = mean((1 - z)*res0/(1 - pscore))
     ace.dr    = ace.reg + r.treat - r.control

     return(c(ace.reg, ace.ipw0, ace.ipw, ace.dr))     
}


OS_ATE = function(z, y, x, n.boot = 2*10^2,
                     out.family = gaussian, truncps = c(0, 1))
{
     point.est  = OS_est(z, y, x, out.family, truncps)
     
     ## nonparametric bootstrap
     n          = length(z)
     x          = as.matrix(x)
     boot.est   = replicate(n.boot, {
       id.boot = sample(1:n, n, replace = TRUE)
       OS_est(z[id.boot], y[id.boot], x[id.boot, ], 
              out.family, truncps)
     })

     boot.se    = apply(boot.est, 1, sd)
     
     res        = rbind(point.est, boot.se)
     rownames(res) = c("est", "se")
     colnames(res) = c("reg", "HT", "Hajek", "DR")
     
     return(res)
}



## Chapter 12.3.2
library(parallel)
numCores = detectCores()
n.sim   = 500
n       = 500

## simulation with correct models
simu.11 = function(mc)
{
  x       = matrix(rnorm(n*2), n, 2)
  x1      = cbind(1, x)
  beta.z  = c(0, 1, 1)
  pscore  = 1/(1 + exp(- as.vector(x1%*%beta.z)))
  z       = rbinom(n, 1, pscore)
  beta.y1 = c(1, 2, 1)
  beta.y0 = c(1, 2, 1)
  y1      = rnorm(n, x1%*%beta.y1)
  y0      = rnorm(n, x1%*%beta.y0)
  y       = z*y1 + (1 - z)*y0
  ce      = OS_ATE(z, y, x)
  
  ## true ACE, est, se
  c(mean(y1 - y0), ce[1, ], ce[2, ])
}




## simulation with an incorrect propensity score model
simu.01 = function(mc)
{
  x       = matrix(rnorm(n*2), n, 2)
  x1      = cbind(1, x, exp(x))
  beta.z  = c(-1, 0, 0, 1, -1)
  pscore  = 1/(1 + exp(- as.vector(x1%*%beta.z)))
  z       = rbinom(n, 1, pscore)
  beta.y1 = c(1, 2, 1, 0, 0)
  beta.y0 = c(1, 1, 1, 0, 0)
  y1      = rnorm(n, x1%*%beta.y1)
  y0      = rnorm(n, x1%*%beta.y0)
  y       = z*y1 + (1 - z)*y0
  ce      = OS_ATE(z, y, x)
  
  ## true ACE, est, se
  c(mean(y1 - y0), ce[1, ], ce[2, ])
}



## simulation with an incorrect outcome model
simu.10 = function(mc)
{
  x       = matrix(rnorm(n*2), n, 2)
  x1      = cbind(1, x, exp(x))
  beta.z  = c(0, 1, 1, 0, 0)
  pscore  = 1/(1 + exp(- as.vector(x1%*%beta.z)))
  z       = rbinom(n, 1, pscore)
  beta.y1 = c(1, 0, 0, 0.2, -0.1)
  beta.y0 = c(1, 0, 0, -0.2, 0.1)
  y1      = rnorm(n, x1%*%beta.y1)
  y0      = rnorm(n, x1%*%beta.y0)
  y       = z*y1 + (1 - z)*y0
  ce      = OS_ATE(z, y, x)
  
  ## true ACE, est, se
  c(mean(y1 - y0), ce[1, ], ce[2, ])
}


## simulation without correct models
simu.00 = function(mc)
{
  x       = matrix(rnorm(n*2), n, 2)
  x1      = cbind(1, x, exp(x))
  beta.z  = c(-1, 0, 0, 1, -1)
  pscore  = 1/(1 + exp(- as.vector(x1%*%beta.z)))
  z       = rbinom(n, 1, pscore)
  beta.y1 = c(1, 0, 0, 0.2, -0.1)
  beta.y0 = c(1, 0, 0, -0.2, 0.1)
  y1      = rnorm(n, x1%*%beta.y1)
  y0      = rnorm(n, x1%*%beta.y0)
  y       = z*y1 + (1 - z)*y0
  ce      = OS_ATE(z, y, x)
  
  ## true ACE, est, se
  c(mean(y1 - y0), ce[1, ], ce[2, ])
}



## simulations 
res = mclapply(1:n.sim, simu.11, mc.cores = numCores)
res = simplify2array(res)

bias = res[2:5, ] - 0
res = rbind(apply(bias, 1, mean),
            apply(bias, 1, sd),
            apply(res[6:9, ], 1, mean))
row.names(res) = c("ave.bias", "true.se", "est.se")
round(res, 2)

res = mclapply(1:n.sim, simu.01, mc.cores = numCores)
res = simplify2array(res)

bias = res[2:5, ] - 0
res = rbind(apply(bias, 1, mean),
            apply(bias, 1, sd),
            apply(res[6:9, ], 1, mean))
row.names(res) = c("ave.bias", "true.se", "est.se")
round(res, 2)



res = mclapply(1:n.sim, simu.10, mc.cores = numCores)
res = simplify2array(res)

bias = res[2:5, ] - 0.2*exp(1/2)
res = rbind(apply(bias, 1, mean),
            apply(bias, 1, sd),
            apply(res[6:9, ], 1, mean))
row.names(res) = c("ave.bias", "true.se", "est.se")
round(res, 2)


res = mclapply(1:n.sim, simu.00, mc.cores = numCores)
res = simplify2array(res)

bias = res[2:5, ] - 0.2*exp(1/2)
res = rbind(apply(bias, 1, mean),
            apply(bias, 1, sd),
            apply(res[6:9, ], 1, mean))
row.names(res) = c("ave.bias", "true.se", "est.se")
round(res, 2)

 
## Chapter 12.3.3
## Data "nhanes_bmi" from the "ATE" package in R
nhanes_bmi = read.csv("nhanes_bmi.csv")[, -1]
z = nhanes_bmi$School_meal
y = nhanes_bmi$BMI
x = as.matrix(nhanes_bmi[, -c(1, 2)])
x = scale(x)

causaleffects = OS_ATE(z, y, x, n.boot = 10^3)
round(causaleffects, 3)
rbind(causaleffects[1, ] - 1.96*causaleffects[2, ],
      causaleffects[1, ] + 1.96*causaleffects[2, ])

## checking the data
pscore   = glm(z ~ x, family = binomial)$fitted.values
hist(pscore[z==1], col="grey", border = NA, freq = FALSE,
     ylim = c(0, 4.5), breaks = 30, main = "",
     xlab = expression(hat(e)(X)), ylab = "")
hist(pscore[z==0], add= T, freq = FALSE, breaks = 30)

## truncated propensity score
causaleffects = OS_ATE(z, y, x, n.boot = 10^3,
                          truncps = c(0.1, 0.9))
round(causaleffects, 3)
rbind(causaleffects[1, ] - 1.96*causaleffects[2, ],
      causaleffects[1, ] + 1.96*causaleffects[2, ])

