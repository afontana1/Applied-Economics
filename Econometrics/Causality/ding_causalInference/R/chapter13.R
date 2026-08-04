rm(list=ls())

## Chapter 13.3
ATT.est = function(z, y, x, out.family = gaussian, Utruncps = 1)
{
  ## sample size
  nn  = length(z)
  nn1 = sum(z)
  
  ## fitted propensity score
  pscore   = glm(z ~ x, family = binomial)$fitted.values
  pscore   = pmin(Utruncps, pscore)
  odds     = pscore/(1 - pscore)
  
  ## fitted potential outcomes
  outcome0 = glm(y ~ x, weights = (1 - z), 
                 family = out.family)$fitted.values
  
  ## outcome regression estimator
  ace.reg0 = lm(y ~ z + x)$coef[2]
  ace.reg  = mean(y[z==1]) - mean(outcome0[z==1]) 
  ## propensity score weighting estimator
  ace.ipw0 = mean(y[z==1]) - 
                mean(odds*(1 - z)*y)*nn/nn1
  ace.ipw  = mean(y[z==1]) - 
                mean(odds*(1 - z)*y)/mean(odds*(1 - z))
  ## doubly robust estimator
  res0     = y - outcome0
  ace.dr   = ace.reg - mean(odds*(1 - z)*res0)*nn/nn1
  
  return(c(ace.reg0, ace.reg, ace.ipw0, ace.ipw, ace.dr))     
}


OS_ATT = function(z, y, x, n.boot = 10^2,
                  out.family = gaussian, Utruncps = 1)
{
  point.est  = ATT.est(z, y, x, out.family, Utruncps)
  
  ## nonparametric bootstrap
  n   = length(z)
  x          = as.matrix(x)
  boot.est   = replicate(n.boot, {
    id.boot = sample(1:n, n, replace = TRUE)
    ATT.est(z[id.boot], y[id.boot], x[id.boot, ], 
            out.family, Utruncps)
  })
  
  boot.se    = apply(boot.est, 1, sd)
  
  res        = rbind(point.est, boot.se)
  rownames(res) = c("est", "se")
  colnames(res) = c("reg0", "reg", "HT", "Hajek", "DR")
  
  return(res)
}


## simulation: homework problem  

## an application: the NHANES BMI dataset
## Data "nhanes_bmi" from the "ATE" package in R
nhanes_bmi = read.csv("nhanes_bmi.csv")[, -1]
z = nhanes_bmi$School_meal
y = nhanes_bmi$BMI
x = as.matrix(nhanes_bmi[, -c(1, 2)])
x = scale(x)

ATTs = OS_ATT(z, y, x, n.boot = 10^3)
round(ATTs, 3)
 

## truncated propensity score
ATTs = OS_ATT(z, y, x, n.boot = 10^3, Utruncps = 0.9)
round(ATTs, 3)
 
