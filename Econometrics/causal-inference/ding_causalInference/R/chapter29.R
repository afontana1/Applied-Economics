rm(list=ls())

EST = replicate(10^4, {
   n = 2000
   x0 = rnorm(n)
   e1 = 1/(1 + exp(-x0))
   z1 = rbinom(n, 1, e1)
   x1 = z1 + x0 + rnorm(n)
   e2 = 1/(1 + exp(-(-0.5 + z1 + x1 + 0.5*x0)))
   z2 = rbinom(n, 1, e2)
   y = z2 + z1 + x1 + x0 + rnorm(n)
   ## OLS estimation of (1,1) v.s. (0,0) 
   ## based on the formula in Example 29.1
   ols.y = lm(y ~ z2 + z1 + x1 + x0)$coef
   ols.x1 = lm(x1 ~ z1 + x0)$coef
   est1 = ols.y[2] + ols.y[3] + ols.y[4]*ols.x1[2]
   ## IPW - Hajek form
   e.z1 = glm(z1 ~ x0, family = binomial)$fitted.values
   e.z2 = glm(z2 ~ z1 + x1 + x0, family = binomial)$fitted.values
   mean11 = mean(z1*z2*y/e.z1/e.z2)/
              mean(z1*z2/e.z1/e.z2)
   mean00 = mean((1-z1)*(1-z2)*y/(1-e.z1)/(1-e.z2))/
              mean((1-z1)*(1-z2)/(1-e.z1)/(1-e.z2))
   est2 = mean11 - mean00
   
   c(est1, est2)
 })

apply(EST, 1, mean)
apply(EST, 1, sd)



EST = replicate(10^4, {
  n = 2000
  um = rnorm(n)
  x0 = rnorm(n)
  e1 = 1/(1 + exp(-x0))
  z1 = rbinom(n, 1, e1)
  x1 = z1 + x0 + um + rnorm(n)
  e2 = 1/(1 + exp(-(-0.5 + z1 + x1 + 0.5*x0)))
  z2 = rbinom(n, 1, e2)
  y = z2 + z1 + x1 + x0 + um +rnorm(n)
  ## OLS estimation of (1,1) v.s. (0,0) 
  ## based on the formula in Example 29.1
  ols.y = lm(y ~ z2 + z1 + x1 + x0)$coef
  ols.x1 = lm(x1 ~ z1 + x0)$coef
  est1 = ols.y[2] + ols.y[3] + ols.y[4]*ols.x1[2]
  ## IPW - Hajek form
  e.z1 = glm(z1 ~ x0, family = binomial)$fitted.values
  e.z2 = glm(z2 ~ z1 + x1 + x0, family = binomial)$fitted.values
  mean11 = mean(z1*z2*y/e.z1/e.z2)/
    mean(z1*z2/e.z1/e.z2)
  mean00 = mean((1-z1)*(1-z2)*y/(1-e.z1)/(1-e.z2))/
    mean((1-z1)*(1-z2)/(1-e.z1)/(1-e.z2))
  est2 = mean11 - mean00
  
  c(est1, est2)
})

apply(EST, 1, mean)
apply(EST, 1, sd)



