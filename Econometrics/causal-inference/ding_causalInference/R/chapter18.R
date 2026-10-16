rm(list=ls())

## Chapter 18.3.2
OS_est_sa = function(z, y, x, out.family = gaussian, 
                     truncps = c(0, 1), e1 = 1, e0 = 1)
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
     ace.reg  = mean(z*y) + mean((1-z)*outcome1/e1) - 
                   mean(z*outcome0*e0) - mean((1-z)*y) 
     ## IPW estimators
     w1 = pscore + (1-pscore)/e1
     w0 = pscore*e0 + (1-pscore)
     ace.ipw0 = mean(z*y*w1/pscore) - 
                   mean((1 - z)*y*w0/(1 - pscore))
     ace.ipw  = mean(z*y*w1/pscore)/mean(z/pscore) - 
                   mean((1 - z)*y*w0/(1 - pscore))/mean((1 - z)/(1 - pscore))
     ## doubly robust estimator
     aug = outcome1/pscore/e1 + outcome0*e0/(1-pscore)
     ace.dr   = ace.ipw0 + mean((z-pscore)*aug)

     return(c(ace.reg, ace.ipw0, ace.ipw, ace.dr))     
}



## an application: the NHANES BMI dataset
nhanes_bmi = read.csv("nhanes_bmi.csv")[, -1]
z = nhanes_bmi$School_meal
y = nhanes_bmi$BMI
x = as.matrix(nhanes_bmi[, -c(1, 2)])
x = scale(x)


E1 = c(1/2, 1/1.7, 1/1.5, 1/1.3, 1, 1.3, 1.5, 1.7, 2)
E0 = c(1/2, 1/1.7, 1/1.5, 1/1.3, 1, 1.3, 1.5, 1.7, 2)
EST = outer(E1, E0)
ll1 = length(E1)
ll0 = length(E0)
for(i in 1:ll1)
  for(j in 1:ll0)
    EST[i, j] = OS_est_sa(z, y, x, e1 = E1[i], e0 = E0[j])[4]


Names = c("1/2", "1/1.7", "1/1.5", "1/1.3", "1", 
          "1.3", "1.5", "1.7", "2")
row.names(EST) = Names
colnames(EST)  = Names
round(EST, 2)

library(xtable)
xtable(EST)
