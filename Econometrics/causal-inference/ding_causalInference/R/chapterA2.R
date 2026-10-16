rm(list=ls())

## EHW standard error - Chapter A2.4
library("car")
library("mlbench")
data(BostonHousing)
ols.fit = lm(medv ~ ., data = BostonHousing)
summary(ols.fit)

ols.fit.hc0 = sqrt(diag(hccm(ols.fit, type = "hc0")))
ols.fit.hc1 = sqrt(diag(hccm(ols.fit, type = "hc1")))
ols.fit.hc2 = sqrt(diag(hccm(ols.fit, type = "hc2")))
ols.fit.hc3 = sqrt(diag(hccm(ols.fit, type = "hc3")))
tvalues = summary(ols.fit)$coef[,1]/
  cbind(summary(ols.fit)$coef[,2], 
        ols.fit.hc0, 
        ols.fit.hc1, 
        ols.fit.hc2, 
        ols.fit.hc3)
colnames(tvalues) = c("ols", "hc0", "hc1", "hc2", "hc3")
round(tvalues, 2)


## logistic regression - Chapter A2.6.2
library(Matching)
data(lalonde)
logit.re78 = glm(I(re78>0) ~ ., family = binomial, 
                 data = lalonde)
summary(logit.re78)
