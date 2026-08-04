rm(list=ls())
#code to replicate analysis of Lalonde data - Chapter 1.2.1
dat <- read.table("cps1re74.csv", header = TRUE)
dat$u74 <- as.numeric(dat$re74==0)
dat$u75 <- as.numeric(dat$re75==0)

## linear regression on the outcome
## . means regression on all other variable in dat
lmoutcome = lm(re78 ~ ., data = dat)
round(summary(lmoutcome)$coef[2, ], 3)

lmoutcome = lm(re78 ~ treat, data = dat)
round(summary(lmoutcome)$coef[2, ], 3)

## reanalysis of the resume experiment - Chapter 1.2.2
resume = read.csv("resume.csv")
Alltable = table(resume$race, resume$call)
Alltable
fisher.test(Alltable)

## reanalysis of the UCB admission data - Chapter 1.4
library(datasets)
UCBAdmissions = aperm(UCBAdmissions, c(2, 1, 3))
UCBAdmissions
UCBAdmissions.sum = apply(UCBAdmissions, c(1, 2), sum)
UCBAdmissions.sum

## key function
risk.difference = function(tb2)
{
  p1      = tb2[1, 1]/(tb2[1, 1] + tb2[1, 2])
  p2      = tb2[2, 1]/(tb2[2, 1] + tb2[2, 2])
  testp   = chisq.test(tb2)
  
  return(list(p.diff = p1 - p2,  
              pv = testp$p.value))	    
}

risk.difference(UCBAdmissions.sum)

P.diff = rep(0, 6)
PV     = rep(0, 6)
for(dd in 1:6)
{
  department = risk.difference(UCBAdmissions[, , dd])
  P.diff[dd] = department$p.diff
  PV[dd]     = department$pv 
}

round(P.diff, 2)
round(PV, 2)

