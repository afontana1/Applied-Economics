rm(list=ls())


## Chapter 23.7
library("car")
## Card Data
card.data = read.csv("card1995.csv")
Y = card.data[, "lwage"]
D = card.data[, "educ"]
Z = card.data[, "nearc4"]
X = card.data[, c("exper", "expersq", "black", "south", 
                  "smsa", "reg661", "reg662", "reg663", 
                  "reg664", "reg665", "reg666", 
                  "reg667", "reg668", "smsa66")]
X = as.matrix(X)

## TSLS
Dhat    = lm(D ~ Z + X)$fitted.values
tslsreg = lm(Y ~ Dhat + X)
tslsest = coef(tslsreg)[2]
## correct se by changing the residuals
res.correct       = Y - cbind(1, D, X)%*%coef(tslsreg)
tslsreg$residuals = as.vector(res.correct)
tslsse = sqrt(hccm(tslsreg, type = "hc0")[2, 2])
res = c(tslsest, tslsest - 1.96*tslsse, tslsest + 1.96*tslsse)
names(res) = c("TSLS", "lower CI", "upper CI")
round(res, 3)


## FAR confidence set
BetaAR   = seq(-0.1, 0.4, 0.001)
PvalueAR = sapply(BetaAR, function(b){
  Y_b   = Y - b*D
  ARreg = lm(Y_b ~ Z + X)
  coefZ = coef(ARreg)[2]
  seZ   = sqrt(hccm(ARreg)[2, 2])
  Tstat = coefZ/seZ
  (1 - pnorm(abs(Tstat)))*2
})

pdf("FAR_Carddata.pdf", height = 5, width = 8.5)
plot(PvalueAR ~ BetaAR, type = "l",
     xlab = "coefficient of D",
     ylab = "p-value",
     main = "Fieller-Anderson-Rubin interval based on Card's data")
point.est = BetaAR[which.max(PvalueAR)]
abline(h = 0.05, lty = 2, col = "grey")
abline(v = point.est, lty = 2, col = "grey")
ARCI = range(BetaAR[PvalueAR >= 0.05])
abline(v = ARCI[1], lty = 2, col = "grey")
abline(v = ARCI[2], lty = 2, col = "grey")
## FAR results
FARres = c(point.est, ARCI)
names(FARres) = c("FAR est", "lower CI", "upper CI")
round(FARres, 3)
dev.off()



## Problem: Efron-Feldman data
## The first column is treatment assignment (control=0, treatment=1). 
## The second column is how much they took of the assigned dose, out of 100 (there are some values in the data like 101, but that's a bug). 
## EF computed the outcome difference as 0.25*C3 + 0.75*C4 - C5. 
## See the book for more details. 
dat_ef = read.csv("EF.csv", header = FALSE)
