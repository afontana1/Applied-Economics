rm(list=ls())

## Chapter 16.3.1
## M bias with large sample size
n  = 10^6
U1 = rnorm(n)
U2 = rnorm(n)
X  = U1 + U2 + rnorm(n)
Y  = U2 + rnorm(n)
## with a continuous treatment Z
Z  = U1 + rnorm(n)
round(summary(lm(Y ~ Z))$coef[2, 1], 3)
round(summary(lm(Y ~ Z + X))$coef[2, 1], 3)

## with a binary treatment Z
Z  = (Z >= 0)
round(summary(lm(Y ~ Z))$coef[2, 1], 3)
round(summary(lm(Y ~ Z + X))$coef[2, 1], 3)



## Chapter 16.3.2
## Z bias with large sample size
n  = 10^6
X = rnorm(n)
U = rnorm(n)
Z = X + U + rnorm(n)
Y = U + rnorm(n)

round(summary(lm(Y ~ Z))$coef[2, 1], 3)
round(summary(lm(Y ~ Z + X))$coef[2, 1], 3)

## stronger association between X and Z
Z = 2*X + U + rnorm(n)
round(summary(lm(Y ~ Z))$coef[2, 1], 3)
round(summary(lm(Y ~ Z + X))$coef[2, 1], 3)

## even stronger association between X and Z
Z = 10*X + U + rnorm(n)
round(summary(lm(Y ~ Z))$coef[2, 1], 3)
round(summary(lm(Y ~ Z + X))$coef[2, 1], 3)

