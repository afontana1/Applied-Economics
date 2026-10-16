rm(list=ls())

## a function testing coverage of a confidence interval
coverfn = function(truepar, point, varest)
{
  lowerCI = point - 1.96*sqrt(varest)
  upperCI = point + 1.96*sqrt(varest)
  
  return((lowerCI <= truepar)*(upperCI >= truepar))
}


## simulation in Chapter 4.5.1
pdf("neymanevaluation3.pdf", height = 9, width = 8)
par(mfrow = c(3, 2), mai = c(0.7, 0.5, 0.1, 0.1), 
    mgp = c(1.5, 0.5, 0))
## repeated sampling properties with constant effects
n  = 100
n1 = 60
n0 = 40
y0 = rexp(n)
y0 = sort(y0, decreasing = TRUE)
y1 = y0 + 1
tautrue = mean(y1) - mean(y0)
plot(y1 ~ y0, 
     xlab = expression(Y[0]), 
     ylab = expression(Y[1]))
z  = c(rep(1, n1), rep(0, n0))
MC = 10^4
TauHat = rep(0, MC)
VHat   = rep(0, MC)
CoverCI= rep(0, MC) 
for(mc in 1:MC)
{
  zmc = sample(z)
  y   = zmc*y1  + (1 - zmc)*y0
  tauhat     = mean(y[zmc==1]) - mean(y[zmc==0])
  TauHat[mc] = tauhat 
  vhat       = var(y[zmc==1])/n1 + var(y[zmc==0])/n0
  VHat[mc]   = vhat 
  CoverCI[mc]= coverfn(tautrue, tauhat, vhat)
}
## repeated sampling evaluation
mean(TauHat) -  tautrue
hist(TauHat - tautrue, xlab = expression(hat(tau) - tau),
     ylab = "", yaxt='n', main = expression(tau[i]==tau),
     breaks = 50, border = FALSE, col = "grey",
     xlim = c(-0.6, 0.6), freq = FALSE)
neymanvar = var(y1)/n1 + var(y0)/n0 - var(y1-y0)/n
xx = seq(-1, 1, 0.001)
yy = dnorm(xx, 0, sqrt(neymanvar))
lines(yy ~ xx)
result1 = c(var(TauHat),
            mean(VHat),
            mean(CoverCI))


## repeated sampling properties 
## with "negatively correlated potential outcomes"
y0 = sort(y0, decreasing = FALSE)
plot(y1 ~ y0, 
     xlab = expression(Y[0]), 
     ylab = expression(Y[1]))
tautrue = mean(y1) - mean(y0)
for(mc in 1:MC)
{
  zmc = sample(z)
  y   = zmc*y1  + (1 - zmc)*y0
  tauhat     = mean(y[zmc==1]) - mean(y[zmc==0])
  TauHat[mc] = tauhat 
  vhat       = var(y[zmc==1])/n1 + var(y[zmc==0])/n0
  VHat[mc]   = vhat 
  CoverCI[mc]= coverfn(tautrue, tauhat, vhat)
}
## repeated sampling evaluation
mean(TauHat) - tautrue
hist(TauHat - tautrue, xlab = expression(hat(tau) - tau),
     ylab = "", yaxt='n', 
     main = "negative corr between Y(1) and Y(0)",
     breaks = 50, border = FALSE, col = "grey",
     xlim = c(-0.6, 0.6), freq = FALSE)
neymanvar = var(y1)/n1 + var(y0)/n0 - var(y1-y0)/n
xx = seq(-1, 1, 0.001)
yy = dnorm(xx, 0, sqrt(neymanvar))
lines(yy ~ xx)
result2 = c(var(TauHat),
            mean(VHat),
            mean(CoverCI))



## repeated sampling properties with "independent potential outcomes"
y0 = sample(y0)
plot(y1 ~ y0, 
     xlab = expression(Y[0]), 
     ylab = expression(Y[1]))
tautrue = mean(y1) - mean(y0)
for(mc in 1:MC)
{
  zmc = sample(z)
  y   = zmc*y1  + (1 - zmc)*y0
  tauhat     = mean(y[zmc==1]) - mean(y[zmc==0])
  TauHat[mc] = tauhat 
  vhat       = var(y[zmc==1])/n1 + var(y[zmc==0])/n0
  VHat[mc]   = vhat 
  CoverCI[mc]= coverfn(tautrue, tauhat, vhat)
}
## repeated sampling evaluation
mean(TauHat) - tautrue
hist(TauHat - tautrue, xlab = expression(hat(tau) - tau),
     ylab = "", yaxt='n', 
     main = "uncorrelated Y(1) and Y(0)",
     breaks = 50, border = FALSE, col = "grey",
     xlim = c(-0.6, 0.6), freq = FALSE)
neymanvar = var(y1)/n1 + var(y0)/n0 - var(y1-y0)/n
xx = seq(-1, 1, 0.001)
yy = dnorm(xx, 0, sqrt(neymanvar))
lines(yy ~ xx)
result3 = c(var(TauHat),
            mean(VHat),
            mean(CoverCI))
dev.off()


## compare the results
results = cbind(result1, result2, result3)
colnames(results) = c("constant", "negative", "uncorrelated")
rownames(results) = c("var", "estimated var", "coverege rate")
round(results, 3)  


## simulation in Chapter 4.5.2
## heavy-tailed data
par(mfrow = c(3, 1))
# prob combination = 0.1
eps = rbinom(n, 1, 0.1)
y0 = (1 - eps)*rexp(n) + eps*rcauchy(n)
y1 = y0 + 1
tautrue = mean(y1) - mean(y0)
z  = c(rep(1, n1), rep(0, n0))
MC = 10^4
TauHat = rep(0, MC)
VHat   = rep(0, MC)
CoverCI= rep(0, MC) 
for(mc in 1:MC)
{
  zmc = sample(z)
  y   = zmc*y1  + (1 - zmc)*y0
  tauhat     = mean(y[zmc==1]) - mean(y[zmc==0])
  TauHat[mc] = tauhat 
  vhat       = var(y[zmc==1])/n1 + var(y[zmc==0])/n0
  VHat[mc]   = vhat 
  CoverCI[mc]= coverfn(tautrue, tauhat, vhat)
}
## repeated sampling evaluation
mean(TauHat) -  tautrue
hist(TauHat - tautrue, 
     xlab = "prob comtamination = 0.1",
     ylab = "", yaxt='n', main = expression(tau[i]==tau),
     breaks = 50, border = FALSE, col = "grey",
     xlim = c(-4, 5), freq = FALSE)
neymanvar = var(y1)/n1 + var(y0)/n0 - var(y1-y0)/n
xx = seq(-4, 5, 0.001)
yy = dnorm(xx, 0, sqrt(neymanvar))
lines(yy ~ xx)


# prob combination = 0.3
eps = rbinom(n, 1, 0.3)
y0 = (1 - eps)*rexp(n) + eps*rcauchy(n)
y1 = y0 + 1
tautrue = mean(y1) - mean(y0)
z  = c(rep(1, n1), rep(0, n0))
MC = 10^4
TauHat = rep(0, MC)
VHat   = rep(0, MC)
CoverCI= rep(0, MC) 
for(mc in 1:MC)
{
  zmc = sample(z)
  y   = zmc*y1  + (1 - zmc)*y0
  tauhat     = mean(y[zmc==1]) - mean(y[zmc==0])
  TauHat[mc] = tauhat 
  vhat       = var(y[zmc==1])/n1 + var(y[zmc==0])/n0
  VHat[mc]   = vhat 
  CoverCI[mc]= coverfn(tautrue, tauhat, vhat)
}
## repeated sampling evaluation
mean(TauHat) -  tautrue
hist(TauHat - tautrue, 
     xlab = "prob comtamination = 0.3",
     ylab = "", yaxt='n', main = expression(tau[i]==tau),
     breaks = 50, border = FALSE, col = "grey",
     xlim = c(-4, 4), freq = FALSE)
neymanvar = var(y1)/n1 + var(y0)/n0 - var(y1-y0)/n
xx = seq(-4, 5, 0.001)
yy = dnorm(xx, 0, sqrt(neymanvar))
lines(yy ~ xx)



# prob combination = 0.5
eps = rbinom(n, 1, 0.5)
y0 = (1 - eps)*rexp(n) + eps*rcauchy(n)
y1 = y0 + 1
tautrue = mean(y1) - mean(y0)
z  = c(rep(1, n1), rep(0, n0))
MC = 10^4
TauHat = rep(0, MC)
VHat   = rep(0, MC)
CoverCI= rep(0, MC) 
for(mc in 1:MC)
{
  zmc = sample(z)
  y   = zmc*y1  + (1 - zmc)*y0
  tauhat     = mean(y[zmc==1]) - mean(y[zmc==0])
  TauHat[mc] = tauhat 
  vhat       = var(y[zmc==1])/n1 + var(y[zmc==0])/n0
  VHat[mc]   = vhat 
  CoverCI[mc]= coverfn(tautrue, tauhat, vhat)
}
## repeated sampling evaluation
mean(TauHat) -  tautrue
hist(TauHat - tautrue, 
     xlab = "prob comtamination = 0.5",
     ylab = "", yaxt='n', main = expression(tau[i]==tau),
     breaks = 50, border = FALSE, col = "grey",
     xlim = c(-4, 4), freq = FALSE)
neymanvar = var(y1)/n1 + var(y0)/n0 - var(y1-y0)/n
xx = seq(-4, 4, 0.001)
yy = dnorm(xx, 0, sqrt(neymanvar))
lines(yy ~ xx)


## data analysis in Chapter 4.5.3
rm(list=ls())
library(Matching)
data(lalonde)
z = lalonde$treat
y = lalonde$re78

## Neymanian inference
n1= sum(z)
n0= length(z) - n1
tauhat = mean(y[z==1]) - mean(y[z==0])
vhat   = var(y[z==1])/n1 + var(y[z==0])/n0
sehat  = sqrt(vhat)
tauhat
sehat

## OLS
olsfit = lm(y ~ z)
summary(olsfit)$coef[2, 1: 2]

## Eicker-Huber-White se
library(car)
sqrt(hccm(olsfit)[2, 2])
sqrt(hccm(olsfit, type = "hc0")[2, 2])
sqrt(hccm(olsfit, type = "hc2")[2, 2])


