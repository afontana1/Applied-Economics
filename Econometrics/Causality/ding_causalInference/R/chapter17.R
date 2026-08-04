rm(list=ls())

## e-value based on RR
evalue = function(rr)
{
  rr + sqrt(rr*(rr - 1))
}


## Chapter 17.3
## Hammond and Holl (1958 JAMA) Data
## Two by Two Table
##          Lung Cancer    No Lung Cancer
##Smoker    397            78557
##Nonsmoker 51             108778

## Analysis
p1  = 397/(397+78557)
p0  = 51/(51+108778)

## Relative Risk
rr  = p1/p0

## Asymptotic Variance of Log of RR
logrr = log(p1/p0)
se    = sqrt(1/397+1/51-1/(397+78557)-1/(51+108778))
upper = exp(logrr+1.96*se)
lower = exp(logrr-1.96*se)

## point estimate
rr
## lower CI
lower
## e-value based on rr
evalue(rr)
## e-value based on lower CI
evalue(lower)


## Figure 17.1
## bias factor and hyperbola
RR   = rr
RR.L = lower
xmax = 40
x = seq(0, xmax, 0.01)
pdf("jointvalueplot.pdf", height = 7, width = 6.5)
plot(x, x, lty = 2, col = "white", type = "l", xaxs = "i", yaxs = "i", xaxt="n", yaxt = "n",
     xlab = expression(RR[ZU]), ylab = expression(RR[UY]),
     xlim = c(0,xmax),
     main = "")

x = seq(RR, xmax, 0.01)
x.L = seq(RR.L, xmax, 0.01)

y    = RR*(RR-1)/(x-RR)+RR
y.L  = RR.L*(RR.L-1)/(x.L-RR.L)+RR.L


abline(h=RR, lty = 3, col = "grey")
abline(h=RR.L, lty = 3, col = "grey")
abline(v=RR, lty = 3, col = "grey")
abline(v=RR.L, lty = 3, col = "grey")
abline(0, 1, lty = 3, col = "grey")

lines(x, y, type = "l")
lines(x.L, y.L, type = "l", lty = 2)




high = RR + sqrt(RR*(RR-1))
high.L = RR.L + sqrt(RR.L*(RR.L-1))




points(high, high, pch = 19)
points(high.L, high.L, pch = 19)


label5 = seq(5, 40, by = 5)
axis(1, label5, label5, cex.axis = 1)
axis(2, label5, label5, cex.axis = 1)



text(high.L+5, high.L, "(15.52, 15.52)")
text(high+5, high, "(20.95, 20.95)")

legend("bottomleft", c(expression(
  RR[ZU]*RR[UY]/(RR[ZU]+RR[UY]-1)==10.73
), expression(
  RR[ZU]*RR[UY]/(RR[ZU]+RR[UY]-1)==8.02
)),
lty = 1:2)
dev.off()        





## Chapter 17.4.2
NCHS2003 = read.table("NCHS2003.txt", header = TRUE, sep = "\t")
## outcome: PTbirth
y_logit = glm(PTbirth ~ ageabove35 + 
                mar + smoking + drinking + somecollege + 
                hispanic + black + nativeamerican + asian,
              data = NCHS2003, 
              family = binomial)
log_or   = summary(y_logit)$coef[2, 1:2]
est      = exp(log_or[1])
lower.ci = exp(log_or[1] - 1.96*log_or[2])
est
evalue(est)
lower.ci
evalue(lower.ci)




## Figure 17.2
pdf("value_rr.pdf", height = 3, width = 8.5)
par(mfrow = c(1, 3))
## small RR
RR = seq(1, 1.5, 0.01)
evalue = RR + sqrt(RR*(RR - 1))
plot(evalue ~ RR, type = "l",
     ylab = "E-value")

## medium RR
RR = seq(1, 3, 0.01)
evalue = RR + sqrt(RR*(RR - 1))
plot(evalue ~ RR, type = "l",
     ylab = "E-value")

## large RR
RR = seq(1, 10, 0.01)
evalue = RR + sqrt(RR*(RR - 1))
plot(evalue ~ RR, type = "l",
     ylab = "E-value")
dev.off()
