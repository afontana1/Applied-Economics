rm(list=ls())

## Chapter 19.3
library("Matching")
library("sensitivitymv")
library("sensitivitymw")

## LaLonde observational data
dat <- read.table("cps1re74.csv",header=T)
# unemployed
dat$u74 <- as.numeric(dat$re74==0)
dat$u75 <- as.numeric(dat$re75==0)
y = dat$re78
z = dat$treat
x = as.matrix(dat[, c("age", "educ", "black",
                      "hispan", "married", "nodegree",
                      "re74", "re75", "u74", "u75")])

matchest = Match(Y = y, Tr = z, X = x)
summary(matchest)

matchest.adj = Match(Y = y, Tr = z, X = x, BiasAdjust = TRUE)
summary(matchest.adj)

## Rosenbaum sensitivity analysis for Lalonde
ytreated = y[matchest$index.treated]
ycontrol = y[matchest$index.control]
datamatched = cbind(ytreated, ycontrol)

par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.3, 0.3))
hist(ytreated - ycontrol, breaks = 30,
     freq = FALSE, main = "",
     xlab = expression(hat(tau)[i]))

Gamma  = seq(1, 1.4, 0.001)
Pvalue = Gamma
for(i in 1:length(Gamma))
{
  Pvalue[i] = senmw(datamatched, gamma = Gamma[i], 
                    method = "t")$pval
}
gammastar = Gamma[which(Pvalue >= 0.05)[1]]
gammastar

plot(Pvalue ~ Gamma, type = "l",   
     xlab = expression(Gamma), 
     ylab = "p-value")
abline(h = 0.05, lty = 2)
abline(v = gammastar, lty = 2)


par(mfrow = c(3, 1), mai = c(0.4, 0.1, 0.1, 0.1))
pair_diff     = datamatched[, 1] - datamatched[, 2]
pair_diff_abs = abs(pair_diff)
Svecobs       = (pair_diff >= 0)
Tobs          = sum(Svecobs*pair_diff_abs)
## null distribution with Gamma = 1
nn    = length(pair_diff)
null1 = replicate(10^4,
                  {
                    Svec = rbinom(nn, 1, 0.5)
                    sum(Svec*pair_diff_abs)
                  })
hist(null1, xlim = c(5*10^5, 11*10^5),
     yaxt = "n", main = "", xlab = "",
     breaks = 30)
abline(v = Tobs)
legend("topright", 
       paste("p = ", round(mean(null1 >= Tobs), 3)),
       bty = "n")
legend("topleft",
       expression(Gamma == 1),
       bty = "n")

## Gamma = 1.1
prob_gamma = 1.1/(1.1 + 1)
null1.1 = replicate(10^4,
                  {
                    Svec = rbinom(nn, 1, prob_gamma)
                    sum(Svec*pair_diff_abs)
                  })
hist(null1.1, xlim = c(5*10^5, 11*10^5),
     yaxt = "n", main = "", xlab = "",
     breaks = 30)
abline(v = Tobs)
legend("topright", 
       paste("p = ", round(mean(null1.1 >= Tobs), 3)),
       bty = "n")
legend("topleft",
       expression(Gamma == 1.1),
       bty = "n")

## Gamma = 1.3
prob_gamma = 1.3/(1.3 + 1)
null1.3 = replicate(10^4,
                  {
                    Svec = rbinom(nn, 1, prob_gamma)
                    sum(Svec*pair_diff_abs)
                  })
hist(null1.3, xlim = c(5*10^5, 11*10^5),
     yaxt = "n", main = "", xlab = "",
     breaks = 30)
abline(v = Tobs)
legend("topright", 
       paste("p = ", round(mean(null1.3 >= Tobs), 3)),
       bty = "n")
legend("topleft",
       expression(Gamma == 1.3),
       bty = "n")
dev.off()




## other Rosenbaum sensitivity analysis
par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.3, 0.3))
data(erpcp)
hist(erpcp[, 1] - erpcp[, 2], main = "erpcp",
     xlab = expression(hat(tau)[i]),
     freq = FALSE)

Gamma  = seq(1, 5, 0.005)
Pvalue = Gamma
for(i in 1:length(Gamma))
{
  Pvalue[i] = senmw(erpcp, gamma = Gamma[i], method = "t")$pval
}
gammastar = Gamma[which(Pvalue >= 0.05)[1]]
gammastar

plot(Pvalue ~ Gamma, type = "l", 
     xlab = expression(Gamma), 
     ylab = "p-value")
abline(h = 0.05, lty = 2)
abline(v = gammastar, lty = 2)
dev.off()

par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.3, 0.3))
data(lead250)
hist(lead250[, 1] - lead250[, 2],
     main = "lead250",
     xlab = expression(hat(tau)[i]),
     freq = FALSE)

Gamma  = seq(1, 2.5, 0.001)
Pvalue = Gamma
for(i in 1:length(Gamma))
{
  Pvalue[i] = senmw(lead250, gamma = Gamma[i], method = "t")$pval
}
gammastar = Gamma[which(Pvalue >= 0.05)[1]]
gammastar

plot(Pvalue ~ Gamma, type = "l", 
     xlab = expression(Gamma), ylab = "p-value")
abline(h = 0.05, lty = 2)
abline(v = gammastar, lty = 2)

