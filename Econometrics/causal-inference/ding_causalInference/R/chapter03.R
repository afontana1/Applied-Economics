rm(list=ls())


## generating all possible CREs with fixed sample size - Chapter 3.2
permutation10 = function(n, n1){
 M  = choose(n, n1)
 treat.index = combn(n, n1)
 Z = matrix(0, n, M)
 for(m in 1:M){
   treat  = treat.index[, m]
   Z[treat, m] = 1
 }
 Z
}

permutation10(5, 3)


## FRT for the LaLonde data - Chapter 3.4
library(Matching)
data(lalonde)
z = lalonde$treat
y = lalonde$re78


## histograms
pdf("lalonde_histograms.pdf", width = 6, height = 5)
par(mfrow = c(1, 1))
hist(y[z == 0], col = "grey", border = FALSE, 
     freq = FALSE, breaks = 30, 
     xlab = "real earnings in 1978", 
     ylab = "density",
     main = "", yaxt = "n")
hist(y[z == 1], freq = FALSE, 
     breaks = 30, add = TRUE)
abline(v = mean(y[z == 1]))
abline(v = mean(y[z == 0]), lty = 2)
legend("topright",
       c("treatment mean", "control mean"),
       col = c("black", "grey"),
       lty = 1:2, bty = "n")
dev.off()


## observed test statistics
tauhat = t.test(y[z == 1], y[z == 0], 
                var.equal = TRUE)$statistic
tauhat
student = t.test(y[z == 1], y[z == 0],
                 var.equal = FALSE)$statistic
student
W = wilcox.test(y[z == 1], y[z == 0])$statistic
W
D = ks.test(y[z == 1], y[z == 0])$statistic
D



## Monte Carlo version of the FRT
MC = 10^4
Tauhat   = rep(0, MC)
Student  = rep(0, MC)
Wilcox   = rep(0, MC)
Ks       = rep(0, MC)
for(mc in 1:MC)
{
  zperm       = sample(z)
  Tauhat[mc]  = t.test(y[zperm == 1], y[zperm == 0], 
                       var.equal = TRUE)$statistic 
  Student[mc] = t.test(y[zperm == 1], y[zperm == 0], 
                       var.equal = FALSE)$statistic
  Wilcox[mc]  = wilcox.test(y[zperm == 1], 
                            y[zperm == 0])$statistic 
  Ks[mc]      = ks.test(y[zperm == 1], 
                        y[zperm == 0])$statistic
}

pdf("FRT_lalonde.pdf", height = 6, width = 7)
par(mfrow = c(2, 2), mar = c(4,1,1,1), mgp = c(1.5, 0.5, 0))
hist(Tauhat, col = "grey", 
     border = FALSE, freq = FALSE,
     xlab = expression(t[eqvar]),
     ylab = "",
     main = "", yaxt = "n")
abline(v = tauhat)
xx = seq(-5, 5, 0.01)
yy = dnorm(xx)
lines(yy ~ xx)


hist(Student, col = "grey", 
     border = FALSE, freq = FALSE,
     xlab = expression(t[uneqvar]),
     ylab = "",
     main = "", yaxt = "n")
abline(v = student)
lines(yy ~ xx)

hist(Wilcox, col = "grey", 
     border = FALSE, freq = FALSE,
     xlab = expression(W),
     ylab = "",
     main = "", yaxt = "n")
abline(v = W)
nn  = length(z)
nn1 = sum(z)
mean.null = nn1*(nn - nn1 + 1)/2 
var.null  = nn1*(nn - nn1)*(nn + 1)/12
se.null   = sqrt(var.null)
xx        = seq(mean.null - 3*se.null, mean.null + 3*se.null, 1)
yy        = dnorm(xx, mean.null, se.null)
lines(yy ~ xx)

hist(Ks, col = "grey", 
     border = FALSE, freq = FALSE,
     xlab = expression(D),
     ylab = "",
     main = "", yaxt = "n")
abline(v = D)
dev.off()

## exact one-sided p-values
exact.pv = c(mean(Tauhat >= tauhat),
             mean(Student >= student),
             mean(Wilcox >= W),
             mean(Ks >= D))
round(exact.pv, 3)


## approximate tests, easy implementation in R
asym.pv = c(t.test(y[z == 1], y[z == 0], 
                   var.equal = TRUE)$p.value, 
            t.test(y[z == 1], y[z == 0], 
                   var.equal = FALSE)$p.value,
            wilcox.test(y[z == 1], y[z == 0])$p.value,
            ks.test(y[z == 1], y[z == 0])$p.value)
round(asym.pv, 3)
