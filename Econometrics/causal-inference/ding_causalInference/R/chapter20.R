rm(list=ls())


library("ggplot2")
library("tidyr")

## Figure 20.2
mlda = read.csv("mlda.csv")
mlda

mlda$agecell21 = mlda$agecell - 21
mlda$older21 = (mlda$agecell > 0)

mlda.long = gather(mlda, var, outcome, 
                   all:externalother, factor_key=TRUE)

ggplot(mlda.long) + 
  geom_point(aes(x = agecell, y = outcome,
                 shape = var),
             alpha = 0.5) +
  scale_shape_manual(values = 1:9) +
  geom_vline(aes(xintercept = 21),
             alpha = 0.7) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
ggsave("mlda_9outcomes.pdf", height = 4, width = 8.5)




## Figure 20.3
pdf("rdd_graph.pdf", height = 6, width = 8.5)
par(mfrow = c(2, 2), mar = c(4, 2, 1, 1), mgp = c(2,1,0))
n   = 500
x   = rnorm(n)
y0  = x + rnorm(n, 0, 0.5)
y1  = y0 + 5
z   = (x>=0)
y   = z*y1 + (1-z)*y0
plot(y0 ~ x, col = "grey", pch = 19, cex = 0.3,
     ylim = c(min(y), max(y)),
     xlab = "X", ylab = "")
points(y1 ~ x, col = "grey", pch = 19, cex = 0.3)
points(y ~ x, col = "black", pch = 19, cex = 0.3)
abline(v = 0, lty = 2)


Greg = lm(y ~ z + x + z*x)
cbind(coef(Greg)[2], confint(Greg, 'zTRUE'))


y0  = x + rnorm(n, 0, 0.5)
y1  = y0 + 1
z   = (x>=0)
y   = z*y1 + (1-z)*y0
plot(y0 ~ x, col = "grey", pch = 19, cex = 0.3,
     ylim = c(min(y), max(y)),
     ylab = "", xlab = "X")
points(y1 ~ x, col = "grey", pch = 19, cex = 0.3)
points(y ~ x, col = "black", pch = 19, cex = 0.3)
abline(v = 0, lty = 2)

Greg = lm(y ~ z + x + z*x)
cbind(coef(Greg)[2], confint(Greg, 'zTRUE'))



y0  = 2*x + rnorm(n, 0, 0.5)
y1  = 5 + 0.5*x + rnorm(n, 0, 0.5)
z   = (x>=0)
y   = z*y1 + (1-z)*y0
plot(y0 ~ x, col = "grey", pch = 19, cex = 0.3,
     ylim = c(min(y), max(y)),
     ylab = "", xlab = "X")
points(y1 ~ x, col = "grey", pch = 19, cex = 0.3)
points(y ~ x, col = "black", pch = 19, cex = 0.3)
abline(v = 0, lty = 2)


Greg = lm(y ~ z + x + z*x)
cbind(coef(Greg)[2], confint(Greg, 'zTRUE'))


y0  = 2*x + rnorm(n, 0, 0.5)
y1  = 1 + 0.5*x + rnorm(n, 0, 0.5)
z   = (x>=0)
y   = z*y1 + (1-z)*y0
plot(y0 ~ x, col = "grey", pch = 19, cex = 0.3,
     ylim = c(min(y), max(y)),
     ylab = "", xlab = "X")
points(y1 ~ x, col = "grey", pch = 19, cex = 0.3)
points(y ~ x, col = "black", pch = 19, cex = 0.3)
abline(v = 0, lty = 2)

Greg = lm(y ~ z + x + z*x)
cbind(coef(Greg)[2], confint(Greg, 'zTRUE'))
dev.off()

## simulation under nonlinear models - Problem 20.3



## Chapter 20.2.4
house = read.csv("house.csv")[, -1]
pdf("lee2008rdd.pdf", height = 4, width = 8.5)
par(mfrow = c(1, 1), mar = c(3, 3, 1, 1), mgp = c(2,1,0))
plot(y ~ x, data = house, pch = 19, cex = 0.1)
abline(v = 0, col = "grey")
dev.off()

library(rdrobust)
RDDest = rdrobust(house$y, house$x)
cbind(RDDest$coef, RDDest$ci)

pdf("rdd_lee.pdf", height = 5, width = 8.5)
house$z = (house$x >= 0)
hh = seq(0.05, 1, 0.01)
local.lm = sapply(hh, function(h){
  Greg = lm(y ~ z + x + z*x, data = house,
            subset = (abs(x)<=h))
  cbind(coef(Greg)[2], confint(Greg, 'zTRUE'))
})
plot(local.lm[1, ] ~ hh, type = "p",
     pch = 19, cex = 0.3,
     ylim = range(local.lm),
     xlab = "h",
     ylab = "point and interval estimates",
     main = "subset linear regression: |X|<h")
lines(local.lm[2, ] ~ hh, type = "p",
      pch = 19, cex = 0.1) 
lines(local.lm[3, ] ~ hh, type = "p",
      pch = 19, cex = 0.1)
dev.off()

