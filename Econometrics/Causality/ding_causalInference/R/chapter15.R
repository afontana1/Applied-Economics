rm(list=ls())
library("car")
library("Matching")

## Chapter 15.5.1
## experimental data
data("lalonde")
head(lalonde)

y = lalonde$re78
z = lalonde$treat
x = as.matrix(lalonde[, c("age", "educ", "black",
                          "hisp", "married", "nodegr",
                          "re74", "re75")])

## analysis the randomized experiment
neymanols = lm(y ~ z)
fisherols = lm(y ~ z + x)
xc = scale(x)
linols = lm(y ~ z*xc)
resols = c(neymanols$coef[2],
           fisherols$coef[2],
           linols$coef[2],
           sqrt(hccm(neymanols, type = "hc2")[2, 2]),
           sqrt(hccm(fisherols, type = "hc2")[2, 2]),
           sqrt(hccm(linols, type = "hc2")[2, 2]))
resols = matrix(resols, 3, 2)
rownames(resols) = c("neyman", "fisher", "lin")
colnames(resols) = c("est", "se")
resols 

## analysis as if it is an observational study
matchest.adj = Match(Y = y, Tr = z, X = x, BiasAdjust = TRUE)
summary(matchest.adj)

 
## Chapter 15.5.2
## observational data
dat <- read.table("cps1re74.csv", header = TRUE)
# unemployed
dat$u74 <- as.numeric(dat$re74==0)
dat$u75 <- as.numeric(dat$re75==0)
y = dat$re78
z = dat$treat
x = as.matrix(dat[, c("age", "educ", "black",
                          "hispan", "married", "nodegree",
                          "re74", "re75", "u74", "u75")])

## analyze as if it is from a randomized experiment
neymanols = lm(y ~ z)
fisherols = lm(y ~ z + x)
xc = scale(x)
linols = lm(y ~ z*xc)
resols = c(neymanols$coef[2],
           fisherols$coef[2],
           linols$coef[2],
           sqrt(hccm(neymanols, type = "hc2")[2, 2]),
           sqrt(hccm(fisherols, type = "hc2")[2, 2]),
           sqrt(hccm(linols, type = "hc2")[2, 2]))
resols = matrix(resols, 3, 2)
rownames(resols) = c("neyman", "fisher", "lin")
colnames(resols) = c("est", "se")
resols 


## analyze the observational study
matchest = Match(Y = y, Tr = z, X = x, BiasAdjust = TRUE)
summary(matchest)

## matched pairs analysis 
diff = y[matchest$index.treated] - 
          y[matchest$index.control]
round(summary(lm(diff ~ 1))$coef[1, ], 2)

diff.x = x[matchest$index.treated, ] - 
              x[matchest$index.control, ]
round(summary(lm(diff ~ diff.x))$coef[1, ], 2)



## Chapter 15.5.3
## balance checking before and after matching
lm.before = lm(z ~ x)
summary(lm.before) 

lm.after = lm(z ~ x, 
              subset = c(matchest$index.treated, 
                         matchest$index.control))
summary(lm.after) 

