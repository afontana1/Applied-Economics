rm(list=ls())
library(car)

## function for SRE
## estimation
Neyman_SRE = function(z, y, x)
{
  xlevels = unique(x)
  K       = length(xlevels)
  PiK     = rep(0, K)
  TauK    = rep(0, K)
  varK    = rep(0, K)
  for(k in 1:K)
  {
    xk         = xlevels[k]
    zk         = z[x == xk]
    yk         = y[x == xk]
    PiK[k]     = length(zk)/length(z)
    TauK[k]    = mean(yk[zk==1]) - mean(yk[zk==0])
    varK[k]    = var(yk[zk==1])/sum(zk) + 
      var(yk[zk==0])/sum(1 - zk)
  }
  
  return(c(sum(PiK*TauK), sqrt(sum(PiK^2*varK))))
}


## Chapter 11.1.3
## Data "nhanes_bmi" from the "ATE" package in R
nhanes_bmi = read.csv("nhanes_bmi.csv")[, -1]
z = nhanes_bmi$School_meal
y = nhanes_bmi$BMI
x = as.matrix(nhanes_bmi[, -c(1, 2)])
x = scale(x)

## simple regression analysis
DiM = lm(y ~ z)
Fisher = lm(y ~ z + x)
Lin = lm(y ~ z + x + z*x)
res.regression = c(coef(DiM)[2], hccm(DiM)[2, 2]^0.5,
                   coef(Fisher)[2], hccm(Fisher)[2, 2]^0.5,
                   coef(Lin)[2], hccm(Lin)[2, 2]^0.5)
res.regression = matrix(res.regression,
                        nrow = 2, ncol = 3)
rownames(res.regression) = c("est", "se")
colnames(res.regression) = c("naive", "fisher", "lin")
round(res.regression, 3)                  


## propensity score stratification
pdf("pscore_stratified_hist.pdf", height = 4, width = 8)

pscore = glm(z ~ x, family = binomial)$fitted.values
par(mfrow = c(1, 3), mar = c(2, 0.1, 1, 0.1))
hist(pscore[z==1], freq = FALSE, col = "grey",
     border = NA, xlab = "",
     ylab = "", yaxt = "n", breaks = 5, 
     main = "breaks = 5", 
     xlim = c(0, 1), ylim = c(0, 4.5))
hist(pscore[z==0], freq = FALSE, 
     add = TRUE, breaks = 5)

hist(pscore[z==1], freq = FALSE, col = "grey",
     border = NA, xlab = "",
     ylab = "", yaxt = "n", breaks = 10, 
     main = "breaks = 10", 
     xlim = c(0, 1), ylim = c(0, 4.5))
hist(pscore[z==0], freq = FALSE, 
     add = TRUE, breaks = 10)

hist(pscore[z==1], freq = FALSE, col = "grey",
     border = NA, xlab = "",
     ylab = "", yaxt = "n", breaks = 30, 
     main = "breaks = 30", 
     xlim = c(0, 1), ylim = c(0, 4.5))
hist(pscore[z==0], freq = FALSE, 
     add = TRUE, breaks = 30)
dev.off()

## discretized by the quantiles of the pscore
pscore = glm(z ~ x, family = binomial)$fitted.values
n.strata = c(5, 10, 20, 50, 80)
strat.res = sapply(n.strata, FUN = function(nn){
  q.pscore = quantile(pscore, (1:(nn-1))/nn)
  ps.strata = cut(pscore, breaks = c(0,q.pscore,1), 
                  labels = 1:nn)
  Neyman_SRE(z, y, ps.strata)})

rownames(strat.res) = c("est", "se")
colnames(strat.res) = n.strata
round(strat.res, 3)




## Chapter 11.2.4
## functions for IPW estimation
ipw.est = function(z, y, x, truncps = c(0, 1))
{
  ## fitted propensity score
  pscore   = glm(z ~ x, family = binomial)$fitted.values
  pscore   = pmax(truncps[1], pmin(truncps[2], pscore))
  
  ace.ipw0 = mean(z*y/pscore - (1 - z)*y/(1 - pscore))
  ace.ipw  = mean(z*y/pscore)/mean(z/pscore) - 
    mean((1 - z)*y/(1 - pscore))/mean((1 - z)/(1 - pscore))
  
  return(c(ace.ipw0, ace.ipw))     
}


ipw.boot = function(z, y, x, n.boot = 500, truncps = c(0, 1))
{
  point.est  = ipw.est(z, y, x, truncps)
  
  ## nonparametric bootstrap
  n.sample   = length(z)
  x          = as.matrix(x)
  boot.est   = replicate(n.boot, {
    id.boot = sample(1:n.sample, n.sample, replace = TRUE)
    ipw.est(z[id.boot], y[id.boot], x[id.boot, ], truncps)
  })
  boot.se    = apply(boot.est, 1, sd)
  
  res = cbind(point.est, boot.se)
  colnames(res) = c("est", "se")
  rownames(res) = c("HT", "Hajek")
  
  return(res)
}


trunc.list = list(trunc0 = c(0,1), 
                  trunc.01 = c(0.01, 0.99), 
                  trunc.05 = c(0.05, 0.95), 
                  trunc.1 = c(0.1, 0.9))
trunc.est = lapply(trunc.list,
                   function(t){
                     est = ipw.boot(z, y, x, truncps = t)
                     round(est, 3)
                   })
trunc.est





## Chapter 11.3.2
## balance check for K=5
library(ggplot2)
Bcheck = sapply(1:dim(x)[2],
                FUN = function(px){
                  q.pscore = quantile(pscore, (1:4)/5)
                  ps.strata = cut(pscore, breaks = c(0,q.pscore,1), 
                                  labels = 1:5)
                  Neyman_SRE(z, x[, px], ps.strata)
                })

dat_balance = data.frame(est = Bcheck[1, ],
                         upper = Bcheck[1, ] + 1.96*Bcheck[2, ],
                         lower = Bcheck[1, ] - 1.96*Bcheck[2, ],
                         cov = factor(1:11))
ggplot(dat_balance) + 
  geom_errorbar(aes(x = cov,
                    ymin = lower,
                    ymax = upper),
                alpha = 0.6) + 
  geom_point(aes(x = cov,
                 y = est),
             alpha = 0.6) +
  geom_hline(aes(yintercept = 0),
             alpha = 0.3) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank()) +
  xlab("balance check based on stratification with K=5")                     
ggsave("balance_PSstratification5.pdf", height = 3, width = 8)


## balance check based on Hajek
Bcheck = sapply(1:dim(x)[2],
                FUN = function(px){
                  ipw.boot(z, x[, px], x)[2, ]
                })

dat_balance = data.frame(est = Bcheck[1, ],
                         upper = Bcheck[1, ] + 1.96*Bcheck[2, ],
                         lower = Bcheck[1, ] - 1.96*Bcheck[2, ],
                         cov = factor(1:11))
ggplot(dat_balance) + 
  geom_errorbar(aes(x = cov,
                    ymin = lower,
                    ymax = upper),
                alpha = 0.6) + 
  geom_point(aes(x = cov,
                 y = est),
             alpha = 0.6) +
  geom_hline(aes(yintercept = 0),
             alpha = 0.3) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank()) +
  xlab("balance check based on weighting")
ggsave("balance_PShajek.pdf", height = 3, width = 8)




## data for Problem 11.5
## Rosenbaum and Rubin 1983 Table 1
nn.surgical = c(26, 68, 98, 164,234)
nn.medical  = c(277, 235, 205, 139, 69)
prop.impv.surgical = c(0.54, 0.70, 0.70, 0.71, 0.70)
prop.impv.medical  = c(0.35, 0.40, 0.35, 0.30, 0.39)

