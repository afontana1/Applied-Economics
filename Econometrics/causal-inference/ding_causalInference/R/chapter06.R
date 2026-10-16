rm(list=ls())
library("ggplot2")

library("car")
library("foreign")
## Angrist 2009 data: Canadian university - Chapter 6.4
angrist   = read.dta("star.dta")
angrist2  = subset(angrist, control == 1|sfsp == 1)
## imputing missing outcomes
y = angrist2$GPA_year1
meany    = mean(y, na.rm = TRUE)
y = ifelse(is.na(y), meany, y)
z = angrist2$sfsp 
x = angrist2[, c("female", "gpa0")]

## unadjusted estimator
fit_unadj = lm(y ~ z)
ace_unadj = coef(fit_unadj)[2]
se_unadj  = sqrt(hccm(fit_unadj, type = "hc2")[2, 2])

## regression adjustment
x         = scale(x)
fit_adj   = lm(y ~ z*x)
ace_adj   = coef(fit_adj)[2]
se_adj    = sqrt(hccm(fit_adj, type = "hc2")[2, 2])

res = c(ace_unadj, ace_adj, se_unadj, se_adj)
dim(res) = c(2, 2)
t.stat   = res[, 1]/res[, 2]
p.value  = 2*pnorm(abs(t.stat), lower.tail = FALSE) 
res      = cbind(res, t.stat, p.value)
rownames(res) = c("Neyman", "Lin")
colnames(res) = c("estimate", "s.e.", "t-stat", "p-value")
round(res, 3)

## rerandomization
Mahalanobis2 = function(z, x)
{
  x1 = x[z==1, ]
  x0 = x[z==0, ]
  n1 = dim(x1)[1]
  n0 = dim(x0)[1]
  diff    = apply(x1, 2, mean) - apply(x0, 2, mean)
  covdiff = (n1 + n0)/(n1*n0)*cov(x)
  M       = sum(diff*solve(covdiff, diff))
  return(M)
}

rReM = function(x, n1, n0, a)
{
  n     = n1 + n0
  z     = sample(c(rep(1, n1), rep(0, n0)))
  M     = Mahalanobis2(z, x)
  while(M > a){
    z = sample(z)
    M = Mahalanobis2(z, x)
  }
  return(z)
}

x2       = cbind(x, x^2)
data2    = data.frame(y = y, z = z, x2 = x2)
y1lm     = lm(y ~ x2, weights = z, data = data2)
sigma1   = summary(y1lm)$sigma 
y0lm     = lm(y ~ x2, weights = 1 - z, data = data2)
sigma0   = summary(y0lm)$sigma 

## two settings 
a  = 0.05
MC = 2000
n  = length(z)
n1 = sum(z)
n0 = n - n1

rescale   = 0.1
y1impute  = predict(y1lm, data = data2) + 
                 rnorm(n)*sigma1*rescale
y0impute  = predict(y0lm, data = data2) +
                 rnorm(n)*sigma0*rescale
tauimpute = mean(y1impute - y0impute)

TauHatCRE    = rep(0, MC)
TauHatRegCRE = rep(0, MC)
TauHatReM    = rep(0, MC)
TauHatRegReM = rep(0, MC)
for(mc in 1:MC){
  ## complete randomization
  zCRE = sample(z)
  yCRE = zCRE*y1impute + (1-zCRE)*y0impute
  TauHatCRE[mc]    = mean(yCRE[zCRE==1]) - mean(yCRE[zCRE==0])
  TauHatRegCRE[mc] = lm(yCRE ~ zCRE*x)$coef[2]
  
  ## rerandomization
  zReM = rReM(x, n1, n0, a)
  yReM = zReM*y1impute + (1-zReM)*y0impute
  TauHatReM[mc]    = mean(yReM[zReM==1]) - mean(yReM[zReM==0])
  TauHatRegReM[mc] = lm(yReM ~ zReM*x)$coef[2]
}
 
dat.1 = data.frame(rescale = "rescale = 0.1",
                   estimate = c(TauHatCRE, TauHatRegCRE, TauHatReM, TauHatRegReM)
                         - tauimpute,
                   method = rep(c("cre.N", "cre.L", "rem.N", "rem.L"),
                                each = MC))

rescale  = 0.25
y1impute = predict(y1lm, data = data2) + 
  rnorm(n)*sigma1*rescale
y0impute = predict(y0lm, data = data2) +
  rnorm(n)*sigma0*rescale
tauimpute = mean(y1impute - y0impute)

for(mc in 1:MC){
  ## complete randomization
  zCRE = sample(z)
  yCRE = zCRE*y1impute + (1-zCRE)*y0impute
  TauHatCRE[mc]    = mean(yCRE[zCRE==1]) - mean(yCRE[zCRE==0])
  TauHatRegCRE[mc] = lm(yCRE ~ zCRE + x + zCRE*x)$coef[2]
  
  ## rerandomization
  zReM = rReM(x, n1, n0, a)
  yReM = zReM*y1impute + (1-zReM)*y0impute
  TauHatReM[mc]    = mean(yReM[zReM==1]) - mean(yReM[zReM==0])
  TauHatRegReM[mc] = lm(yReM ~ zReM + x + zReM*x)$coef[2]
}
 
dat.25 = data.frame(rescale = "rescale = 0.25",
                   estimate = c(TauHatCRE, TauHatRegCRE, TauHatReM, TauHatRegReM)
                         - tauimpute,
                   method = rep(c("cre.N", "cre.L", "rem.N", "rem.L"),
                                each = MC))


dat = rbind(dat.1, dat.25)
dat$method = ordered(dat$method,
                     c("cre.N", "cre.L", "rem.N", "rem.L"))
ggplot(dat) + 
  geom_violin(aes(x=method, y=estimate),
              draw_quantiles = 0.5) + 
  geom_hline(aes(yintercept = 0),
             alpha = 0.3) + 
  facet_grid(~rescale) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
ggsave("RemReg_violin.pdf", height = 4, width = 8)
