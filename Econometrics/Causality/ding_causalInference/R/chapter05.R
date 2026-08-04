rm(list=ls())

## basic function -  test statistics in SRE with large strata
stat_SRE = function(z, y, x)
{
       xlevels = unique(x)
       K       = length(xlevels)
       PiK     = rep(0, K)
       TauK    = rep(0, K)
       WK      = rep(0, K)
       for(k in 1:K)
       {
             xk         = xlevels[k]
             zk         = z[x == xk]
             yk         = y[x == xk]
             PiK[k]     = length(zk)/length(z)
             TauK[k]    = mean(yk[zk==1]) - mean(yk[zk==0])
             WK[k]      = wilcox.test(yk[zk==1], yk[zk==0])$statistic
       }
       
       return(c(sum(PiK*TauK), sum(WK/PiK)))
}

## basic function -  random treatment vector in a SRE
zRandomSRE = function(z, x)
{
  xlevels = unique(x)
  K       = length(xlevels)
  zrandom = z
  for(k in 1:K)
  {
    xk = xlevels[k]   
    zrandom[x == xk] = sample(z[x == xk])
  }
  
  return(zrandom)
}

## pennsylvania re-employment bonus experiment - Chapter 5.2.2
## description of the DATA: 
## Koenker and Xiao 2002 Econometrica 
## "Inference on the Quantile Regression Process" 

penndata = read.table("Penn46_ascii.txt")
z = penndata$treatment
y = log(penndata$duration)
block = penndata$quarter
table(penndata$treatment, penndata$quarter)



 

## Fisher Randomization test
## values of the test statistics
stat.obs = stat_SRE(z, y, block)
MC = 10^3
statSREMC = matrix(0, MC, 2)
for(mc in 1:MC)
{
    zrandom         = zRandomSRE(z, block) 
    statSREMC[mc, ] = stat_SRE(zrandom, y, block)     
}
mean(statSREMC[, 1] <= stat.obs[1])
mean(statSREMC[, 2] <= stat.obs[2])


pdf("FRT_SRE_histograms.pdf", height = 6, width = 8)
par(mfrow = c(2, 1), mai = c(1, 0.2, 0.1, 0.2))
hist(statSREMC[, 1], 
     xlab = expression(hat(tau)[S]),
     ylab = "", main = "", yaxt = 'n', 
     border = FALSE, col = "grey",
     breaks = 20)
abline(v = stat.obs[1])

hist(statSREMC[, 2], 
     xlab = expression(W[S]),
     ylab = "", main = "", yaxt = 'n', 
     border = FALSE, col = "grey",
     breaks = 20)
abline(v = stat.obs[2])
dev.off()



## Chapter 5.3.2 
rm(list=ls())

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
  
  return(c(sum(PiK*TauK), sum(PiK^2*varK)))
}


## simulation with a few large strata
K  = 5
n  = 80
n1 = 50
n0 = 30
x  = rep(1:K, each = n)
y0 = rexp(n*K, rate = x)
y1 = y0 + 1
zb = c(rep(1, n1), rep(0, n0))
MC = 10^4
TauHat = rep(0, MC)
VarHat = rep(0, MC)
for(mc in 1:MC)
{
  z  = replicate(K, sample(zb))
  z  = as.vector(z)
  y  = z*y1 + (1-z)*y0
  est = Neyman_SRE(z, y, x) 
  TauHat[mc] = est[1]
  VarHat[mc] = est[2]
}

par(mfrow = c(2, 1), mai = c(1, 0.1, 0.2, 0.2))
hist(TauHat, xlab = expression(hat(tau)[S]),
     ylab = "", main = "a few large strata", 
     border = FALSE, col = "grey",
     breaks = 30, yaxt = 'n',
     xlim = c(0.8, 1.2))
abline(v = 1)

var(TauHat)
mean(VarHat)

## simulation with many small strata
K  = 50
n  = 8
n1 = 5
n0 = 3
x  = rep(1:K, each = n)
y0 = rexp(n*K, rate = log(x + 1))
y1 = y0 + 1
zb = c(rep(1, n1), rep(0, n0))
MC = 10^4
TauHat = rep(0, MC)
VarHat = rep(0, MC)
for(mc in 1:MC)
{
  z  = replicate(K, sample(zb))
  z  = as.vector(z)
  y  = z*y1 + (1-z)*y0
  est = Neyman_SRE(z, y, x) 
  TauHat[mc] = est[1]
  VarHat[mc] = est[2]
}

hist(TauHat, xlab = expression(hat(tau)[S]),
     ylab = "", main = "many small strata", 
     border = FALSE, col = "grey",
     breaks = 30, yaxt = 'n',
     xlim = c(0.8, 1.2))
abline(v = 1)

var(TauHat)
mean(VarHat)



## Neymanian analysis of the penn data - Chapter 5.3.2
## pennsylvania re-employment bonus experiment
## description of the DATA: 
## Koenker and Xiao 2002 Econometrica 
## "Inference on the Quantile Regression Process" 

penndata = read.table("Penn46_ascii.txt")
z = penndata$treatment
y = log(penndata$duration)
block = penndata$quarter
est = Neyman_SRE(z, y, block)
est[1]
sqrt(est[2])


## Chapter 5.4.1
## stratum 1
n11 = 98+8
y11 = 98/n11
n10 = 115+5
y10 = 115/n10
ace1 = y11 - y10
var1 = y11*(1-y11)/n11 + y10*(1-y10)/n10

## stratum 2
n21 = 76+22
y21 = 76/n21
n20 = 69+16
y20 = 69/n20
ace2 = y21 - y20
var2 = y21*(1-y21)/n21 + y20*(1-y20)/n20

## post-stratification
n1 = n11+n10
n2 = n21+n20
n  = n1+n2
ace = n1/n*ace1 + n2/n*ace2
var = (n1/n)^2*var1 + (n2/n)^2*var2

## crude estimation
y1.crude = 174/(174+30) 
y0.crude = 184/(184+21)
ace.crude = y1.crude - y0.crude
var.crude = y1.crude*(1-y1.crude)/(174+30) + y0.crude*(1-y0.crude)/(184+21)

## comparison
comparison = c(ace1, ace2, ace, ace.crude,
               sqrt(var1), sqrt(var2), sqrt(var), sqrt(var.crude))
comparison = matrix(comparison, byrow = TRUE,
                    nrow = 2, ncol = 4)
rownames(comparison) = c("est", "se")
colnames(comparison) = c("stratum 1", "stratum 2", "overall", "crude")
round(comparison, 3)



## Chapter 5.4.2
library("foreign")
dat_chong = read.dta("chong.dta")
table(dat_chong$treatment, dat_chong$class_level)

use.vars = c("treatment", 
             "gradesq34", 
             "class_level", 
             "anemic_base_re")
dat_physician = subset(dat_chong,
                       treatment != "Soccer Player",
                       select = use.vars)
dat_physician$z = (dat_physician$treatment=="Physician")
dat_physician$y = dat_physician$gradesq34
table(dat_physician$z, 
      dat_physician$class_level)
table(dat_physician$z, 
      dat_physician$class_level,
      dat_physician$anemic_base_re)

## stratified estimator
tauS = with(dat_physician,
            Neyman_SRE(z, gradesq34, class_level))
## further post-stratification 
tauSPS = with(dat_physician, {
  sps = interaction(class_level, anemic_base_re)
  Neyman_SRE(z, gradesq34, sps)
})
# comparison
est = c(tauS[1], tauSPS[1])
se  = sqrt(c(tauS[2], tauSPS[2]))
t.stat = est/se
p.value = 2*pnorm(abs(t.stat), lower.tail = FALSE)
comparison = cbind(est, se, t.stat, p.value)
row.names(comparison) = c("stratify", "stratify & post-stratify")
round(comparison, 3)


## data for Problem 5.6
treatment = list(c(1,1,0,0), 
                 c(1,1,0,0), 
                 c(1,1,1,0,0), 
                 c(1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,0,0), 
                 c(1,1,1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,0,0),
                 c(1,1,0,0))
outcome = list(c(0.165,0.321,-0.197,0.236), 
               c(0.918,-0.202,1.19,0.117),
               c(0.341,0.561,-0.059,-0.496,0.225),
               c(-0.024,-0.450,-1.104,-0.956), 
               c(-0.258,-0.083,-0.126,0.106),
               c(1.151,0.707,0.597,-0.495), 
               c(0.077,0.371,0.685,0.270), 
               c(-0.870,-0.496,-0.444,0.392,-0.934,-0.633), 
               c(-0.568,-1.189,-0.891,-0.856), 
               c(-0.727,-0.580,-0.473,-0.807), 
               c(-0.533,0.458,-0.383,0.313), 
               c(1.001,0.102,0.484,0.474,0.140), 
               c(0.855,0.509,0.205,0.296), 
               c(0.618,0.978,0.742,0.175), 
               c(-0.545,0.234,-0.434,-0.293), 
               c(-0.240,-0.150,0.355,-0.130))


