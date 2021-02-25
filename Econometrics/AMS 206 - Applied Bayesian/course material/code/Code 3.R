# Comparing the mean of two normals with known precisions
# Data
n    = 17  
m    = 23
phi1 = 1
phi2 = 1/4
x    = rnorm(n, 0, sqrt(1/phi1))
y    = rnorm(m, 1, sqrt(1/phi2))

#x = c( 0.9459729, -0.5388405,  1.4013212, -0.9748023,  1.3425622,  1.9893319,  
#       1.6432645, -0.1239098,  0.4860784,  0.6424578,  0.8745826, -0.6881401,
#      -0.7538624, -0.7622556, -0.2957435,  0.1014514, -1.1607980)
#y = c(-0.7616783,  3.6947879,  3.6487142,  7.2653851, -1.3628874,  2.0635219,
#      -1.4080133,  1.2338488,  1.2890826,  1.9157559,  1.4684068,  2.8503731,
#      -1.1873645,  2.0599470, -0.4449682, -1.4820969,  1.0820910, -0.7893104,
#       0.2538306,  2.6716138,  0.4541178,  2.1960326, -0.5721954)


# Prior
tau2 = 2
mu   = 0

# Marginal likelihood
lmarHa = -0.5*n*log(2*pi) - 0.5*n*log(phi1) - 0.5*log(tau2) - 0.5*log(n*phi1 + 1/tau2) - 
          0.5*(phi1*sum(x^2) + mu^2/tau2 - (phi1*n*mean(x) + mu/tau2)^2/(phi1*n + 1/tau2)) +
         -0.5*m*log(2*pi) - 0.5*m*log(phi2) - 0.5*log(tau2) - 0.5*log(m*phi2 + 1/tau2) - 
          0.5*(phi2*sum(y^2) + mu^2/tau2 - (phi2*m*mean(y) + mu/tau2)^2/(phi2*m + 1/tau2))

lmarH0 = -0.5*(n+m)*log(2*pi) - 0.5*n*log(phi1) - 0.5*m*log(phi2)  - 0.5*log(tau2) - 0.5*log(n*phi1 + m*phi2 + 1/tau2) - 
          0.5*(phi1*sum(x^2) + phi2*sum(y^2) + mu^2/tau2 - (phi1*n*mean(x) + phi2*m*mean(y) + mu/tau2)^2/(phi1*n + phi2*n + 1/tau2))

# Probability of model 0 (theta 1 = theta2)
prH0 = 1/(1 + exp(lmarHa - lmarH0))
prH0

zstat  = abs(mean(x)-mean(y))/sqrt(1/(n*phi1) + 1/(m*phi2))
pvalue = 2*pnorm(zstat, lower.tail=FALSE)
pvalue


