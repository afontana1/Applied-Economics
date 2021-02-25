####### Metropolis sampler for the Gumble problem
### x | \theta \sim Gumble(\theta)
### \theta \sim normal(xi, kappa^2)


### Generate data (true theta=0)
x <- -log(-log(runif(100)))
quartz()
hist(x)

### Prior Parameters
xi <- 0
kappa2 <- 1

### Number of iterations
r <- 10000
#Variance of proposal
tau2 <- 5

### Initial value
thetac <- 0

###Output
theta.out <- rep(0, r)
aceptrate <- 0

### Density of the Gumble distribution
dgumble <- function(x, theta, log.d=F){
  z <- -(x-theta) - exp( -(x-theta))
  if(log.d == F){
    return(exp(z))
  }else{
    return(z)
  }
}

### Random walk metropolis algorithm
for(s in 1:r){
  thetap <- rnorm(1, thetac, sqrt(tau2))
  alph <- sum(dgumble(x, thetap, log.d=T)) - sum(dgumble(x, thetac, log.d=T))  # Note that the likelihood and prior are evaluated in the logarithmic scale to avoid overflow
  alph <- alph + dnorm(thetap, xi, sqrt(kappa2), log=T) - dnorm(thetac, xi, sqrt(kappa2), log=T)
  u <- runif(1, 0, 1)
  if(log(u) < alph){  #Comparisons are also made in the log scale, and there is no need to compute the minimum.
    thetac <- thetap
    aceptrate <- aceptrate + 1
  }
  print(s)
  theta.out[s] <- thetac
}

print(paste("Acceptance rate is ", aceptrate/r))

quartz()
#par(mfrow=c(2,2))
hist(theta.out)

quartz()
plot(theta.out, type="l")
quartz()
acf(theta.out)
summary(theta.out)











####### Gaussian Process example
# y | \lambda, \kappa2 \sim \normal(0, \Sigma(\lambda,\kappa2))
# \lambda \sim Gamma(a_lambda, b_lambda)
# \kappa2 \sim Gamma(a_kappa2, b_kappa2)

### Basic functions
rm(list=ls())
library(mvtnorm)

construct.Sigma = function(xst1, xst2, kappa2, lambda){
  Sigma    = kappa2*exp(-abs(outer(xst1, xst2, "-"))/lambda)
  return(Sigma)
}

sample.Sigma = function(lambda, kappa2, Sigma, xst, y, Omega, alambda, blambda, akappa2, bkappa2){
  # This could be improved if we coded the density of the multivariate normal ourselves in terms of Sigma^{-1} and stored it (would save one inversion per iteration)
  n         = length(y)
  temp      = rmvnorm(1, c(log(lambda),log(kappa2)), Omega)
  lambdap   = exp(temp[1])
  kappa2p   = exp(temp[2])
  Sigmap    = construct.Sigma(xst, xst, kappa2p, lambdap)
  laceptr   = dmvnorm(y, rep(0, n), Sigmap, log=T) - dmvnorm(y, rep(0, n), Sigma, log=T)
  laceptr   = laceptr + dgamma(lambdap, alambda, scale=blambda, log=T) - dgamma(lambda, alambda, scale=blambda, log=T)
  laceptr   = laceptr + dgamma(kappa2p, akappa2, scale=bkappa2, log=T) - dgamma(kappa2, akappa2, scale=bkappa2, log=T)
  laceptr   = laceptr + log(lambdap) - log(lambda)
  laceptr   = laceptr + log(kappa2p) - log(kappa2)
  
  u = runif(1,0,1)
  if(log(u)<laceptr){
    lambda   = lambdap
    kappa2   = kappa2p
    Sigma    = Sigmap
    aceptr   = 1
  }else{
    aceptr   = 0
  }
  return(list(lambda=lambda, kappa2=kappa2, Sigma=Sigma, aceptr=aceptr))
}

### Generate data
n        = 30
xst      = runif(n, 0, 1)
lambda.t = 0.2
kappa2.t = 0.2
Sigma    = construct.Sigma(xst, xst, kappa2.t, lambda.t)
Sigma
y        = as.vector(rmvnorm(1, rep(0, n), Sigma))

### Prior parameters
akappa2 = 1
bkappa2 = 1
alambda = 1
blambda = 1

### Initial values
kappa2 = rgamma(1, akappa2, scale=bkappa2)
lambda = rgamma(1, alambda, scale=blambda)

### Default initial value for tuning parameter
Omega = matrix(c(0.3, 0, 0, 0.3), 2, 2)
#Omega = 2.38^2*matrix(c(0.3, 0.24, 0.24, 0.25), 2, 2)/2   #Obtained using the posterior variance

#Omega = matrix(c(0.745, 0.657, 0.657, 0.651), 2, 2)

### Setup number iterations
burn = 1000
repl = 11000
lambda.out = rep(0,repl)
kappa2.out = rep(0,repl)
aceptr.mh  = 0

for(s in 1:repl){
  temp   = sample.Sigma(lambda, kappa2, Sigma, xst, y, Omega, alambda, blambda, akappa2, bkappa2)
  lambda = temp$lambda
  kappa2 = temp$kappa2
  Sigma  = temp$Sigma
  aceptr.mh = aceptr.mh + temp$aceptr
  if(s/1000==floor(s/1000)){
    print(s)
  }
  lambda.out[s] = lambda
  kappa2.out[s] = kappa2
}

print(aceptr.mh/repl)

print(summary(lambda.out))
print(summary(kappa2.out))

quartz()
plot(log(lambda.out),log(kappa2.out))
quartz()
plot(lambda.out,kappa2.out)
quartz()
par(mfrow=c(2,2))
hist(lambda.out)
hist(kappa2.out)
acf(lambda.out)
acf(kappa2.out)



