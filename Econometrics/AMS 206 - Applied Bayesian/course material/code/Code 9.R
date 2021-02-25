#####
## Bivariate Gaussian model with missing data
## y | mu, Sigma ~ Normal(mu, Sigma)
## mu ~ Normal(d, D)
## Sigma ~ IWis(nu, A)

library(mvtnorm)
library(MCMCpack)

n         = 50
mu.t      = c(2, 5)
Sigma.t   = matrix(c(1,0.7,0.7,1), nrow=2, ncol=2)
y.t       = rmvnorm(n, mu.t, Sigma.t)
plot(y.t)
zz        = rbinom(n, 1, 0.20)
zz2       = zz*(floor(2*runif(n))+1)

y           = y.t
y[zz2==1,1] = NA
y[zz2==2,2] = NA
missing     = is.na(y)

#load("test.Rdata")
load("final.Rdata")

## Prior
d    = c(0, 0)
D    = diag(100,2)
Dinv = solve(D)
nu = 3
A  = diag(1,2)

mu = rmvnorm(1, d, D)
Sigmainv = drop(rWishart(1, nu, solve(A)))
Sigma    = solve(Sigmainv)

rrr = 10000
mu.out = array(0, dim=c(rrr,2))
Sigma.out = array(0, dim=c(rrr,2,2))
for(s in 1:rrr){
  # Impute missing values
  for(i in 1:n){
    if(missing[i,1]){
      y[i,1] = rnorm(1, mu[1] + Sigma[1,2]*(y[i,2] - mu[2])/Sigma[2,2], Sigma[1,1] - Sigma[1,2]^2/Sigma[2,2])
    }else{
      if(missing[i,2]){
        y[i,2] = rnorm(1, mu[2] + Sigma[2,1]*(y[i,1] - mu[1])/Sigma[1,1], Sigma[2,2] - Sigma[2,1]^2/Sigma[1,1])
      }
    }
  }
  
  # Sample the mean
  DD.post = solve(n*Sigmainv + Dinv)
  d.post  = drop(DD.post%*%(Sigmainv%*%apply(y,2,sum) + Dinv%*%d))
  mu = drop(rmvnorm(1, d.post, DD.post))
  
  # Sample the variance-covariance
  yst      = scale(y, center=mu, scale=FALSE)
  A.post   = t(yst)%*%yst + solve(A)
  nu.post  = n + nu
  Sigmainv = drop(rWishart(1, nu.post, solve(A.post)))
  Sigma    = solve(Sigmainv)
  
  # Store values
  mu.out[s,]     = mu
  Sigma.out[s,,] = Sigma
  
  if(s/100==floor(s/100)){
    print(paste("s =",s))
  }
}

mu.postexp    = apply(mu.out,2,mean)
Sigma.postexp = apply(Sigma.out,c(2,3),mean)
Sigma.postexp[1,2]/sqrt(Sigma.postexp[1,1]*Sigma.postexp[2,2])




