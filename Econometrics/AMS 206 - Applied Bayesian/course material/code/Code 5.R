## Data
y = rnorm(25, 0, 1)
n = length(y)
ybar = mean(y)

## Prior
theta = 0
tau2  = 4
a     = 2
b     = 2

##########################
## MAP estimate
epsilon = 0.000001
mu = 5
sigma2 = 0.2
mu.old = mu
sigma2.old = sigma2

i = 0
repeat{
  mu     = (n*ybar/sigma2 + theta/tau2)/(n/sigma2 + 1/tau2)
  sigma2 = (b+0.5*sum((y-mu)^2))/(a+0.5*n)
  i = i + 1
  if(all(c(abs(mu.old-mu)/abs(mu.old), abs(sigma2.old-sigma2)/abs(sigma2.old))< epsilon)){
    break
  }else{
    mu.old = mu
    sigma2.old = sigma2
  }
}
mu.map     = mu
sigma2.map = sigma2
mu.map
sigma2.map

##########################
## Gibbs sampler
mu = 5
sigma2 = 0.2
rrr = 20000

mu.out     = rep(0, rrr)
sigma2.out = rep(0, rrr)

for(ss in 1:rrr){
  mu     = rnorm(1, (n*ybar/sigma2 + theta/tau2)/(n/sigma2 + 1/tau2), sqrt(1/(n/sigma2 + 1/tau2)))
  sigma2 = 1/rgamma(1, a+n/2, b+0.5*sum((y-mu)^2))
  
  mu.out[ss]     = mu
  sigma2.out[ss] = sigma2
}

plot(mu.out,sigma2.out, pch=20)
points(mu.map, sigma2.map, col="red", pch=3)
points(mean(y), var(y), col="green", pch=4)
points(mean(mu.out), mean(sigma2.out), col="yellow", pch=2)
legend(0.37,2.5, c("MAP","MLE","Post mean"), col=c("red","green", "yellow"), pch=c(3,4,2), bty="n")


plot(mu.out, type="l")
plot(sigma2.out, type="l")
acf(mu.out)
acf(sigma2.out)

## Location
print(mu.map)
print(mean(mu.out))
print(mean(y))

## Variance
print(sigma2.map)
print(mean(sigma2.out))
print(var(y))


