### y_i ~ \normal(\theta, 1)
### \theta ~ DExp(\mu, \tau)
library(msm)

# Data and prior
ybar = 0
n    = 10
mu   = 1
tau  = 1


lambda1 = ybar + 1/(n*tau)
lambda2 = ybar - 1/(n*tau)

a = exp(-mu/tau + 0.5*n*lambda1^2)*pnorm((mu - lambda1)/sqrt(n))
b = exp(mu/tau + 0.5*n*lambda2^2)*pnorm((mu - lambda2)/sqrt(n), lower.tail=FALSE)
w1 = a/(a+b)

x = seq(-2,2,length=10000)
y = w1*dtnorm(x, mean=lambda1, sd=sqrt(1/n), lower=-Inf, upper=mu) + (1-w1)*dtnorm(x, mean=lambda2, sd=sqrt(1/n), lower=mu, upper=Inf)
plot(x,y,type="l")




## Posterior expectation as a function of mu
mus = seq(-3,3,length=1001)
lambda1 = ybar + 1/(n*tau)
lambda2 = ybar - 1/(n*tau)
a = exp(-mus/tau + 0.5*n*lambda1^2)*pnorm(sqrt(n)*(mus - lambda1))
b = exp( mus/tau + 0.5*n*lambda2^2)*pnorm(sqrt(n)*(mus - lambda2),lower.tail=FALSE)
w1 = a/(a+b)

yy = w1*(lambda1 - (1/sqrt(n))*dnorm(sqrt(n)*(mus-lambda1))/pnorm(sqrt(n)*(mus-lambda1))) +
     (1-w1)*(lambda2 + (1/sqrt(n))*dnorm(sqrt(n)*(mus-lambda2))/pnorm(sqrt(n)*(mus-lambda2),lower.tail=FALSE))

kappa2 = 2*tau^2  ## The variance of the double exponential is 2*tau^2
zz = (n*ybar + mus/kappa2)/(n + 1/kappa2)  # Under a Gaussian prior

plot(mus, zz, type="l")
lines(mus, yy, col="red")
abline(h=lambda1, lty=2, col="grey")
abline(h=lambda2, lty=2, col="grey")
abline(h=0, lty=2, col="grey")
abline(v=0, lty=2, col="grey")
legend(-3.2, 0.1, c("Gaussian prior","Double Exp prior"), col=c("black","red"),lty=c(1,1),bty="n",cex=0.9)


