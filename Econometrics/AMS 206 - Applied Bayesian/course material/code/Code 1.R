##### Binomial model with unknown success probability

# Data
n = 129     # Number of women surveyed
sumy = 25   # Number of affirmative answers

# Likelihood
theta = seq(0, 1, length=100)
lik = (theta^sumy)*(1-theta)^(n-sumy)
plot(theta, lik, type="l", ylab="Likelihood of data")

# Prior and posterior distribution under a uniform prior
theta = seq(0, 1, length=100)
plot(theta, dbeta(theta,1,1), type="l", ylim=c(0,12), ylab="Densities")
lines(theta, dbeta(theta,sumy+1,n-sumy+1), col="red")
legend(0.5,7,c("Prior", "Posterior"),col=c("black","red"), lty=c(1,1), bty="n")


# How different prior distributions compare
theta = seq(0, 1, length=100)
plot(theta, dbeta(theta,1,3), type="l", ylim=c(0,8), ylab="Prior density")
lines(theta, dbeta(theta,10, 30), col="red")
lines(theta, dbeta(theta,.1, .3), col="green")
lines(theta, dbeta(theta,20, 60), col="blue")
legend(0.5,7,c("a=0.1  b=0.3", "a=1     b=3", "a=10   b=30", "a=20   b=60"),col=c("green","black","red","blue"), lty=c(1,1,1,1,1), bty="n")


# Prior and posterior distribution under a general beta prior, including posterior mean and 95% symmetric credible interval
a = 10
b = 30
theta = seq(0, 1, length=100)
plot(theta, dbeta(theta, a, b), type="l", ylim=c(0,14), ylab="Density")
lines(theta, dbeta(theta, sumy+a, n-sumy+b), col="red")
abline(v = (sumy+a)/(n+a+b), lty=3)
abline(v = qbeta(0.025, sumy+a, n-sumy+b), lty=2, col="grey")
abline(v = qbeta(0.975, sumy+a, n-sumy+b), lty=2, col="grey")
legend(0.5,10,c("Prior", "Posterior", "Creible interval", "Posterior mean"),col=c("black","red","grey","black"), lty=c(1,1,2,3), bty="n")


# Credible interval
c(qbeta(0.025, sumy+a, n-sumy+b), qbeta(0.975, sumy+a, n-sumy+b))

# Classical confidence interval
thetahat = sumy/n
c(thetahat - 2*sqrt(thetahat*(1-thetahat)/n), thetahat + 2*sqrt(thetahat*(1-thetahat)/n))
