# Sampling approach for inference on the correct diagnosis rate.
# x ~ Bin(n1, theta1)     theta1 ~ Beta(a1,b1)
# y ~ Bin(n2, theta2)     theta2 ~ Beta(a2,b2)
# z ~ Bin(m, phi)         phi ~ Beta(a3,b3)
# Interest is on eta = theta1*phi/(theta1*phi + theta2*(1-phi))

# Data
x  = 953
n1 = 1000
y  = 67
n2 = 1000
z  = 3812
m  = 1000000

# Hyperparameters
a1 = 1
b1 = 1
a2 = 1
b2 = 1
a3 = 1
b3 = 1

# Number of samples used in the approximation
rr = 100000
theta1 = rbeta(rr, x+a1, n1-x+b1)
theta2 = rbeta(rr, y+a2, n2-y+b2)
phi    = rbeta(rr, z+a3, m-z+b3)
eta = theta1*phi/(theta1*phi + theta2*(1-phi))

hist(eta, xlim=c(0,1), xlab=expression(eta), main="Posterior")
mean(eta)
quantile(eta, c(0.025,0.975))



#####
# Understanding the implied prior (rather than the  posterior)

theta1 = rbeta(rr, a1, b1)
theta2 = rbeta(rr, a2, b2)
phi    = rbeta(rr, a3, b3)
eta = theta1*phi/(theta1*phi + theta2*(1-phi))
hist(eta, xlim=c(0,1), xlab=expression(eta), main="Prior")

