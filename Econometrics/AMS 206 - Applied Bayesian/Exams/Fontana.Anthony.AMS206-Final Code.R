##Anthony Fontana

### Number 1D

data <- read.table("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/AMS 206 - Applied Bayesian/lifetime.txt",header=T)

# Data
n <- length(data$y)
meanx <- mean(data$x)
meany <- mean(data$y)

# Marginal liklihood of null
null <- pgamma(meany, n, n*meanx, lower.tail = T)

# marginal liklihood alternative
alt <- pgamma(meany, n, n*meanx, lower.tail = F)

# Probability of null given hypothesis
1/(1 + alt/null)

###Number 2C
install.packages("VGAM")
require(VGAM)
b <- sqrt(rnorm(100000)^2 + rnorm(100000)^2)
ray<- rgenray(100000,shape = 1)
par(mfrow=c(3,1))
plot(density(ray),main="rayleigh")
plot(density(b),main="beta")

delta <- rnorm(100000)/b
plot(density(delta),xlim=c(-10,10),main="delta")

####Number 2D

install.packages("invgamma")
#initial values
library(invgamma)
library(MASS)
omega <- 10000
sigmasqr <- 15 
G <- c(5,-0.1,0.5)

data <- read.table("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/AMS 206 - Applied Bayesian/orbits.txt",header=T)
y<-data$Y
x<-data$X
#Data matrix as a function of omega
func <- function (x, omega) {
  m1<-rep(1, times = 100)
  m2 <- cos(omega*x)
  m3 <- sin(omega*x)
  dmat <- as.matrix(cbind(m1,m2,m3))
  return(dmat)
}

output = matrix (0, nrow = 10000, ncol = 5)
colnames(output) = c("omega","sigma2","alpha","gamma1","gamma2")

#logFullconditional for omega
logFullConditional<- function(omega,G, sigmasqr,x){
      dmat<- func(x,omega)
      omega <- (1/(2*sigmasqr))*sum((y-dmat*G)^2)
}

#Metropolis-Hastings
mh <- function(prev, G, sigmasqr,x){
  prop <- rnorm(1, prev, sqrt(1))
  if(log(runif(1)) < logFullConditional(prop, G, sigmasqr,x) - logFullConditional(prev, G, sigmasqr,x)) {
    return(prop)
  }else{
    return(prev)
  }
}

for (i in 1:10000) {
  #Conditional for omega
  omega = mh(omega, G, sigmasqr,x)
  dmat = func(x, omega)
  
  #Conditional for sigmasqr
  sigmasqr.alpha <- 50
  sigmasqr.beta  <- (.5)*sum((y-dmat*G)^2)
  sigmasqr       <- rinvgamma(1, sigmasqr.alpha, sigmasqr.beta)
  
  #Conditional for gamma
  G.Sigma        <- solve(t(dmat)%*%dmat) * sigmasqr
  G.mu           <- (G.Sigma %*% t(dmat)) %*% y / sigmasqr
  G              <- mvrnorm(1, G.mu, G.Sigma)
  
  output[i, "omega"]  = omega
  output[i, "sigma2"] = sigmasqr
  output[i, 3:5]      = G
}


par(mfrow=c(3,2))
plot(output[,1], type = "l")
plot(output[,2], type = "l")
plot(output[,3], type = "l")
plot(output[,4], type = "l")
plot(output[,5], type = "l")


par(mfrow=c(3,2))
plot(density(output[,1]))
plot(density(output[,2]))
plot(density(output[,3]))
plot(density(output[,4]))
plot(density(output[,5]))

mean1<-mean(output[,2])
mean2<-mean(output[,4])
mean3<-mean(output[,5])

quantile(output[,2],prob = c(.025, .975))
quantile(output[,4],prob = c(.025, .975))
quantile(output[,5],prob = c(.025, .975))

sigma1mean <- mean(output[,4])
sigma2mean <- mean(output[,5])
alphamean  <- mean(output[,3])
omegamean  <- log(mean(output[,1]))
sigmanew   <- mean(output[,2])
