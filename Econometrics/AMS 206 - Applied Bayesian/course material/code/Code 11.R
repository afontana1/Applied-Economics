### Homes dataset
library(nimble)
rm(list=ls())
homes <- as.matrix(read_csv("~/Documents/Courses/AMS206/homes.csv"))
plot(homes[,3], homes[,1], xlab="Living Area", ylab="Sell price")  
plot(homes[,4], homes[,1], xlab="Number of Rooms", ylab="Sell price")


### Create a Nimble model
houseCode <- nimbleCode({ 
  for (i in 1:N){
    y[i] ~ dnorm(beta0 + beta1*x1[i] + beta2*x2[i], tau=phi)
  }
  phi ~ dgamma(0.1,1.0)
  beta0 ~ dnorm(0, 1)
  beta1 ~ dnorm(0, 1)
  beta2 ~ dnorm(0, 1)
})
houseData <- list(y=drop(as.matrix(homes[,1])))
houseConsts <- list(N=dim(homes)[1], p=2, x1=drop(as.matrix(homes[,3])), x2=drop(as.matrix(homes[,4])))
houseInits <- list(phi = 1, beta0 = 0, beta1 = 0, beta2 = 0)

house <- nimbleModel(code = houseCode, name = 'house', constants = houseConsts,
                     data = houseData, inits = houseInits)



houseConf <- configureMCMC(house, print = TRUE)   ### Configure the MCMC engine
houseConf$addMonitors(c('beta0', 'beta1', 'beta2', 'phi'))


# You can run also compile the code in C
Chouse <- compileNimble(house)
houseMCMC <- buildMCMC(houseConf)
ChouseMCMC <- compileNimble(houseMCMC, project = house)
niter <- 11000
set.seed(0)
ChouseMCMC$run(niter)

samples <- as.matrix(ChouseMCMC$mvSamples)
sellpricepred <- rep(0, niter)

## Prediction for a house with x1 = 25 and x2 = 6
x1st = 25
x2st = 6
sellpricepred <- samples[,'beta0'] + samples[,'beta1']*x1st + samples[,'beta2']*x2st

mean(sellpricepred[-seq(1:1000)])
quantile(sellpricepred[-seq(1:1000)], c(0.025, 0.975))

plot(samples[,'beta1'], samples[,'beta2'])