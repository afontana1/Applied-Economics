library(nimble)

##########################################
# Creating and manipulating a model object
##########################################

### Create a Nimble model
pumpData <- list(x = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22))
pumpConsts <- list(N = 10, t = c(94.3, 15.7, 62.9, 126, 5.24, 31.4, 1.05, 1.05, 2.1, 10.5))
pumpCode <- nimbleCode({ 
  for (i in 1:N){
    theta[i] ~ dgamma(alpha,beta)
    lambda[i] <- theta[i]*t[i]
    x[i] ~ dpois(lambda[i])
  }
  alpha ~ dexp(1.0)
  beta ~ dgamma(0.1,1.0) 
})
pumpInits <- list(alpha = 1, beta = 1, theta = rep(0.1, pumpConsts$N))
pump <- nimbleModel(code = pumpCode, name = 'pump', constants = pumpConsts,
                    data = pumpData, inits = pumpInits)



### Check the content of the models
pump$getNodeNames()
pump$x
pump$logProb_x
pump$alpha
pump$lambda
pump$getDependencies(c('alpha', 'beta'))
pump$getDependencies(c('alpha', 'beta'), determOnly = TRUE)
pump$plotGraph()        ## See all dependencies (popular, but I do not necessarily like it too much)

### Manipulating the model object
pump$theta              
pump$lambda
set.seed(0)             ## This makes the simulations here reproducible
pump$simulate('theta')  ## Simulate a new value of theta (from the prior, I believe)
pump$theta              
pump$lambda
pump$calculate(pump$getDependencies(c('theta')))
pump$lambda



##################################################################
# Creating and manipulating an algorithm associated with the model
##################################################################

#Configure the model
pumpConf <- configureMCMC(pump, print = TRUE)   ### Configure the MCMC engine
pumpConf$addMonitors(c('alpha', 'beta', 'theta'))

# You can run the code built in R
pumpMCMC <- buildMCMC(pumpConf)
niter <- 100
set.seed(0)
pumpMCMC$run(niter)


# You can run also compile the code in C
Cpump <- compileNimble(pump)
CpumpMCMC <- compileNimble(pumpMCMC, project = pump)
niter <- 10000
set.seed(0)
CpumpMCMC$run(niter)


# Playing around with the posterior samples
samples <- as.matrix(CpumpMCMC$mvSamples)
par(mfrow = c(2, 2), mar = c(4, 4, 1, 1)+0.1)
plot(samples[ , 'alpha'], type = 'l', xlab = 'iteration',
     ylab = expression(alpha))
plot(samples[ , 'beta'], type = 'l', xlab = 'iteration',
     ylab = expression(beta))
plot(samples[ , 'alpha'], samples[ , 'beta'], xlab = expression(alpha),
     ylab = expression(beta))
plot(samples[ , 'theta[1]'], type = 'l', xlab = 'iteration',
     ylab = expression(theta[1]))

par(mfrow = c(2, 1), mar = c(4, 4, 1, 1)+0.1)
acf(samples[ , 'alpha'], ylab=expression(paste("ACF of ",alpha)))
acf(samples[ , 'beta'], ylab=expression(paste("ACF of ",beta)))



## Constructing a new sampler with a joint MH for alpha and beta
pumpConf$addSampler(target = c('alpha', 'beta'), type = 'RW_block',
                   control = list(adaptInterval = 100))
pumpMCMC2 <- buildMCMC(pumpConf)
CpumpNewMCMC <- compileNimble(pumpMCMC2, project  = pump, resetFunctions = TRUE)
set.seed(0);
CpumpNewMCMC$run(niter)

# Playing around with the new posterior samples
samples <- as.matrix(CpumpNewMCMC$mvSamples)
par(mfrow = c(2, 2), mar = c(4, 4, 1, 1)+0.1)
plot(samples[ , 'alpha'], type = 'l', xlab = 'iteration',
     ylab = expression(alpha))
plot(samples[ , 'beta'], type = 'l', xlab = 'iteration',
     ylab = expression(beta))
plot(samples[ , 'alpha'], samples[ , 'beta'], xlab = expression(alpha),
     ylab = expression(beta))
plot(samples[ , 'theta[1]'], type = 'l', xlab = 'iteration',
     ylab = expression(theta[1]))

par(mfrow = c(2, 1), mar = c(4, 4, 1, 1)+0.1)
acf(samples[ , 'alpha'], ylab=expression(paste("ACF of ",alpha)))
acf(samples[ , 'beta'], ylab=expression(paste("ACF of ",beta)))

