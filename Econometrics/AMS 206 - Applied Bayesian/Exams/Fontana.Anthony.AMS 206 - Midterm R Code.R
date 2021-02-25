
#Installing dependencies 
library(data.table)
install.packages("msm")
install.packages("truncnorm")
library(truncnorm)
install.packages("pscl")
library(pscl)
install.packages("magrittr")
library(magrittr)


#reading in the data
data_ <- data.frame(read.table("C:/Users/Aj/Documents/UCSC Coursework/Winter Quarter 2017/AMS 206 - Applied Bayesian/interestrate.csv"))[,1]
MonteCarlo <- function(rho, sigma2, mu, data_){
  
    xi <- -3
    tau2 <- 1
    a <- 2
    b <- .1
    n <- length(data_)


    outputmatrix = matrix(0, nrow = 10000, ncol=3)
    colnames(outputmatrix) = c("rho", "sigma2", "mu")

        for (i in 2:10000) {

          rho.mean = sum((data_[-1] - mu) * (data_[-n] - mu)) / (sum((data_[-n] - mu)^2))
          rho.var <- sigma2 / sum((data_[-n]-mu)^2)
          rho_ = rtruncnorm(n=1, mean=rho.mean, sd=sqrt(rho.var), a=-1, b=1)


          sigma2.alpha <- a + (n-1)/2
          sigma2.beta <- b + .5 * sum(
            ((data_[-1] - mu) - rho*(data_[-n] - mu))^2
            )
          sigma2_ <- rigamma(1, sigma2.alpha, abs(sigma2.beta))


          y_bar <- mean(data_[-1]-rho*data_[-n])/(1-rho)
          sigma2_star <- sigma2/(1-rho)^2
          mu.var <- (n-1)/sigma2_star + 1/tau2
          mu.mu <- (y_bar/(sigma2_star/n-1) + xi/tau2)/mu.var
          mu_ <- rnorm(1, mu.mu, sqrt(mu.var))


          outputmatrix[i,"rho"] = rho_
          outputmatrix[i,"sigma2"] = sigma2_
          outputmatrix[i,"mu"] = mu_
        }


    rhovector <- outputmatrix[, "rho"]
    sigma2vector <- outputmatrix[, "sigma2"]
    muvector <- outputmatrix[, "mu"]


# C

    plot(rhovector, type = "l", main = "rho")
    plot(sigma2vector, type = "l", main = "sigma2")
    plot(muvector[0:10000], type = "l", main = "mu")

    hist(rhovector[1:10000], breaks = 1000,main = "Posterior Rho")
    hist(sigma2vector[1:10000], breaks = 1000, main = "Posterior Var")
    hist(muvector[1:10000], breaks = 1000, main = "Long Run Avg")
    
    return(list(rhovector, sigma2vector, muvector))
}

function_estimate <- MonteCarlo(rho = 0.9, sigma = 1, mu = -3, data_)
function_estimate_2 <- MonteCarlo(rho = 0.3, sigma = 10, mu = -10, data_)
plot(function_estimate[[2]], type = "l", main = "sigma2")
plot(function_estimate[[3]], type = "l", main = "mu")


#rho
mean(function_estimate[[1]])
quantile(function_estimate[[1]], prob = c(.025, .975))

#sigma
mean(function_estimate[[2]])
quantile(function_estimate[[2]], prob = c(.025, .975))

#mu
mean(function_estimate[[3]])
quantile(function_estimate[[3]], prob = c(.025, .975))


