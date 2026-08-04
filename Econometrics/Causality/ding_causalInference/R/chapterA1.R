rm(list=ls())

## bootstrapping the sample mean - Chapter A1.6
## sample size
n  = 100
## number of Monte Carlo simulations
MC = 500
## number of bootstrap replicates B
n.boot = 200
simulation = replicate(MC, {
  Y = rnorm(n, 1, 1)
  boot.mu.hat = replicate(n.boot, {
    index = sample(1:n, n, replace = TRUE)
    mean(Y[index])
  })
  c(mean(Y), var(Y)/n, var(boot.mu.hat))
})
## summarize the results
apply(simulation, 1, mean)



