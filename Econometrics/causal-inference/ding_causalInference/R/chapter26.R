rm(list=ls())

## Chapter 26.4.2
## function for SACE with a binary outcome
SACE01.fit = function(Z, M, Y)
{
  ## summary statistics
  pM1 = mean(M[Z==1])
  pM0 = mean(M[Z==0])
  
  mu11 = mean(Y[Z==1&M==1])
  mu01 = mean(Y[Z==0&M==1])
  
  ## proporitions of the strata
  pi11 = pM0
  pi00 = 1 - pM1
  pi10 = 1 - pi11 - pi00
  
  ## bounds on the treatment potential outcomes
  lb = ((pi11 + pi10)*mu11 - pi10)/pi11
  ub = ((pi11 + pi10)*mu11)/pi11 
  
  ## bounds on the SACE
  c(lb - mu01, ub - mu01)
}

SACE01 = function(Z, M, Y, n.boot = 1000, alpha = 0.05)
{
  bounds = SACE01.fit(Z, M, Y)
  index  = 1:length(Z)
  cv = qnorm(1 - alpha)
  
  ## bootstrap se of the upper and lower bounds
  b.bounds = sapply(1:n.boot, FUN = function(b){
    id.boot = sample(index, replace = TRUE)
    SACE01.fit(Z[id.boot], M[id.boot], Y[id.boot])
  })
  b.se = apply(b.bounds, 1, sd)
  
  ## Imbens and Manski confidence interval
  l.ci = bounds[1] - cv*b.se[1]
  u.ci = bounds[2] + cv*b.se[2]
  
  res = cbind(bounds, b.se, c(l.ci, u.ci))
  colnames(res) = c("est", "se", "confidence limit")
  row.names(res) = c("lower", "upper")
  
  return(res)
}

## Chapter 26.4.3
## truncation by death example
## data from Yang and Small (2016)
pi11 = 277/(277 + 152)
pi00 = 109/(109 + 322)
pi10 = 1 - pi11 - pi00

## observed means
mu11 = 54/322
mu01 = 59/277

## bounds on the treatment potential outcomes
lb = ((pi11 + pi10)*mu11 - pi10)/pi11
ub = ((pi11 + pi10)*mu11)/pi11
lb
ub

## bounds on the sace
lb - mu01
ub - mu01


## truncation by death example
## data from Yang and Small (2016)
Z = c(rep(1, 322+109), rep(0, 277+152))
M = c(rep(1, 322), rep(0, 109),
      rep(1, 277), rep(0, 152))
Y = c(rep(1, 54), rep(0, 268), rep(NA, 109),
      rep(1, 59), rep(0, 218), rep(NA, 152))
yangsmall = SACE01(Z, M, Y)
round(yangsmall, 3)





## Chapter 26.5.2
## function for principal score
psw = function(Z, M, Y, X) {
  ## probabilities of 10 and 00
  pi.10 = mean(M[Z==1])
  pi.00 = 1 - pi.10
  
  ## conditional probabilities of 10 and 00
  ps.10 = glm(M ~ X, family = binomial, 
              weights = Z)$fitted.values
  ps.00 = 1 - ps.10
  
  ## PCEs 10 and 00 
  tau.10 = mean(Y[Z==1&M==1]) - mean(Y[Z==0]*ps.10[Z==0])/pi.10
  tau.00 = mean(Y[Z==1&M==0]) - mean(Y[Z==0]*ps.00[Z==0])/pi.00
  c(tau.10, tau.00)
}

psw.boot = function(Z, M, Y, X, n.boot = 500){
  ## point estimates
  point.est = psw(Z, M, Y, X)
  ## bootstrap standard errors
  n = length(Z)  
  boot.est = replicate(n.boot, {
    id.boot = sample(1:n, n, replace = TRUE)
    psw(Z[id.boot], M[id.boot], Y[id.boot], X[id.boot, ])
  })
  boot.se    = apply(boot.est, 1, sd)
  ## results
  res        = rbind(point.est, boot.se)
  rownames(res) = c("est", "se")
  colnames(res) = c("tau10", "tau00")
  return(res)
}


## jobs data
jobsdata = read.csv("jobsdata.csv")
getX     = lm(treat ~ sex + age + marital 
              + nonwhite + educ + income,
              data = jobsdata)
X = model.matrix(getX)[, -1]
Z = jobsdata$treat
M = jobsdata$comply
Y = jobsdata$job_seek
table(Z, M)
psw.boot(Z, M, Y, X)





