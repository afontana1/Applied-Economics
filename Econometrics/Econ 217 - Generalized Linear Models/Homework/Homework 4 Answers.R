####################### 
# ECON 217 Homework 4 #
# Sample Code         #   
####################### 

######## Part a ########
# Notes:
# 1. For an ARMA(p,q), you need at least r=max(p,q) numbers of "initialization". Alternatively doing p+q is fine.
# 2. Here I use Y[(i-1):(i-p)] so when you call the function, do (phi1, phi2, phi3 ...).
#     Alternatively, you can Y[(i-p):(i-1)] but reverse the vector of argument. Same for theta. 

####### IMPORTANT NOTES ABOUT GRADING #######
# Keep in mind that phi[i] always goes with y[i-1], and phi[p] always stays with y[i-p]
# Since the homework does not specify it clearly on c(.3, .2, .1), whether .3 is phi[1] or phi[3]. 
# In the grading I regard both way as correct.
# BUT, if next time it specifies which one is phi_1 and which is phi_p,
# PLEASE MAKE SURE that you manage it correct!!

ARMA<-function(n,phi,theta){
  p<-length(phi)
  q<-length(theta)
  r<-max(p,q)
  es<-rnorm(n+r)
  Y<-rep(0,n+r)
  Y[1:r]<-rnorm(r)
  for(i in (r+1):(r+n)){
    Y[i]<-t(phi)%*%Y[(i-1):(i-p)]+t(theta)%*%es[(i-1):(i-q)]+es[i]
  }
  return(Y[-(1:r)])
}

######## Part b & c ########
# Notes:
# 1. Here I show a trick called structured programming: since you're doing some similar job by just varying N,
#   I make a function that generates an ARMA process, plot the graph along with ACF, and returns mean/var.
#   This programming habit helps you saving time and the code is easy to maintain.
# 2. I also use an additional parameter (Ifplot) to control whether to plot the graph or simply calculate mean/var.
#   The default value is given by Ifplot=0, so, when you need to plot, you must specify Ifplot=1.

Plot.an.ARMA<-function(N,Ifplot=0){
  Y=ARMA(N,c(.3, .2, .1),c(.2, .1))
  if (Ifplot==1) {
    plot(Y,type="l",xlab="t",ylab="ARMA(phi=(.3, .2, .1), theta=(.5, .3))")
    acf(Y,lag.max=10,type="correlation")
  }
  c(mean(Y), var(Y))
}

# Here comes the main part. Just call the "Plot.an.ARMA" function!
# Comments: we can find that the mean/var and the ACF function does not change when sample size is sufficiently large.
# (Yes, this is called stationarity)


par(mfcol=c(2,3))
Plot.an.ARMA(100,Ifplot=1)
Plot.an.ARMA(1000,Ifplot=1)
Plot.an.ARMA(10000,Ifplot=1)

######## Part d ########
# Similar code from previous class. This time it is called a "Monte-Carlo" method

B=1000
for(rep in 1:B){
  p<-Plot.an.ARMA(100)
  SampleMean<-as.numeric(p[1])
  SampleVar<-as.numeric(p[2])
  if(rep==1){results<-data.frame(rep,SampleMean,SampleVar)}
  else{results<-rbind(results,data.frame(rep,SampleMean,SampleVar))}
}

quantile(results$SampleMean,prob=c(0.025,0.975),na.rm=TRUE)
quantile(results$SampleVar,prob=c(0.025,0.975),na.rm=TRUE)



################  Problem 2
################  Part A

#Step 1: Get data for google and amazon

library(quantmod)
getSymbols('GOOG',from='2014-04-01',to='2016-01-01')
price_G=GOOG$GOOG.Open

getSymbols('AMZN',from='2014-04-01',to='2016-01-01')
price_A=AMZN$AMZN.Open

####  Plot series in a panel chart
par(mfrow=c(2,1))
plot(price_A)
plot(price_G)

#### VAR with one lag

n=length(price_A)
regG=lm(price_G[2:n]~price_G[1:(n-1)]+price_A[1:(n-1)])
regA=lm(price_A[2:n]~price_G[1:(n-1)]+price_A[1:(n-1)])


summary(regG)
summary(regA)


#	> 	summary(regG)
#
# Call:
# lm(formula = price_G[2:n] ~ price_G[1:(n - 1)] + price_A[1:(n - 1)])
#
# Residuals:
#    Min      1Q  Median      3Q     Max 
#-67.296  -4.957  -0.409   4.412  81.421 
#
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        13.623692   6.078816   2.241  0.02551 *  
# price_G[1:(n - 1)]  0.960612   0.015609  61.543  < 2e-16 ***
# price_A[1:(n - 1)]  0.023833   0.009137   2.608  0.00941 ** 
#---
# Signif. codes:  0 â€?***â€? 0.001 â€?**â€? 0.01 â€?*â€? 0.05 â€?.â€? 0.1 â€? â€? 1
#
# Residual standard error: 10.48 on 439 degrees of freedom
# Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9759 
# F-statistic:  8949 on 2 and 439 DF,  p-value: < 2.2e-16
#
#> 	summary(regA)
#
# Call:
# lm(formula = price_A[2:n] ~ price_G[1:(n - 1)] + price_A[1:(n - 1)])
#
# Residuals:
#    Min      1Q  Median      3Q     Max 
#-52.661  -4.200  -0.300   3.799  86.672 
#
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         3.112405   5.631642   0.553    0.581    
# price_G[1:(n - 1)] -0.009105   0.014461  -0.630    0.529    
# price_A[1:(n - 1)]  1.007254   0.008465 118.993   <2e-16 ***
#---
# Signif. codes:  0 â€?***â€? 0.001 â€?**â€? 0.01 â€?*â€? 0.05 â€?.â€? 0.1 â€? â€? 1
#
# Residual standard error: 9.713 on 439 degrees of freedom
# Multiple R-squared:  0.9929,	Adjusted R-squared:  0.9929 
# F-statistic: 3.08e+04 on 2 and 439 DF,  p-value: < 2.2e-16

# From the results in regG, we see that lags in the price of google and the price of amazon have a positive and significant
# association with the price of google.  In Reg A, lags in the price of amazon have a positive and significant association with 
# the price of amazon, but lags in the price of google do not.
#
# Editorial note:  I wonder if this is because amazon specializing in products that google provides (tablets, OS, big data) along 
# with products that google does not (drones, delivery, etc.).  Just speculation, but I found this interesting.


################  Problem 2
################  Part B

####  Granger Test, with one lag (p=1)

library(MSBVAR)
y2<-ts(data.frame(price_A,price_G))
granger.test(y2,p=1)

#                         F-statistic     p-value
# GOOG.Open -> AMZN.Open   0.3964686 0.529246259
# AMZN.Open -> GOOG.Open   6.8037687 0.009406833
#
# We see that Amazon granger causes google, but google does not granger cause amazon.  
# This is consistent with the earlier reduced form VAR in that the lagged price of google did not have a significant association
# with current price of amazon, but the lag in amazon had a significant association with the price of google.

################  Problem 2
################  Part C

#### Impulse Response Function

#Collect Coefficients from the reduced form VAR

B<-rbind(as.numeric(coef(regG)),as.numeric(coef(regA)))

#Initialize the initial price, which is the last price of the series

pG<-as.numeric(price_G[n])
pA<-as.numeric(price_A[n])
i<-0
p<-data.frame(i,pG,pA)

#Initialize the initial constant/price vector
z0<-matrix(nrow=3,ncol=1)
z0[1,1]<-1
z0[2,1]<-pG
z0[3,1]<-pA

#Initialize shock matrix

shock<-matrix(nrow=2,ncol=1)
shock[1,]<-0
shock[2,]<-60

for(i in 1:10){
  
  #for the first iteration, multiply the coefficient matrix by the price matrix + the shock
  if(i==1){
    z<-B%*%z0+shock
  }
  #for remaining iterations, multiply the coefficient matrix by the updated price matrix, but without the shock.
  if(i>1){
    z<-B%*%z0
  }
  
  ## Save the updated price
  pG<-z[1,1]
  pA<-z[2,1]
  p<-rbind(p,data.frame(i,pG,pA))
  
  ## update the initial price for the next period
  z0[2,]<-pG
  z0[3,]<-pA
}

### Plot the impulse response
plot(pA~i,p,type="l",ylim=c(675,780))
lines(pG~i,p,col='blue')

